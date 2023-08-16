--[[ Ikaruga Snake

It's snake with Ikaruga scoring rules.  Basically a port of what I submitted
for TRICK 2022:
https://github.com/tric/trick2022/tree/master/09-omoikane

Actually the only thing ported over is the concept of mixing snake and Ikaruga.
I wanted to make use of Playdate's graphical display, and that pretty much
required rewriting everything from scratch.  Two bits of detail made it
nontrivial:

- Movements that are smaller than a single grid cell: I didn't want to just
  make the snake abruptly jump from one cell to the next (like I did in the
  text console version).  A fair bit of logic was added to animate the snake
  heads and tail smoothly as it move across cells.

- Smaller grid: we didn't need the sub-cell movements if we just had smaller
  grid cells, but with Playdate's screen size, 32 pixels across comes out
  to be just ~4.7mm, so I decided that the minimum grid cell size was going
  to be 32 by 32 pixels, which meant the grid size is is only 12 by 7.

Given the graphical constraints, the 400x240 screen is laid out as follows:

   (  8,   0)   Dots              36 by 12, 3*1 cells
   ( 52,   0)   Text              340 by 12, 34*1 cells
   (  8,  12)   Game field        384 by 224, 12*7 cells
   ( 44,   0)   Black space       8 by 12
   (  0,   0)   Left border       8 by 236
   (392,   0)   Right border      8 by 236
   (  0, 236)   Bottom border     400 by 4
   (254,   0)   Speed indicator   76 by 12

Grid size limits maximum number of fruits that can be eaten, thus maximum
number of chains is 12*7/3 = 28.  If eating one fruit is 10 points and
maximum chain bonus is capped at 25600, maximum possible score would be:

   100 + 200 + 400 + 800 + 1600 + 3200 + 6400 + 12800 + 25600 +
   25600 * 19 +
   10 * 12*7
   = 538340

This means we only need to reserve so few of the 34 characters:

   1234567890123456789012345678901234
   4321098765432109876543210987654321
   +##       HI ######         ######

So we got 90 pixels free available to between high score and current score,
and we use 76 of that for the speed indicator (16x12 sprite, in 60 possible
positions).

--]]

import "CoreLibs/graphics"
import "CoreLibs/sprites"

import "tiles"

-- Cached imported references.
local gfx <const> = playdate.graphics
local blank_tile <const> = tiles.blank
local snake_tiles <const> = tiles.snake

-- Seed random number generator.
math.randomseed(playdate.getSecondsSinceEpoch())

----------------------------------------------------------------------
--{{{ Game states.

-- Game state.
local GAME_INIT <const> = 0
local GAME_TITLE <const> = 1
local GAME_RUNNING <const> = 2
local GAME_OVER <const> = 3
local game_state = GAME_INIT

-- Game speed.  This is how much snake_frame is incremented at each frame.
-- Higher is faster.
local MAX_SPEED <const> = 8
local MEDIUM_SPEED <const> = 4
local MIN_SPEED <const> = 1
local game_speed = MAX_SPEED

-- Grid cells.
local GRID_W <const> = 12
local GRID_H <const> = 7
local GRID_X <const> = 8
local GRID_Y <const> = 12
local GRID_CELL_SIZE <const> = 32
local GRID_EMPTY <const> = -1
local GRID_WALL <const> = false
local grid = {}

-- Grid cell constants for fruits, using this bitmask:
-- bit 9 = fruit is being animated.
-- bit 8 = fruit is eligible for collisions.
-- bits 0-1 = fruit color.
--
-- Note that fruit cells are all above 0x100 range.  Values in the lower
-- ranges are reserved for snake body (coming from tiles.DIRECTION_* indices).
local GRID_FRUIT_FLAGS <const> = 0x100
local GRID_FRUIT_GROW_FLAGS <const> = 0x300
local GRID_FRUIT_EAT_FLAGS <const> = 0x200
local GRID_LIGHT_FRUIT <const> = GRID_FRUIT_FLAGS | 1
local GRID_DARK_FRUIT <const> =  GRID_FRUIT_FLAGS | 2
local GRID_LIGHT_FRUIT_GROW <const> = GRID_FRUIT_GROW_FLAGS | 1
local GRID_DARK_FRUIT_GROW <const> =  GRID_FRUIT_GROW_FLAGS | 2
local GRID_LIGHT_FRUIT_EAT <const> = GRID_FRUIT_EAT_FLAGS | 1
local GRID_DARK_FRUIT_EAT <const> =  GRID_FRUIT_EAT_FLAGS | 2

-- Ring buffer of input commands, a zero-based array.
--
-- input_write_index is the index of the command that was just recorded,
-- and input_read_index is the index of the command that was just consumed.
-- Thus, the two indices being different indicates that there are button
-- presses available.
--
-- Buffering commands makes it easier to have a tight turning radius.
-- For example, players can press two direction commands and expect two
-- direction changes without having to time their button presses to match
-- the frame rate.
--
-- We buffer up to 16 presses, although nobody really needs 16 unless you
-- are Takahashi Meijin.
local INPUT_BUFFER_MASK <const> = 15
local INPUT_UP <const> = tiles.DIRECTION_UP_UP
local INPUT_RIGHT <const> = tiles.DIRECTION_RIGHT_RIGHT
local INPUT_DOWN <const> = tiles.DIRECTION_DOWN_DOWN
local INPUT_LEFT <const> = tiles.DIRECTION_LEFT_LEFT
local INPUT_CHANGE_COLOR <const> = -1
local input_buffer = {}
local input_write_index = 0
local input_read_index = 0

-- If true, wait for input buffer to clear before completing a state transition.
local clear_buffered_inputs = true

-- Table of opposite directions.  Any attempt to go in the exact opposite
-- direction is always silently ignored.
local OPPOSITE_DIRECTION <const> =
{
	[INPUT_UP] = INPUT_DOWN,
	[INPUT_DOWN] = INPUT_UP,
	[INPUT_LEFT] = INPUT_RIGHT,
	[INPUT_RIGHT] = INPUT_LEFT,
}

-- Table of turns to final directions.
local TURN_DIRECTION <const> =
{
	[tiles.DIRECTION_UP_LEFT] = INPUT_LEFT,
	[tiles.DIRECTION_UP_UP] = INPUT_UP,
	[tiles.DIRECTION_UP_RIGHT] = INPUT_RIGHT,
	[tiles.DIRECTION_RIGHT_UP] = INPUT_UP,
	[tiles.DIRECTION_RIGHT_RIGHT] = INPUT_RIGHT,
	[tiles.DIRECTION_RIGHT_DOWN] = INPUT_DOWN,
	[tiles.DIRECTION_DOWN_RIGHT] = INPUT_RIGHT,
	[tiles.DIRECTION_DOWN_DOWN] = INPUT_DOWN,
	[tiles.DIRECTION_DOWN_LEFT] = INPUT_LEFT,
	[tiles.DIRECTION_LEFT_DOWN] = INPUT_DOWN,
	[tiles.DIRECTION_LEFT_LEFT] = INPUT_LEFT,
	[tiles.DIRECTION_LEFT_UP] = INPUT_UP,
}

-- Threshold for accepting input.  Input is accepted if snake_frame is
-- less than this value, otherwise they are buffered until the next window.
--
-- Motivation for this threshold has to do with how the head of the snake
-- crosses two cells, but visually it's still within the first cell when
-- the animation frame is 0..7 (at 8..15 it crosses into the next cell).
-- We accept user input when the snake is still within the first cell,
-- and apply collision checks when the snake has reached the second cell.
local INPUT_ACCEPT_WINDOW <const> = 8 << 2

-- Ring buffer of snake positions.  We need at least 7*12 cells to cover
-- the entire grid, plus a few to avoid ring buffer overflow.  The next
-- power of 2 greater than that is 128, hence the 127 mask.
--
-- Positions are expressed in terms of tilemap coordinates.  Upper left
-- corner is (1, 1), lower right corner is (GRID_W, GRID_H).
local SNAKE_BUFFER_MASK <const> = 127
assert(SNAKE_BUFFER_MASK > GRID_W * GRID_H)
local snake = {}

-- Current snake length.  Snakes always start out at zero length.
local snake_length = 0

-- Length of the snake that snake needs to grow to.
local snake_target_length = 0

-- Index into snake[] buffer, used if snake_length is greater than zero.
-- snake[snake_head] is the location of the cell just behind the current
-- head (next cell to become snake body), and snake_length cells before
-- that is the tail (next cell to be erased).
--
-- snake[] buffer is accessed with this index plus length scheme as opposed
-- to a two-index scheme (with a second index for snake tail) to avoid
-- pitfalls with zero length snakes.  Also, we actually reference the
-- length of the snake fairly often, so it's good to have that cached.
local snake_head = 0

-- Position of the currently moving snake head.  This is not part of the
-- snake[] buffer, and it's also not counted as part of snake's body length.
-- This really kind of a preview cell where the snake will be.  Once
-- snake_frame overflows, snake[snake_head] is updated with this position,
-- and these values are updated to where snake will be going next according
-- to current direction.
--
-- It's implemented this way rather than have all coordinates inside snake[]
-- buffer because the head requires a fair bit of special handling, and it's
-- a mess trying to index the first two elements of snake[] all the time.
local snake_x = 1
local snake_y = 1

-- Current snake direction and color.
local snake_direction = INPUT_UP
local snake_color = tiles.COLOR_LIGHT

-- Counters for moving head or tail within a cell.
--
-- Snake position is logically handled in grid units, but on screen, snake
-- will move more smoothly at pixel units.  snake_frame encodes the sub-cell
-- status -- it's incremented by game_speed at each frame, then we drop the
-- lowest 2 bits (shifting right) to get the tile frame index.
--
-- If game_speed is greater than 4, the upper 4 bits of snake_frame could
-- occasionally exceed 15, but we only have 16 frames.  To avoid indexing
-- out of bounds, snake_tiles[] is padded to have extra elements by
-- repeating the final animation frame.
local snake_frame = 0

-- Run time stats.
local snake_score = 0
local snake_chain = 0
local snake_max_chain = 0
local snake_current_chain_color = 0
local snake_current_chain_length = 0

-- High score data.
local save_state = playdate.datastore.read()
if ((not save_state) or
    (not save_state.hi_score) or
    save_state.hi_score < 0 or
    save_state.hi_score > 999999 or
    save_state.hi_score % 10 ~= 0) then
	save_state = {hi_score = 0}
end

-- List of grid positions.  For random placements of fruits, we take the
-- next unused coordinate out of this list, and shuffle the list when we
-- have used the last item.  Doing it this way means if there is one
-- available grid spot, we will find it in deterministic time.
--
-- A different way of doing this would be to start at a random grid
-- position, then linearly search for the next available spot.  But doing
-- it that way skews the distribution of coordinates, making some positions
-- more likely than others depending on which direction we search.
--
-- What we don't want is generate random coordinates repeatedly and hope
-- we eventually find an empty spot, because there is no guarantee that
-- we will.
local random_positions = table.create(GRID_W * GRID_H)
for y = 1, GRID_H do
	for x = 1, GRID_W do
		random_positions[(y - 1) * GRID_W + (x - 1) + 1] = {x, y}
	end
end

-- Index of the last used random position.  Set to a high value to force
-- shuffle on first use.
local random_positions_index = GRID_W * GRID_H + 2

-- Fruit spawn status.
--
-- Color of each newly generated fruit is based on fruit_serial, such that
-- we repeat a cycle of 3 light fruits followed by 3 dark fruits.  If
-- fruit_endgame is false, fruit_serial is incremented after each fruit,
-- otherwise it remains constant (i.e. fruit color does not change in
-- endgame mode).  This means if a player has maintained perfect chains
-- before reaching endgame mode, they will continue to make perfect chains
-- all the way to the end.
local fruit_serial = 0
local fruit_endgame = false

-- Timers for animating transitional fruits.
local START_FRUIT_TIMER <const> = 4
local fruit_grow_timer = 0
local fruit_eat_timer = 0
local fruit_transition_timer = 0

-- List of cell neighbors to check when placing fruits.
local FRUIT_NEIGHBORS <const> =
{
	{-1, -1}, {0, -1}, {1, -1},
	{-1,  0},          {1,  0},
	{-1,  1}, {0,  1}, {1,  1},
}

-- Number of remaining frames for flashing the chain dot display.
local dot_timer = 0

-- Table of bonus scores for each completed chain.
local bonus_table = {100}
for i = 2, 9 do
	bonus_table[i] = 2 * bonus_table[i - 1]
end
for i = 10, (GRID_W * GRID_H // 3 + 1) do
	bonus_table[i] = bonus_table[i - 1]
end

--}}}

----------------------------------------------------------------------
--{{{ Graphic states.

-- Z orders.
local Z_BACKGROUND <const> = 1
local Z_GRID <const> = 2
local Z_TEXT <const> = 3
local Z_SNAKE_HEAD <const> = 4
local Z_TITLE <const> = 5

-- Background image.  A random one for each run, but constant for the
-- lifetime of the game.
--
-- I wanted some grayish pattern that is not a solid color, and since
-- Playdate SDK came with Perlin noise, this seems like a good place
-- to use it.
--
-- Simulator says it costs ~110ms to generate this image.
local ui_background_image = gfx.image.new(384, 224, gfx.kColorWhite)
do
	gfx.lockFocus(ui_background_image)
	gfx.setColor(gfx.kColorBlack)

	-- Accumulated errors for Floyd-Steinberg.
	--
	-- Floyd-Steinberg actually only needs (-1, +1) beyond the row size, but
	-- we do (-3, +3) here to avoid some edge artifacts.
	--
	-- We could avoid all this extra complexity and just use something like
	-- Bayer 8x8 ordered dithering, but Floyd-Steinberg looks much better.
	local fs_error = {}
	fs_error[0] = {}
	fs_error[1] = {}
	for x = -3, 386 do
		fs_error[0][x] = 0
		fs_error[1][x] = 0
	end

	-- Set random Z value.  Playdate's Perlin function appears to always
	-- generate the same pattern, so we add variety by choosing a different
	-- Z value on each run.
	--
	-- We can get a bit more variety if we zoom into a subset of it in the
	-- XY space, but that would require increasing the octave count to get
	-- more details, which in turn causes the generation time to increase.
	-- Varying just the Z value seems to be the best balance.
	local z = math.random()

	-- Generate scanlines.  We only need 224 rows worth, but we generate
	-- 2 extra rows above to reduce artifacts due to dithering.
	--
	-- The artifacts are just how Floyd-Steinberg works, which propagates
	-- some errors to subsequent rows.  Without at least one extra row,
	-- you will see some maze-like effects near the top edge.  One extra
	-- row is mostly sufficient, two rows reduces the effect even further.
	for y = 0, 225 do
		-- Generate one row of Perlin noise.  We only need 384 pixels
		-- worth, but we generate 3 extra pixels on each side to avoid
		-- edge artifacts due to dithering (the artifacts would have looked
		-- like vertical columns along the edges).
		local p = gfx.perlinArray(
			390,         -- count
			0, 1 / 390,  -- x, dx
			y / 225, 0,  -- y, dy
			z, 0,        -- z, dz
			0,           -- repeat
			6,           -- octaves
			0.7)         -- persistence
		local r0 = y & 1
		local r1 = r0 ~ 1
		for x = -2, 385 do
			-- Scale Perlin intensity.  Empirically, I found that Playdate's
			-- Perlin range to be mostly in the 0.34 to 0.55 range, so it's not
			-- very evenly distributed, and definitely doesn't use the full [0, 1)
			-- range.  We want to scale this to be somewhere in the [0, 0.3)
			-- range.
			local a = (p[x + 3] - 0.34) / 0.21 * 0.29
			if a < 0 then
				a = 0
			elseif a > 0.3 then
				a = 0.3
			end

			-- Add residual error and draw pixel.
			a += fs_error[r0][x]
			if a > 0.5 then
				if y > 1 and x >= 0 and x < 384 then
					gfx.drawPixel(x, y - 2)
				end
				a -= 0.5
			end

			-- Propagate error.
			fs_error[r0][x + 1] += a * 7 / 16
			fs_error[r1][x - 1] += a * 3 / 16
			fs_error[r1][x    ] += a * 5 / 16
			fs_error[r1][x + 1] += a / 16
		end

		-- Reset errors for next row.
		for x = -3, 386 do
			fs_error[r0][x] = 0
		end
	end
	gfx.unlockFocus()
end
local ui_background_sprite = gfx.sprite.new(ui_background_image)
ui_background_sprite:setZIndex(Z_BACKGROUND)
ui_background_sprite:setOpaque(true)
ui_background_sprite:addSprite()
ui_background_sprite:setCenter(0, 0)
ui_background_sprite:moveTo(GRID_X, GRID_Y)

-- Grid images, including fruits and snake body.
local ui_tile_images = gfx.imagetable.new("tiles")
assert(ui_tile_images)
local ui_grid = gfx.tilemap.new()
ui_grid:setImageTable(ui_tile_images)
ui_grid:setSize(GRID_W, GRID_H)
local ui_grid_sprite = gfx.sprite.new()
ui_grid_sprite:setTilemap(ui_grid)
ui_grid_sprite:setZIndex(Z_GRID)
ui_grid_sprite:addSprite()
ui_grid_sprite:setCenter(0, 0)
ui_grid_sprite:moveTo(GRID_X, GRID_Y)

-- A true blank tile, for places where we need to replace a grid cell with
-- something transparent.  Basically an invalid index to tell Playdate not
-- to draw anything at that cell.  It's also the value we get back when
-- we call getTiles() before populating the tilemap.
--
-- We can't use blank tiles from image tables due to an opacity bug, see:
-- https://devforum.play.date/t/using-transparent-images-as-tiles-on-tilemap-lead-to-smearing-effect/9851/2
--
-- By the way, we didn't just replace all the blank tile indices in tiles.lua
-- with this invalid index, because there are a few instances where blank_tile
-- is used with getImage(), and there we need a valid index.
local BLANK_TILE <const> = 0x10000

-- Dots, used for drawing chain status.
local ui_dots_images = gfx.imagetable.new("dots")
assert(ui_dots_images)
local DOT_LIGHT <const> = 1
local DOT_LIGHT_BLINK <const> = 2
local DOT_DARK <const> = 3
local DOT_DARK_BLINK <const> = 4
local DOT_BLANK <const> = 5
local ui_dots = gfx.tilemap.new()
ui_dots:setImageTable(ui_dots_images)
ui_dots:setSize(3, 1)
local ui_dots_sprite = gfx.sprite.new()
ui_dots_sprite:setTilemap(ui_dots)
ui_dots_sprite:setZIndex(Z_TEXT)
ui_dots_sprite:addSprite()
ui_dots_sprite:setCenter(0, 0)
ui_dots_sprite:moveTo(8, 0)

-- Status text.
local ui_numbers_images = gfx.imagetable.new("numbers")
assert(ui_numbers_images)
local NUMBER_PLUS <const> = 11
local NUMBER_H <const> = 12
local NUMBER_I <const> = 13
local NUMBER_BLANK <const> = 14
local ui_text = gfx.tilemap.new()
ui_text:setImageTable(ui_numbers_images)
ui_text:setSize(34, 1)
local ui_text_sprite = gfx.sprite.new()
ui_text_sprite:setTilemap(ui_text)
ui_text_sprite:setZIndex(Z_TEXT)
ui_text_sprite:addSprite()
ui_text_sprite:setCenter(0, 0)
ui_text_sprite:moveTo(52, 0)

-- Sprites for just the head of the snake.  This is not part of the grid
-- because occasionally it may go out of bounds or overlap with grid
-- elements.
local ui_snake_head_sprite = gfx.sprite.new()
ui_snake_head_sprite:setImage(ui_tile_images:getImage(blank_tile))
ui_snake_head_sprite:setZIndex(Z_SNAKE_HEAD)
ui_snake_head_sprite:addSprite()
ui_snake_head_sprite:setCenter(0, 0)

-- Speed indicator, only visible when crank is undocked.
local ui_speed_images = gfx.imagetable.new("speed")
assert(ui_speed_images)
local SPEED_SNAIL <const> = 1
local SPEED_TURTLE <const> = 2
local SPEED_RABBIT <const> = 3
local ui_speed_sprite = gfx.sprite.new()
ui_speed_sprite:setImage(ui_speed_images:getImage(SPEED_RABBIT))
ui_speed_sprite:setZIndex(Z_TEXT)
ui_speed_sprite:addSprite()
ui_speed_sprite:setCenter(0, 0)
ui_speed_sprite:setVisible(false)

-- Title and ending.
local ui_title_image = gfx.image.new("title")
assert(ui_title_image)
local TITLE_W <const>, TITLE_H <const> = ui_title_image:getSize()
local TITLE_X <const> = 200 - TITLE_W // 2
local TITLE_Y <const> = 124 - TITLE_H // 2
local ui_title_sprite = gfx.sprite.new(ui_title_image)
ui_title_sprite:setZIndex(Z_TITLE)
ui_title_sprite:addSprite()
ui_title_sprite:setCenter(0, 0)
ui_title_sprite:moveTo(TITLE_X, TITLE_Y)

local ui_game_over_image = gfx.image.new("game_over")
assert(ui_game_over_image)
local GAME_OVER_W <const>, GAME_OVER_H <const> = ui_game_over_image:getSize()
local GAME_OVER_X <const> = 200 - GAME_OVER_W // 2
local GAME_OVER_Y <const> = 90 - GAME_OVER_H // 2
local ui_game_over_sprite = gfx.sprite.new(ui_game_over_image)
ui_game_over_sprite:setZIndex(Z_TITLE)
ui_game_over_sprite:addSprite()
ui_game_over_sprite:setCenter(0, 0)
ui_game_over_sprite:moveTo(GAME_OVER_X, GAME_OVER_Y)
ui_game_over_sprite:setVisible(false)

-- There is no need to draw border as sprites since they are covered
-- by background color.
gfx.setBackgroundColor(gfx.kColorBlack)
gfx.clear()

-- Menu image.  We show either help text or current stats when game is paused.
--
-- We don't really need this, but because we have a dithered background image
-- and the pause screen will draw another dither pattern on top of that, the
-- combined effect looks kind of weird.  Since we are going to replace that
-- part of the screen, might as well show something useful.
local ui_menu_image = gfx.image.new(400, 240, gfx.kColorWhite)

--}}}

----------------------------------------------------------------------
--{{{ Helper functions.

-- Initialize game state.
local function reset()
	gfx.clear()

	-- Reset input buffer.
	input_buffer = {}
	input_write_index = 0
	input_read_index = 0

	-- Reset snake states.
	--
	-- Initial position and direction will be set by game_init() and
	-- game_title().
	snake = {}
	snake_head = 0
	snake_length = 0
	snake_target_length = 3
	snake_color = tiles.COLOR_LIGHT

	snake_score = 0
	snake_chain = 0
	snake_max_chain = 0
	snake_current_chain_color = 0
	snake_current_chain_length = 0

	snake_frame = 0

	-- Start snake head in invisible state.
	ui_snake_head_sprite:setImage(ui_tile_images:getImage(blank_tile))

	-- Reset grid. Populate the borders such that index out of bounds
	-- results in false or nil.
	grid[0] = {}
	grid[GRID_H + 1] = {}
	for y = 1, GRID_H do
		grid[y] = {}
		for x = 1, GRID_W do
			grid[y][x] = GRID_EMPTY
			ui_grid:setTileAtPosition(x, y, BLANK_TILE)
		end
		grid[y][0] = GRID_WALL
		grid[y][GRID_W + 1] = GRID_WALL
	end

	-- Reset fruit spawning status.
	fruit_serial = 0
	fruit_endgame = false
	fruit_grow_timer = 0
	fruit_eat_timer = 0
	fruit_transition_timer = 0

	-- Reset status bar.
	for x = 1, 3 do
		ui_dots:setTileAtPosition(x, 1, DOT_BLANK)
	end
	for x = 1, 34 do
		ui_text:setTileAtPosition(x, 1, NUMBER_BLANK)
	end
	dot_timer = 0
end

-- Adjust speed when crank is undocked.
--
-- Note that game always run at maximum speed when crank is docked.
local function adjust_speed()
	if playdate.isCrankDocked() then
		ui_speed_sprite:setVisible(false)
		return
	end

	-- Convert crank angle (range [0, 359]) to sprite position (range [0, 60]).
	local crank = playdate.getCrankPosition()
	if crank > 180 then
		crank = (crank - 180) // 3
	else
		crank = (180 - crank) // 3
	end

	if crank < 40 then
		if crank < 20 then
			ui_speed_sprite:setImage(ui_speed_images:getImage(SPEED_SNAIL))
			game_speed = MIN_SPEED
		else
			ui_speed_sprite:setImage(ui_speed_images:getImage(SPEED_TURTLE))
			game_speed = MEDIUM_SPEED
		end
	else
		ui_speed_sprite:setImage(ui_speed_images:getImage(SPEED_RABBIT))
		game_speed = MAX_SPEED
	end

	ui_speed_sprite:moveTo(254 + crank, 0)
	ui_speed_sprite:setVisible(true)
end

-- Draw a right-aligned number in the text bar.
local function draw_number(end_position, n)
	assert(n >= 0)
	assert(n <= 999999)
	if n <= 0 then
		ui_text:setTileAtPosition(end_position, 1, 1)
		return
	end
	while n > 0 do
		ui_text:setTileAtPosition(end_position, 1, n % 10 + 1)
		end_position -= 1
		n //= 10
	end
end

-- Draw high score if it's nonzero.
local function draw_high_score()
	if save_state.hi_score > 0 then
		ui_text:setTileAtPosition(11, 1, NUMBER_H)
		ui_text:setTileAtPosition(12, 1, NUMBER_I)
		draw_number(19, save_state.hi_score)
	else
		ui_text:setTileAtPosition(11, 1, NUMBER_BLANK)
		ui_text:setTileAtPosition(12, 1, NUMBER_BLANK)
		for x = 14, 19 do
			ui_text:setTileAtPosition(x, 1, NUMBER_BLANK)
		end
	end
end

-- Draw flash dots for max chain display.
local function draw_max_chain()
	if dot_timer > 0 then
		assert(snake_current_chain_length == 3)

		dot_timer -= 1
		if dot_timer > 0 then
			local tile_index =
			{
				snake_current_chain_color << 1,
				(snake_current_chain_color << 1) - 1
			}
			for x = 1, 3 do
				ui_dots:setTileAtPosition(x, 1, tile_index[((dot_timer + x) & 1) + 1])
			end
		else
			for x = 1, 3 do
				ui_dots:setTileAtPosition(x, 1, DOT_BLANK)
			end
		end
	end
end

-- Update chain status after a score change.
local function update_chain_status()
	local tile_index = (snake_current_chain_color << 1) - 1
	for x = 1, 3 do
		if x > snake_current_chain_length then
			ui_dots:setTileAtPosition(x, 1, DOT_BLANK)
		else
			ui_dots:setTileAtPosition(x, 1, tile_index)
		end
	end
end

-- Draw snake head and tail.
local function draw_snake()
	local frame = (snake_frame >> 2) + 1

	-- Draw snake head.
	ui_snake_head_sprite:moveTo((snake_x - 1) * GRID_CELL_SIZE + GRID_X, (snake_y - 1) * GRID_CELL_SIZE + GRID_Y)
	ui_snake_head_sprite:setImage(ui_tile_images:getImage(snake_tiles[snake_color][snake_direction][tiles.TYPE_HEAD_FRONT][frame]))
	if snake_length == 0 then
		return
	end

	local x = snake[snake_head][1]
	local y = snake[snake_head][2]
	ui_grid:setTileAtPosition(x, y, snake_tiles[snake_color][snake_direction][tiles.TYPE_HEAD_BACK][frame])
	if snake_length == 1 then
		return
	end

	-- Don't draw snake tail if snake is currently growing.
	--
	-- Note: because snake grows immediately upon entering a cell, but eats
	-- fruits somewhere in the middle of a cell, we would observe an effect
	-- where immediately after eating a fruit, the tail would pause for half
	-- the time it takes to traverse a cell, and then suddenly spring outward
	-- a bit.  It's a bit jarring but it's kind of just a feature at this
	-- point.
	--
	-- In order to fix that, one thought was to pause tail growth until the
	-- snake_frame counter has reached the frame when the fruit was eaten,
	-- but this leads to a truncated tail artifact if the player accelerates
	-- immediately after eating a fruit.  Current springy tail looks weird,
	-- but less weird than the truncated tails.
	--
	-- Another thought was to have the fruit be eaten only when the snake
	-- completely enters the cell, i.e. when snake_frame is near zero.  But
	-- then the fruit eating animation would be covered up.
	--
	-- The most complex fix for this might be to keep track of head and tail
	-- pointers separately and have a separate sub-frame counter for each.
	-- That was the initial model I had in mind, but then I ran into
	-- complications with zero length snakes, and decided that head+length
	-- is the right way to go, and I am not inclined to go back to tracking
	-- head and tails separately again.
	if snake_length < snake_target_length then
		return
	end

	-- Draw snake tail.
	local p = (snake_head - snake_length + 1) & SNAKE_BUFFER_MASK
	x = snake[p][1]
	y = snake[p][2]
	local t = grid[y][x]
	ui_grid:setTileAtPosition(x, y, snake_tiles[snake_color][t][tiles.TYPE_TAIL_FRONT][frame])

	p = (snake_head - snake_length) & SNAKE_BUFFER_MASK
	if snake[p] then
		x = snake[p][1]
		y = snake[p][2]
		ui_grid:setTileAtPosition(x, y, snake_tiles[snake_color][t][tiles.TYPE_TAIL_BACK][frame])
	end
end

-- Redraw the whole snake.  This is used in response to snake color change.
local function redraw_snake()
	-- Redraw the body bits.
	local frame = (snake_frame >> 2) + 1
	for i = 1, snake_length - 1 do
		local p = (snake_head - i) & SNAKE_BUFFER_MASK
		if snake[p] then
			local x = snake[p][1]
			local y = snake[p][2]
			ui_grid:setTileAtPosition(x, y, snake_tiles[snake_color][grid[y][x]][tiles.TYPE_BODY][frame])
		end
	end

	-- Draw head and tail tiles over the body.
	draw_snake()
end

-- Update fruit cells.
local function draw_fruits()
	for y = 1, GRID_H do
		for x = 1, GRID_W do
			assert(grid[y][x])
			if grid[y][x] >= GRID_FRUIT_FLAGS then
				local c = grid[y][x] & 3
				if (grid[y][x] & 0xf00) == GRID_FRUIT_EAT_FLAGS then
					-- Animate fruit that is being eaten.
					--
					-- There is at most one.  If player eats a second fruit while
					-- the first fruit has not been fully consumed yet, the first
					-- fruit will be removed so that we don't animate two fruits
					-- with the same counter.  This is normally not something
					-- that happens because it takes 8 frames to traverse a cell
					-- at maximum speed, and only 4 frames to eat a fruit.
					--
					-- Also, eating a fruit normally takes 4 frames, but only the
					-- first 3 will be drawn when snake is moving at maximum speed.
					-- This is because by the 4th frame, grid[y][x] would have been
					-- overwritten by a snake tile.
					--
					-- So then, one thought would be to draw the fruit that is being
					-- eaten in a separate sprite or tilemap, but Playdate's
					-- graphics library appears to not redraw the background
					-- properly in that case due to tilemap opacity bug.
					--
					-- I didn't think the extra one frame of fruit was worth the
					-- trouble, so I am just going to live with 3 frames.
					if fruit_eat_timer <= 0 then
						-- Remove fruit that has been completely eaten.
						ui_grid:setTileAtPosition(x, y, BLANK_TILE)
						grid[y][x] = GRID_EMPTY
					else
						-- Animate fruit.
						assert(fruit_eat_timer <= #tiles.eat_fruits[c])
						ui_grid:setTileAtPosition(x, y, tiles.eat_fruits[c][5 - fruit_eat_timer])
					end
				else
					-- Found a fruit that has not been eaten yet.  If we are in
					-- the middle of a color transition, that will take precedence
					-- over any growth animations.
					if fruit_transition_timer > 0 then
						if c == snake_color then
							-- Hostile to friendly.
							ui_grid:setTileAtPosition(x, y, tiles.change_fruits[c][fruit_transition_timer])
						else
							-- Friendly to hostile.
							ui_grid:setTileAtPosition(x, y, tiles.change_fruits[c][5 - fruit_transition_timer])
						end

						-- Clear growth bit.
						grid[y][x] &= 0x1ff

					elseif (grid[y][x] & 0xf00) == GRID_FRUIT_GROW_FLAGS then
						if fruit_grow_timer > 0 then
							if c == snake_color then
								-- Grow friendly.
								ui_grid:setTileAtPosition(x, y, tiles.grow_friendly_fruits[c][5 - fruit_grow_timer])
							else
								-- Grow hostile.
								ui_grid:setTileAtPosition(x, y, tiles.grow_hostile_fruits[c][5 - fruit_grow_timer])
							end
						else
							-- Clear growth bit.
							grid[y][x] &= 0x1ff
						end
					end
				end
			end
		end
	end

	-- Update timers.
	if fruit_eat_timer > 0 then
		fruit_eat_timer -= 1
	end
	if fruit_transition_timer > 0 then
		fruit_transition_timer -= 1
		fruit_grow_timer = 0
	end
	if fruit_grow_timer > 0 then
		fruit_grow_timer -= 1
	end
end

-- Update snake position and direction for next time step.
local function update_snake_cursor()
	if (snake_direction == tiles.DIRECTION_RIGHT_RIGHT or
	    snake_direction == tiles.DIRECTION_UP_RIGHT or
	    snake_direction == tiles.DIRECTION_DOWN_RIGHT) then
		snake_direction = INPUT_RIGHT
		snake_x += 1
	elseif (snake_direction == tiles.DIRECTION_LEFT_LEFT or
	        snake_direction == tiles.DIRECTION_UP_LEFT or
	        snake_direction == tiles.DIRECTION_DOWN_LEFT) then
		snake_direction = INPUT_LEFT
		snake_x -= 1
	elseif (snake_direction == tiles.DIRECTION_UP_UP or
	        snake_direction == tiles.DIRECTION_RIGHT_UP or
	        snake_direction == tiles.DIRECTION_LEFT_UP) then
		snake_direction = INPUT_UP
		snake_y -= 1
	else -- Down
		snake_direction = INPUT_DOWN
		snake_y += 1
	end
end

-- Update snake positions.
local function update_snake()
	snake_frame += game_speed
	if snake_frame < 64 then
		return
	end
	snake_frame -= 64

	-- Grow the initial segment for a previously empty snake.
	if snake_length == 0 then
		ui_grid:setTileAtPosition(snake_x, snake_y, snake_tiles[snake_color][snake_direction][tiles.TYPE_HEAD_BACK][1])
		snake[snake_head] = {snake_x, snake_y}
		grid[snake_y][snake_x] = snake_direction
		snake_length = 1
		update_snake_cursor()
		return
	end

	-- Finalize the one cell behind current snake head.  It was TYPE_HEAD_BACK,
	-- now it's TYPE_BODY.
	local px = snake[snake_head][1]
	local py = snake[snake_head][2]
	ui_grid:setTileAtPosition(px, py, snake_tiles[snake_color][snake_direction][tiles.TYPE_BODY][1])
	grid[py][px] = snake_direction

	-- Finalize the cell for where snake head is currently at.
	snake_head = (snake_head + 1) & SNAKE_BUFFER_MASK
	ui_grid:setTileAtPosition(snake_x, snake_y, snake_tiles[snake_color][snake_direction][tiles.TYPE_HEAD_BACK][1])
	snake[snake_head] = {snake_x, snake_y}
	grid[snake_y][snake_x] = snake_direction

	-- Stop the snake from turning, and set next cell location.
	update_snake_cursor()

	-- If snake is currently growing, update the length for the one body
	-- cell that we just added.
	if snake_length < snake_target_length then
		snake_length += 1
		return
	end

	-- Snake is not currently growing, need to erase tail to maintain length.
	local p = (snake_head - snake_length - 1) & SNAKE_BUFFER_MASK
	if snake[p] then
		px = snake[p][1]
		py = snake[p][2]
		ui_grid:setTileAtPosition(px, py, BLANK_TILE)
		grid[py][px] = GRID_EMPTY
	end
end

-- Shuffle grid positions.
local function shuffle_positions()
	-- https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
	for i = #random_positions, 2, -1 do
		local j = math.random(1, i)
		if i ~= j then
			random_positions[i], random_positions[j] = random_positions[j], random_positions[i]
		end
	end

	random_positions_index = 0
end

-- Return a random position.
local function get_random_position()
	if random_positions_index >= #random_positions then
		shuffle_positions()
	end
	random_positions_index += 1
	return random_positions[random_positions_index][1], random_positions[random_positions_index][2]
end

-- Generate a random fruit location.
--
-- This assumes that there is at least one empty spot available.
local function generate_fruit_location(next_fruit_color)
	-- Apply constraints in 3 passes:
	-- 1. Avoid different color fruits and snake.
	-- 2. Avoid snake.
	-- 3. Take any available position.
	for pass = 1, 3 do
		for i = 1, #random_positions do
			local x, y = get_random_position()

			-- Skip spots that are already occupied.
			if grid[y][x] ~= GRID_EMPTY then
				goto next_position
			end

			-- Look for obstacles near this tentative placement spot.
			local near_different_fruits = false
			local near_snake = false
			if pass < 3 then
				for _, n in ipairs(FRUIT_NEIGHBORS) do
					local row = grid[y + n[2]]
					local cell = row and row[x + n[1]]
					if cell and cell ~= GRID_EMPTY then
						-- Found a nonempty cell.
						if cell < GRID_FRUIT_FLAGS then
							near_snake = true
						else
							if (cell & GRID_FRUIT_FLAGS) ~= 0 and (cell & 3) ~= next_fruit_color then
								near_different_fruits = true
							end
						end
					end

					-- Since head of the snake is not part of the grid, we need
					-- to manually check the head coordinates here.
					if x + n[1] == snake_x and y + n[2] == snake_y then
						near_snake = true
					end
				end
			end

			-- Avoid placing fruits of different colors near each other, since
			-- they may require quick color transitions to get.
			if pass == 1 and near_different_fruits then
				goto next_position
			end

			-- Avoid placing fruits near the snake.  We especially want to
			-- avoid popping up new fruits right in front of the player,
			-- since it may require a quick color change to react.
			if pass <= 2 and near_snake then
				goto next_position
			end

			-- Found a good position that passes the current set of filters.
			-- If we have to skip some filters to get this far, the grid is
			-- probably overly crowded, and we need to enable endgame mode.
			if pass == 3 then
				fruit_endgame = true
			end

			-- Return the position we found.
			do
				return x, y
			end

			::next_position::
		end
	end

	-- If we got here, it means generate_fruit_location() was called when
	-- the grid is already completely full, and there are no spots available.
	return nil, nil
end

-- Spawn a single fruit while game is running.
local function spawn_fruit()
	local next_fruit_color = (fruit_serial % 6) // 3 + 1
	local x, y = generate_fruit_location(next_fruit_color)
	if not x then
		return
	end
	if not fruit_endgame then
		fruit_serial += 1
	end

	-- Add fruit to grid.
	next_fruit_color |= GRID_FRUIT_GROW_FLAGS
	grid[y][x] = next_fruit_color
	fruit_grow_timer = START_FRUIT_TIMER
end

-- Setup for GAME_RUNNING state.
local function enter_running_state()
	-- Make snake start at center left, going right.
	reset()
	snake_x = 1
	snake_y = 4
	snake_direction = INPUT_RIGHT
	game_state = GAME_RUNNING
	clear_buffered_inputs = true

	-- Spawn initial set of fruits.  This is similar to spawn_fruit(), except
	-- we avoid the middle 3 rows during startup.
	--
	-- Without the middle 3 rows, we have 4*12 = 48 spots available.  Even
	-- if every fruit uses a 2x2 cell so that it's not adjacent to any other
	-- fruit, we should be able to guarantee placement for at least 12 fruits.
	-- Thus, this loop repeats until we got those 12.
	while fruit_serial <= 12 do
		local next_fruit_color = (fruit_serial % 6) // 3 + 1
		local x, y = generate_fruit_location(next_fruit_color)
		if y and (y < 3 or y > 5) then
			grid[y][x] = GRID_FRUIT_GROW_FLAGS | next_fruit_color
			fruit_serial += 1
		end
	end

	-- Generate one last fruit in the middle row, on the way to be consumed
	-- by snake.
	local last_fruit_color = (fruit_serial % 6) // 3 + 1
	fruit_serial += 1
	grid[4][GRID_W - 1] = GRID_FRUIT_GROW_FLAGS | last_fruit_color

	-- Set timer to make fruits grow.
	fruit_grow_timer = START_FRUIT_TIMER

	-- Draw initial score.
	draw_number(34, snake_score)
end

-- Setup for GAME_OVER state.
--
-- Only reachable upon colliding with something.
local function enter_game_over()
	-- Redraw snake with dead expression.
	redraw_snake()
	local d = (TURN_DIRECTION[snake_direction] - 1) // 3 + 1
	ui_snake_head_sprite:setImage(ui_tile_images:getImage(tiles.dead[snake_color][d]))
	gfx.sprite.update()
	coroutine.yield()

	-- Save high score if we made an improvement.
	if snake_score > save_state.hi_score then
		save_state.hi_score = snake_score
		playdate.datastore.write(save_state)
	end

	-- Shake the screen for a few frames.
	--
	-- Note that we use setDrawOffset for these, as opposed to
	-- playdate.display.setOffset.  This is because when the snake head
	-- collides with the edge of the grid, that sprite will be outside of
	-- the screen area.  If we use setOffset to do the shake, that sprite
	-- will be truncated.  By using setDrawOffset and redraw, we get to see
	-- more of the head during the shake.
	for i = 1, 8 do
		gfx.setDrawOffset(math.random(-4, 4), math.random(-4, 4))
		gfx.sprite.update()
		coroutine.yield()
	end
	for i = 1, 4 do
		gfx.setDrawOffset(math.random(-2, 2), math.random(-2, 2))
		gfx.sprite.update()
		coroutine.yield()
	end
	gfx.setDrawOffset(0, 0)

	-- Fade in game over text.
	for i = 1, 32 do
		adjust_speed()
		gfx.sprite.update()
		ui_game_over_image:drawFaded(GAME_OVER_X, GAME_OVER_Y, i / 32, gfx.image.kDitherTypeBayer8x8)
		coroutine.yield()
	end
	ui_game_over_sprite:setVisible(true)

	-- Make high score visible after game over title has faded in.
	--
	-- If this were a regular Ikaruga game, this would be the place where
	-- we show S/A/B/C ranking, but for such a small field I didn't think
	-- it's worth it.  Anyone interested in those rankings can go play
	-- the ruby version mentioned at the beginning of this file.
	draw_high_score()

	-- Update game state to wait for a new button press.
	game_state = GAME_OVER
	clear_buffered_inputs = true
end

-- Fetch the next buffered input command, or nil if there is none.
local function get_next_input()
	if input_read_index == input_write_index then
		return nil
	end
	local command = input_buffer[input_write_index]
	input_read_index = (input_read_index + 1) & INPUT_BUFFER_MASK
	return command
end

-- Push a command back to input buffer such that it will be the next
-- command that's read.  As long as we call both get_next_input() and
-- unget_next_input() within the same playdate.update() cycle, there is
-- no threading issue to worry about according to:
-- https://devforum.play.date/t/when-exactly-button-callbacks-and-input-handlers-are-called/11486/4
local function unget_next_input(command)
	input_read_index = (input_read_index - 1) & INPUT_BUFFER_MASK
	input_buffer[input_read_index] = command
end

-- Handle direction command.  Returns true if command is consumed, or false
-- if command is ignored and needs to be pushed back to input buffer.
local function handle_direction_input(command)
	-- This function is only reached when we are currently going straight,
	-- because we would have blocked user input during a turn.
	assert(snake_direction == INPUT_UP or snake_direction == INPUT_DOWN or snake_direction == INPUT_LEFT or snake_direction == INPUT_RIGHT)

	-- If input is the same as the direction we were going, it's interpreted
	-- as an intent to accelerate movement to the next cell.
	if command == snake_direction then
		if snake_frame + game_speed < 64 then
			snake_frame = 64 - game_speed
		end
		return true
	end

	-- If input is the exact opposite of the direction we were going, it's
	-- always silently ignored.
	if command == OPPOSITE_DIRECTION[snake_direction] then
		return true
	end

	-- Remaining inputs are intent to turn.  We will make the turn now if we
	-- are not more than halfway across the cell.
	if snake_frame < INPUT_ACCEPT_WINDOW then
		-- Draw pending updates before executing the turn.
		gfx.sprite.update()
		coroutine.yield()

		-- Apply the turn command.
		if snake_direction == INPUT_RIGHT then
			if command == INPUT_UP then
				snake_x -= 1
				snake_y -= 1
				snake_direction = tiles.DIRECTION_RIGHT_UP
			else
				assert(command == INPUT_DOWN)
				snake_x -= 1
				snake_y += 1
				snake_direction = tiles.DIRECTION_RIGHT_DOWN
			end
		elseif snake_direction == INPUT_LEFT then
			if command == INPUT_UP then
				snake_x += 1
				snake_y -= 1
				snake_direction = tiles.DIRECTION_LEFT_UP
			else
				assert(command == INPUT_DOWN)
				snake_x += 1
				snake_y += 1
				snake_direction = tiles.DIRECTION_LEFT_DOWN
			end
		elseif snake_direction == INPUT_UP then
			if command == INPUT_RIGHT then
				snake_x += 1
				snake_y += 1
				snake_direction = tiles.DIRECTION_UP_RIGHT
			else
				assert(command == INPUT_LEFT)
				snake_x -= 1
				snake_y += 1
				snake_direction = tiles.DIRECTION_UP_LEFT
			end
		else -- Down
			assert(snake_direction == INPUT_DOWN)
			if command == INPUT_RIGHT then
				snake_x += 1
				snake_y -= 1
				snake_direction = tiles.DIRECTION_DOWN_RIGHT
			else
				assert(command == INPUT_LEFT)
				snake_x -= 1
				snake_y -= 1
				snake_direction = tiles.DIRECTION_DOWN_LEFT
			end
		end

		-- Draw snake to show us halfway through a turn.
		snake_frame = 32 - game_speed
		update_snake()
		draw_snake()
		draw_fruits()

		-- Fast forward such that at the next frame, update_snake() will
		-- straighten out the current direction.
		snake_frame = 64 - game_speed
		return true
	end

	-- The turn will be buffered for now and be handled in the next frame,
	-- but we will accelerate forward movement now so that we arrive at
	-- the next cell in the next frame.
	snake_frame = 64 - game_speed
	return false
end

-- Advance snake animation while ignoring input until it's one frame before
-- snake enters the next cell.
--
-- This is called when player is about to die, but visually they have not
-- fully entered the next cell yet.  We will continue to animate the snake to
-- make the visual transition to death smoother.
local function wait_until_just_before_next_cell()
	-- Draw the last frame before entering waiting loop.
	gfx.sprite.update()
	coroutine.yield()

	while snake_frame + game_speed < 64 do
		update_snake()
		draw_snake()
		draw_fruits()
		-- Note that we do not call adjust_speed() here so that game_speed
		-- remain constant for this loop.
		gfx.sprite.update()
		coroutine.yield()
	end
end

-- Update score and chains right after eating a fruit.
local function update_score(fruit_color)
	-- Base score for eating any fruit.
	snake_score += 10

	-- Update chains.
	if snake_current_chain_length % 3 > 0 then
		-- Current chain length is 1 or 2.
		if fruit_color == snake_current_chain_color then
			-- Continuing current chain.
			snake_current_chain_length += 1
			if snake_current_chain_length >= 3 then
				-- Completed a chain.
				snake_chain += 1
				if snake_max_chain < snake_chain then
					snake_max_chain = snake_chain
				end
				snake_score += bonus_table[snake_chain]

				-- Flash chain dots for a bit.
				dot_timer = 16

				-- Update chain counter.
				ui_text:setTileAtPosition(1, 1, NUMBER_PLUS)
				if snake_chain > 9 then
					draw_number(3, snake_chain)
				else
					draw_number(2, snake_chain)
				end
			end
		else
			-- Broke current chain.
			snake_current_chain_length = 1
			snake_current_chain_color = fruit_color
			snake_chain = 0
			dot_timer = 0

			-- Clear chain counter.
			for x = 1, 3 do
				ui_text:setTileAtPosition(x, 1, BLANK_TILE)
			end
		end
	else
		-- Current chain length is 0 or 3, starting a new chain.
		snake_current_chain_length = 1
		snake_current_chain_color = fruit_color
		dot_timer = 0
	end

	-- Update status display.
	draw_number(34, snake_score)
	if snake_current_chain_length >= 3 then
		draw_max_chain()
	else
		update_chain_status()
	end

	snake_target_length += 1
end

-- Reset high score and remove it from status display.
local function reset_high_score()
	save_state.hi_score = 0
	playdate.datastore.write(save_state)
	draw_high_score()
end

-- Generate menu image while game is running.
local function update_game_menu_image()
	do
		gfx.pushContext(ui_menu_image)
		gfx.setBackgroundColor(gfx.kColorWhite)
		gfx.clear()
		gfx.drawText("*Score*", 4, 4)
		gfx.drawText(snake_score, 4, 26)
		gfx.drawText("*Chain*", 4, 48)
		gfx.drawText(snake_chain, 4, 70)
		gfx.drawText("*Max Chain*", 4, 92)
		gfx.drawText(snake_max_chain, 4, 114)
		gfx.drawText("*High score*", 4, 180)
		gfx.drawText(save_state.hi_score, 4, 202)
		gfx.popContext()
	end
end

-- Generate menu image for title screen.
local function update_title_menu_image()
	do
		gfx.pushContext(ui_menu_image)
		gfx.setBackgroundColor(gfx.kColorWhite)
		gfx.setColor(gfx.kColorBlack)
		gfx.clear()

		gfx.drawText("*GAME CONTROLS*", 4, 4)
		gfx.drawLine(4, 26, 131, 26)
		gfx.drawText("*Change direction*", 4, 36)
		gfx.drawText("Up/Down/Left/Right", 4, 58)
		gfx.drawText("*Change color*", 4, 80)
		gfx.drawText("A/B", 4, 102)
		gfx.drawText("*Change speed*", 4, 124)
		gfx.drawText("Crank", 4, 146)

		gfx.drawText("omoikane@uguu.org", 4, 220)
		gfx.popContext()
	end
end

--}}}

----------------------------------------------------------------------
--{{{ Game logic.

-- Game loop for interactive state.
local function game_running()
	-- Animate snake.
	update_snake()
	draw_snake()
	draw_fruits()
	draw_max_chain()

	-- Get the next buffered command.
	local command = get_next_input()
	if command then
		if command == INPUT_CHANGE_COLOR then
			-- Color change commands are always processed immediately.
			snake_color = 3 - snake_color
			fruit_transition_timer = START_FRUIT_TIMER
			redraw_snake()
		else
			-- Direction commands may or may not be processed depending on
			-- context.  If they are not processed, we push it back to the
			-- input buffer to be consumed at the next available window.
			if not handle_direction_input(command) then
				unget_next_input(command)
			end
		end
	end

	-- Check for collisions when input is not accepted.
	if snake_frame >= INPUT_ACCEPT_WINDOW then
		-- Check for grid boundaries.
		if snake_x < 1 or snake_x > GRID_W or snake_y < 1 or snake_y > GRID_H then
			-- Player is going to die, but visually it works out smoother if
			-- the head of the snake fully enters the next tile.
			wait_until_just_before_next_cell()
			enter_game_over()
			return
		end

		local cell = grid[snake_y][snake_x]
		assert(cell)
		if cell == GRID_EMPTY then
			return
		end

		if (cell & GRID_FRUIT_FLAGS) ~= 0 then
			-- Collision with fruit.
			if (cell & 0x00f) == snake_color then
				-- Eating a friendly fruit.
				grid[snake_y][snake_x] = GRID_FRUIT_EAT_FLAGS | snake_color
				fruit_eat_timer = START_FRUIT_TIMER
				update_score(snake_color)
				spawn_fruit()
			else
				-- Colliding with a hostile fruit.
				wait_until_just_before_next_cell()
				enter_game_over()
				return
			end
		elseif cell < GRID_FRUIT_FLAGS then
			-- Collision with snake body.  If the cell is exactly at where the
			-- tail of the snake is, we will pretend the collision didn't
			-- happen.  This is because visually, there was likely no tail there.
			local p = (snake_head - snake_length) & SNAKE_BUFFER_MASK
			if snake[p] and (snake_x ~= snake[p][1] or snake_y ~= snake[p][2]) then
				wait_until_just_before_next_cell()
				enter_game_over()
			end
		end
	end
end

-- Prepare to transition to title screen.
local function game_init()
	reset()
	ui_title_sprite:setVisible(true)
	ui_game_over_sprite:setVisible(false)

	-- Draw high score if it's available.
	draw_high_score()

	-- Initialize snake to start in bottom right corner, going up.
	snake_x = GRID_W
	snake_y = GRID_H
	snake_direction = INPUT_UP

	-- Set a good length for title screen snake.
	snake_target_length = 8

	game_state = GAME_TITLE
	clear_buffered_inputs = true
end

-- Game loop for title screen.
local function game_title()
	-- If no button is pressed, keep animating the snake, turning it around
	-- at corners, moving counter-clockwise.
	if input_write_index == input_read_index then
		-- Update grid and tilemaps before drawing.
		update_snake()
		draw_snake()

		if snake_x == 0 then
			-- At left edge, turn down.
			snake_x = 1
			snake_y += 1
			snake_direction = tiles.DIRECTION_LEFT_DOWN
		elseif snake_x > GRID_W then
			-- At right edge, turn up.
			snake_x = GRID_W
			snake_y -= 1
			snake_direction = tiles.DIRECTION_RIGHT_UP
		elseif snake_y == 0 then
			-- At top edge, turn left.
			snake_x -= 1
			snake_y = 1
			snake_direction = tiles.DIRECTION_UP_LEFT
		elseif snake_y > GRID_H then
			-- At bottom edge, turn right.
			snake_x += 1
			snake_y = GRID_H
			snake_direction = tiles.DIRECTION_DOWN_RIGHT
		end
		return
	end

	-- Generate snake image for fadeout.
	local snake_tile_image = gfx.image.new(400, 240, gfx.kColorClear)
	do
		gfx.pushContext(snake_tile_image)
		ui_grid:draw(GRID_X, GRID_Y)
		local head_image = ui_tile_images:getImage(snake_tiles[snake_color][snake_direction][tiles.TYPE_HEAD_FRONT][(snake_frame >> 2) + 1])
		head_image:draw(ui_snake_head_sprite:getPosition())
		gfx.popContext()
	end

	-- Transition to running state on any button press.
	ui_grid_sprite:setVisible(false)
	ui_snake_head_sprite:setVisible(false)
	ui_title_sprite:setVisible(false)
	for i = 16, 0, -1 do
		adjust_speed()
		gfx.sprite.update()
		snake_tile_image:drawFaded(0, 0, i / 16, gfx.image.kDitherTypeBayer8x8)
		ui_title_image:drawFaded(TITLE_X, TITLE_Y, i / 16, gfx.image.kDitherTypeBayer8x8)
		coroutine.yield()
	end
	ui_grid_sprite:setVisible(true)
	ui_snake_head_sprite:setVisible(true)

	enter_running_state()
end

-- Game loop for game over screen.
local function game_over()
	-- Cut to title screen on button press.
	if input_write_index ~= input_read_index then
		game_state = GAME_INIT
		return
	end
end

--}}}

----------------------------------------------------------------------
--{{{ Playdate callbacks.

playdate.getSystemMenu():addMenuItem("reset hi score", function()
	save_state.hi_score = -1
end)

function playdate.update()
	if clear_buffered_inputs then
		if input_read_index == input_write_index then
			clear_buffered_inputs = false
		else
			input_read_index = input_write_index
		end
		return
	end

	if game_state == GAME_RUNNING then
		game_running()
	elseif game_state == GAME_INIT then
		game_init()
		game_title()
	elseif game_state == GAME_TITLE then
		game_title()
	else
		game_over()
	end

	-- Respond to reset score signal.
	if save_state.hi_score < 0 then
		reset_high_score()
	end

	-- Respond to crank action.
	adjust_speed()

	gfx.sprite.update()
end

function playdate.leftButtonDown()
	input_write_index = (input_write_index + 1) & INPUT_BUFFER_MASK
	input_buffer[input_write_index] = INPUT_LEFT
end

function playdate.rightButtonDown()
	input_write_index = (input_write_index + 1) & INPUT_BUFFER_MASK
	input_buffer[input_write_index] = INPUT_RIGHT
end

function playdate.upButtonDown()
	input_write_index = (input_write_index + 1) & INPUT_BUFFER_MASK
	input_buffer[input_write_index] = INPUT_UP
end

function playdate.downButtonDown()
	input_write_index = (input_write_index + 1) & INPUT_BUFFER_MASK
	input_buffer[input_write_index] = INPUT_DOWN
end

function playdate.AButtonDown()
	input_write_index = (input_write_index + 1) & INPUT_BUFFER_MASK
	input_buffer[input_write_index] = INPUT_CHANGE_COLOR
end

function playdate.BButtonDown()
	input_write_index = (input_write_index + 1) & INPUT_BUFFER_MASK
	input_buffer[input_write_index] = INPUT_CHANGE_COLOR
end

function playdate.crankDocked()
	game_speed = MAX_SPEED
end

function playdate.gameWillPause()
	if game_state == GAME_RUNNING or game_state == GAME_OVER then
		update_game_menu_image()
	else
		update_title_menu_image()
	end
	playdate.setMenuImage(ui_menu_image)
end

--}}}
