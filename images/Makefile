# Split images.png into various tile maps.
#
# images.png is exported by Inkscape from images.svg, with antialias
# set to zero.  It's tricky to automate that process, which is why
# it's included here.
#
# The tricky part has to do with the export settings not accessible
# through the command line.  The closest option is to set
# export-png-color-mode=Gray_1, but that loses transparency.  Other
# hacks I have tried include using imagemagick or netpbm to threshold
# the output to black and white, or export to PDF and use GhostScript
# to rasterize.  None of these gets the pixels quite right.
#
# You might say that it's silly to be making pixel graphics with a
# vector editing tool.  Your opinion is noted.

targets = \
	title.png \
	game_over.png \
	launch_image.png \
	tiles-table-32-32.png \
	numbers-table-10-12.png \
	dots-table-12-12.png \
	speed-table-16-12.png

tiles_index = tiles.lua


all: $(targets)

# Note that all images go through a round trip to PPM and back.  This
# removes all EXIF data that was embedded in the PNG.
#
# In theory, we could get additional size savings with pngcrush, but
# on my machine pngcrush doesn't always win over pnmtopng, so that
# step is done manually.
title.png: t_title_pixels.ppm t_title_alpha.ppm
	pnmtopng -compression 9 -alpha=t_title_alpha.ppm $< > $@

t_title_pixels.ppm: t_title.png
	pngtopnm $< | ppmtoppm > $@

t_title_alpha.ppm: t_title.png
	pngtopnm -alpha $< > $@

t_title.png: images.png
	convert $< -crop 284x60+1554+130 $@

game_over.png: t_game_over_pixels.ppm t_game_over_alpha.ppm
	pnmtopng -compression 9 -alpha=t_game_over_alpha.ppm $< > $@

t_game_over_pixels.ppm: t_game_over.png
	pngtopnm $< | ppmtoppm > $@

t_game_over_alpha.ppm: t_game_over.png
	pngtopnm -alpha $< > $@

t_game_over.png: images.png
	convert $< -crop 303x47+1184+224 $@

tiles-table-32-32.png: t_tiles_pixels.ppm t_tiles_alpha.ppm
	pnmtopng -compression 9 -alpha=t_tiles_alpha.ppm $< > $@

t_tiles_pixels.ppm: t_tiles_table.png
	pngtopnm $< | ppmtoppm > $@

t_tiles_alpha.ppm: t_tiles_table.png
	pngtopnm -alpha $< > $@

t_tiles_table.png: images.png generate_tiles.pl
	perl generate_tiles.pl $< $@ $(tiles_index)

numbers-table-10-12.png: t_numbers_table.png
	pngtopnm $< | ppmtoppm | pnmtopng -compression 9 > $@

t_numbers_table.png: t_d0.png t_d1.png t_d2.png t_d3.png t_d4.png t_d5.png t_d6.png t_d7.png t_d8.png t_d9.png t_dp.png t_dh.png t_di.png t_d_blank.png
	convert +append $^ $@

t_d0.png: images.png
	convert $< -crop 10x12+1344+352 $@

t_d1.png: images.png
	convert $< -crop 10x12+1376+352 $@

t_d2.png: images.png
	convert $< -crop 10x12+1408+352 $@

t_d3.png: images.png
	convert $< -crop 10x12+1440+352 $@

t_d4.png: images.png
	convert $< -crop 10x12+1472+352 $@

t_d5.png: images.png
	convert $< -crop 10x12+1504+352 $@

t_d6.png: images.png
	convert $< -crop 10x12+1536+352 $@

t_d7.png: images.png
	convert $< -crop 10x12+1568+352 $@

t_d8.png: images.png
	convert $< -crop 10x12+1600+352 $@

t_d9.png: images.png
	convert $< -crop 10x12+1632+352 $@

t_dp.png: images.png
	convert $< -crop 10x12+1664+352 $@

t_dh.png: images.png
	convert $< -crop 10x12+1696+352 $@

t_di.png: images.png
	convert $< -crop 10x12+1728+352 $@

t_d_blank.png: images.png
	convert $< -crop 10x12+1750+352 $@

dots-table-12-12.png: t_dots_table.png
	pngtopnm $< | ppmtoppm | pnmtopng -compression 9 > $@

t_dots_table.png: t_cl0.png t_cl1.png t_cd0.png t_cd1.png t_c_blank.png
	convert +append $^ $@

t_cl0.png: images.png
	convert $< -crop 12x12+1024+224 $@

t_cl1.png: images.png
	convert $< -crop 12x12+1088+224 $@

t_cd0.png: images.png
	convert $< -crop 12x12+1024+544 $@

t_cd1.png: images.png
	convert $< -crop 12x12+1088+544 $@

t_c_blank.png: images.png
	convert $< -crop 12x12+1056+224 $@

speed-table-16-12.png: t_speed_table.png
	pngtopnm $< | ppmtoppm | pnmtopng -compression 9 > $@

t_speed_table.png: t_snail.png t_turtle.png t_rabbit.png
	convert +append $^ $@

t_snail.png: images.png
	convert $< -crop 16x12+1344+32 $@

t_turtle.png: images.png
	convert $< -crop 16x12+1376+32 $@

t_rabbit.png: images.png
	convert $< -crop 16x12+1408+32 $@

launch_image.png: t_launch_image.png
	pngtopnm $< | ppmtoppm | pnmtopng -compression 9 > $@

t_launch_image.png: images.png
	convert $< -crop 400x240+1496+36 $@

clean:
	-rm -f $(targets) $(tiles_index) t_*
