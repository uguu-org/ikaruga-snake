# Debug build.
snake.pdx/pdxinfo: \
	source/main.lua \
	source/tiles.lua \
	source/dots-table-12-12.png \
	source/numbers-table-10-12.png \
	source/speed-table-16-12.png \
	source/tiles-table-32-32.png \
	source/title.png \
	source/game_over.png \
	source/pdxinfo
	pdc source snake.pdx

# Release build.
release: ikaruga_snake.zip

ikaruga_snake.zip:
	-rm -rf snake.pdx $@
	pdc -s source snake.pdx
	zip -9 -r $@ snake.pdx

clean:
	-rm -rf snake.pdx ikaruga_snake.zip
