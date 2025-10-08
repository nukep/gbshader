#!/bin/bash

set -e

rgbgfx hud.png -c '#000,#555,#aaa,#fff' -u -o build/hud_tiledata.bin -t build/hud_tilemap.bin -a build/hud_tileattr.bin

rgbasm main.asm -o build/main.o
rgbasm lookup_tables.asm -o build/lookup_tables.o
rgbasm shader.asm -o build/shader.o
rgbasm image.asm -o build/image.o
rgblink -w build/*.o -l layout.link -o build/main.gb -m build/main.map -n build/main.sym
# GBC only
rgbfix -C -v -m mbc3 build/main.gb
