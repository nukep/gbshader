#!/bin/bash

set -e

rgbasm main.asm -o build/main.o
rgbasm lookup_tables.asm -o build/lookup_tables.o
rgbasm shader.asm -o build/shader.o
rgblink -w build/*.o -l layout.link -o build/main.gb -m build/main.map -n build/main.sym
# GBC only
rgbfix -C -v build/main.gb
