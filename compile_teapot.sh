#!/bin/bash

set -e

python convert_normal.py sequences/teapot_frames/*.png -n Zup --noise 0.08 --outline --serialize frames.generated.npy
python convert_normal.py frames.generated.npy -lt 65 --label_prefix "F_" --output_gb_data frames.generated.asm

cd src
./build.sh

cp ./build/main.gb ../gbshader_teapot.gb

echo 'Built ROM successfully!'
