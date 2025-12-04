#!/bin/bash

set -e

python convert_normal.py sequences/gbspin_frames/*.png --noise 0.00 --serialize frames.generated.npy
python convert_normal.py frames.generated.npy -lt 65 --label_prefix "F_" --output_gb_data frames.generated.asm

cd src
./build.sh

echo 'Built ROM successfully!'
