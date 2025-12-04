# Game Boy Color Shader ROM

A real-time shader on the Game Boy Color!

The ROMs can be found in the GitHub Releases page.

## Compilation

To compile the demos, run `./compile_gbspin.sh` or `./compile_teapot.sh`.

Ensure you have the RGBDS toolkit installed and on your PATH: https://rgbds.gbdev.io/

RGBDS v0.9.1 is the version used for this project. Later versions may or may not work.

The normal map conversion is a Python script, which has some dependencies. Install the dependencies using pip:

```sh
pip install opencv-python scipy
```
