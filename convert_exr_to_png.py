import argparse
import numpy as np
from PIL import Image
import OpenEXR, Imath

# Source for EXR loading logic: ChatGPT (AI) (with manual modifications afterwards)
def load_exr_layer(path, layer_name):
    exr_file = OpenEXR.InputFile(path)
    header = exr_file.header()

    # Get size from data window
    dw = header['dataWindow']
    width  = dw.max.x - dw.min.x + 1
    height = dw.max.y - dw.min.y + 1

    FLOAT = Imath.PixelType(Imath.PixelType.FLOAT)

    # Find all channels belonging to this layer
    all_channels = header['channels'].keys()
    layer_channels = [ch[len(layer_name)+1:] for ch in all_channels if ch.startswith(layer_name + '.')]

    if not layer_channels:
        raise ValueError(f"Layer '{layer_name}' not found. Available: {list(all_channels)}")

    # Pick X, Y, Z or R, G, B.
    if 'X' in layer_channels and 'Y' in layer_channels and 'Z' in layer_channels:
        picked_channels = ['X', 'Y', 'Z']
    elif 'R' in layer_channels and 'G' in layer_channels and 'B' in layer_channels:
        picked_channels = ['R', 'G', 'B']

    channel_data = []
    for ch in picked_channels:
        raw = exr_file.channel(f'{layer_name}.{ch}', FLOAT)
        arr = np.frombuffer(raw, dtype=np.float32).reshape((height, width))
        channel_data.append(arr)

    # Stack into HxWxC array
    img = np.stack(channel_data, axis=-1)
    return img


def main():
    parser = argparse.ArgumentParser(description='Convert EXR layer to PNG')
    parser.add_argument('input', help='Input EXR file path')
    parser.add_argument('output', help='Output PNG file path')
    parser.add_argument('--layer', default='Render.Normal', help='Layer name to extract')
    args = parser.parse_args()

    img_data = load_exr_layer(args.input, args.layer)

    img_data = (img_data + 1) / 2
    img_data = np.clip(img_data, 0, 1)
    
    # Convert to 8-bit (0-255)
    img_data = (img_data * 255).round().astype(np.uint8)

    # Shape of img_data: (H, W, 3)
    
    # Save as PNG
    img = Image.fromarray(img_data, mode='RGB')
    img.save(args.output)

if __name__ == '__main__':
    main()
