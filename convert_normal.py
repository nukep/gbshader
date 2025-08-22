import numpy as np
import math

from multiplication import Mult

import argparse


mult = Mult()


# Source for EXR loading logic: ChatGPT
def load_exr_layer(path, layer_name):
    import OpenEXR, Imath

    exr_file = OpenEXR.InputFile(path)
    header = exr_file.header()

    # Get size from data window
    dw = header['dataWindow']
    width  = dw.max.x - dw.min.x + 1
    height = dw.max.y - dw.min.y + 1

    FLOAT = Imath.PixelType(Imath.PixelType.FLOAT)

    # Find all channels belonging to this layer
    all_channels = header['channels'].keys()
    layer_channels = [ch for ch in all_channels if ch.startswith(layer_name)]

    if not layer_channels:
        raise ValueError(f"Layer '{layer_name}' not found. Available: {list(all_channels)}")

    # Example: layer_channels might be ['Normal.X', 'Normal.Y', 'Normal.Z']
    channel_data = []
    for ch in sorted(layer_channels):  # Sort so X,Y,Z are consistent
        raw = exr_file.channel(ch, FLOAT)
        arr = np.frombuffer(raw, dtype=np.float32).reshape((height, width))
        channel_data.append(arr)

    # Stack into HxWxC array
    img = np.stack(channel_data, axis=-1)
    return img


def solve_coefficients(x, y, z, Lz):
    if x*x + y*y + z*z < 0.1:
        # This is probably empty space
        return (0.0, 0.0, 0.0)

    n_theta = np.atan2(y, x)
    
    m = np.sqrt(1 - Lz*Lz) * np.sqrt(1 - z*z)
    b = z*Lz

    return (n_theta, m, b)


def solve_coefficients_gameboy(x, y, z, Lz, L_theta=None):
    n_theta, m, b = solve_coefficients(x, y, z, Lz)
    n_theta = round(n_theta / (2 * np.pi) * 256) & 0xFF

    m = mult.float_to_log(m * 0.5)
    b = mult.float_to_fixed(b * 0.5 + 0.2)

    if L_theta is not None:
        L_theta = round(L_theta / (2 * np.pi) * 256) & 0xFF
        return n_theta, m, b, L_theta
    else:
        return n_theta, m, b


def solve_dot(x, y, z, Lz, L_theta):
    n_theta, m_log, b, L_theta = solve_coefficients_gameboy(x, y, z, Lz, L_theta)

    v = mult.log_to_fixed(m_log + mult.cos_fixed_to_log(n_theta - L_theta)) + b

    v = v & 0xFF

    if v >= 128:
        return 0

    v = v << 1
    v = v & 0b11000000
    idx = v >> 6

    return idx


def split_to_tiles(arr, size):
    '''
    Takes an array of shape (H, W, *) and returns (H/s, W/s, s, s, *).

    size (s) is the side length of the tile.
    '''
    return arr.reshape(arr.shape[0]//size, size, arr.shape[1]//size, size, *arr.shape[2:]).swapaxes(1,2)


class TileCollection:
    def __init__(self):
        self._tiles = []
        self._dropped = 0
    
    def add(self, x, y, tile):
        self._tiles.append((x, y, tile))
    
    def log_dropped(self, x, y):
        self._dropped += 1

    def __iter__(self):
        return iter(self._tiles)


def count_empty(tile):
    empty = np.array([0.0, 0.0, 0.0])
    mask = (tile == empty).all(axis=len(tile.shape)-1)
    count = np.sum(mask)
    return count

def convert_to_tiles(normals):
    # Convert 8x8 sections of the image to tiles (8 x 8 x 3)

    coll = TileCollection()

    tiles = split_to_tiles(normals, 8)
    H,W = tiles.shape[:2]
    for i in range(H):
        for j in range(W):
            tile = tiles[i][j]
            num_empty = count_empty(tile)
            num_full = 64 - num_empty

            if num_full <= 4:
                # Discard this tile
                if num_full > 0:
                    coll.log_dropped(j, i)
            else:
                coll.add(j, i, tile)

    return coll


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('input_image')
    parser.add_argument('--exr_layer_name', '-l')
    parser.add_argument('--normal_coordinates', '-n', choices=['Yup', 'Zup'], required=False, default='Yup')
    parser.add_argument('--noise', type=float, default=0.0)
    parser.add_argument('--light_z', '-lz', type=float, default=0.4)
    parser.add_argument('--output_gb_data')
    parser.add_argument('--output_rendered_frames')

    args = parser.parse_args()

    normals = load_exr_layer(args.input_image, args.exr_layer_name)
    H,W,_ = normals.shape

    # Convert normals to Y-up right-handed, if needed
    if args.normal_coordinates == 'Zup':
        for i in range(H):
            for j in range(W):
                x,y,z = normals[i][j]
                normals[i][j][1] = z
                normals[i][j][2] = -y

    # Pre-process the normals:
    # - Ensure all normals are either 0 or unit-length.
    # - Optionally, add noise.
    for i in range(H):
        for j in range(W):
            x,y,z = normals[i][j]

            if x*x + y*y + z*z < 0.5:
                # This is probably empty space
                normals[i][j] = np.array([0.0, 0.0, 0.0])
                continue

            # Add some random noise to the normals, to add pseudo-dithering
            if args.noise > 0:
                for k in range(2):
                    normals[i][j][k] += (np.random.rand()*2 - 1) * args.noise

            # Normalize the vectors
            x,y,z = normals[i][j]
            dist = (x**2 + y**2 + z**2)**0.5
            normals[i][j] /= dist
    
    Lz = args.light_z
    print(f'Light-z = {Lz}')

    if args.output_rendered_frames:
        # Render an animation

        import pathlib
        import cv2

        frames_path = pathlib.Path(args.output_rendered_frames)
        frames_path.mkdir(parents=True, exist_ok=True)

        GB_PALETTE = [
            [0x08, 0x18, 0x20],
            [0x34, 0x68, 0x56],
            [0x88, 0xc0, 0x70],
            [0xe0, 0xf8, 0xd0]
        ]
        GB_PALETTE = [np.array(x) for x in GB_PALETTE]

        NUM_FRAMES = 40

        for frame in range(NUM_FRAMES):
            rows = []

            L_theta = 6.28 * (frame/NUM_FRAMES)

            for i in range(H):
                cols = []
                for j in range(W):
                    N = normals[i][j]
                    cols.append(GB_PALETTE[solve_dot(N[0],N[1],N[2], Lz, L_theta)])
                rows.append(cols)

            data = np.array(rows)
            out_path = frames_path / f'{frame:02d}.png'
            cv2.imwrite(out_path.absolute(), data)
            print(f'Wrote {out_path}')
    
    if args.output_gb_data:
        coll = convert_to_tiles(normals)

        for x,y,tile in coll:
            if x == 7 and y == 7:
                break
        
        # Write the tiles in reverse order and with reversed rows. Columns are the same order.
        # The shader decrements its output buffer pointer as it progresses, so we have to compensate this way.

        data = bytearray()

        for row in reversed(tile):
            for normal in row:
                x,y,z = normal
                n_theta, m_log, b = solve_coefficients_gameboy(x, y, z, Lz)
                data += bytearray([n_theta, m_log, b])
                # print(n_theta, m_log, b)
        
        filepath = args.output_gb_data
        with open(filepath, 'wb') as f:
            f.write(data)

        print(f'Wrote {filepath}')
