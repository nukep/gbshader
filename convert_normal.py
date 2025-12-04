import numpy as np
import math
import argparse

from multiplication import Mult

mult = Mult()

# Coefficients to solve for lerp(a1, a2, dot_product)
REMAP_A1 = 0.4
REMAP_A2 = 0.7650


def mask_dilate(a):
    '''
    Perform a box dilation on the input mask array
    '''
    from scipy import ndimage

    return ndimage.binary_dilation(a, [
        [False, True, False],
        [True, True, True],
        [False, True, False],
    ])

def mask_outline(a):
    '''
    Perform a dilation, followed by setting the inside to False.
    '''
    return mask_dilate(a) ^ a

def load_rgb_image(filepath):
    '''
    Reads a RGB-encoded image. Remaps the channel values from [0,1] to [-1,+1].
    '''
    import cv2
    data = cv2.imread(filepath, cv2.IMREAD_ANYCOLOR | cv2.IMREAD_ANYDEPTH)
    # OpenCV reads images in BGR order. Convert to RGB
    data = cv2.cvtColor(data, cv2.COLOR_BGR2RGB)

    if data.dtype == np.uint16:
        maxval = 65535
    elif data.dtype == np.uint8:
        maxval = 255
    elif data.dtype == float:
        maxval = 1.0
    else:
        raise Exception(f'Unknown image value type: {data.dtype}')
    
    data = (data / (maxval/2) - 1.0)
    return data

def solve_coefficients(x, y, z, Lz):
    n_theta = np.atan2(y, x)
    
    m = np.sqrt(1 - Lz*Lz) * np.sqrt(1 - z*z)
    b = z*Lz

    return (n_theta, m, b)


def solve_coefficients_gameboy(x, y, z, Lz):
    dist2 = x*x + y*y + z*z
    if dist2 < 0.25:
        # This is probably empty space
        # Return the darkest color
        return 0, mult.float_to_log(0), 0
    if dist2 > 2.0:
        # This is probably an outline
        # Return the brightest color
        return 0, mult.float_to_log(0), 0b01110000

    n_theta, m, b = solve_coefficients(x, y, z, Lz)

    n_theta = round(n_theta / (2 * np.pi) * 256) & 0xFF

    m = (REMAP_A2 - REMAP_A1) * m
    b = (REMAP_A2 - REMAP_A1) * b + REMAP_A1

    m = mult.float_to_log(m)
    b = mult.float_to_fixed(b)

    return n_theta, m, b


def split_to_tiles(arr, size):
    '''
    Takes an array of shape (H, W, *) and returns (H/s, W/s, s, s, *).

    size (s) is the side length of the tile.
    '''
    return arr.reshape(arr.shape[0]//size, size, arr.shape[1]//size, size, *arr.shape[2:]).swapaxes(1,2)


class TileCollection:
    def __init__(self):
        self._tiles = []
        self._tiles_lookup = {}
        self._zero_tile = np.zeros((8, 8, 3), dtype=float)
        self._dropped = 0
    
    def add(self, x, y, tile):
        self._tiles.append((x, y, tile))
        self._tiles_lookup[(x,y)] = tile
    
    def log_dropped(self, x, y):
        self._dropped += 1

    def __iter__(self):
        return iter(self._tiles)
    
    def get(self, x, y):
        tile = self._tiles_lookup.get((x,y), self._zero_tile)
        return tile


class RomBankAllocator:
    def __init__(self, label_prefix):
        self._label_prefix = label_prefix
        self._data = []

    def allocate(self, data):
        i = len(self._data)+1
        label = f'{self._label_prefix}_{i}'
        self._data.append((label, data))
        return label

    def finish(self):
        out = ''
        for label, data in self._data:
            out += f'SECTION "{label}", ROMX\n'
            out += f'{label}:\n'
            out += 'db ' + (','.join(f'${x:02X}' for x in data)) + '\n'
        
        return out


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

            if num_full <= 1:
                # Discard this tile
                if num_full > 0:
                    coll.log_dropped(j, i)
            else:
                coll.add(j, i, tile)

    return coll


def image_to_normals(filepath, normal_coordinates, noise=0.0, outline=False):
    normals = load_rgb_image(filepath)
    H,W,_ = normals.shape

    # Convert normals to Y-up right-handed, if needed
    if normal_coordinates == 'Zup':
        for i in range(H):
            for j in range(W):
                x,y,z = normals[i][j]
                normals[i][j][1] = z
                normals[i][j][2] = -y

    # Pre-process the normals:
    # - Detect constant color values
    # - Ensure all normals are either 0 or unit-length.
    # - Optionally, add noise.
    for i in range(H):
        for j in range(W):
            x,y,z = normals[i][j]

            dist2 = x*x + y*y + z*z

            if dist2 < 0.5:
                # This is probably empty space
                normals[i][j] = np.array([0.0, 0.0, 0.0])
                continue
        
            if dist2 > 2.0:
                # This is probably white
                normals[i][j] = np.array([0.0, 0.0, 2.0])
                continue

            # Add some random noise to the normals, to add pseudo-dithering
            if noise > 0:
                for k in range(2):
                    normals[i][j][k] += (np.random.rand()*2 - 1) * args.noise

            # Normalize the vectors
            x,y,z = normals[i][j]
            dist = (x**2 + y**2 + z**2)**0.5
            normals[i][j] /= dist
    
    # Add outline
    # A normal of (0,0,2) indicates white (this outline).
    if outline:
        mask = (normals == np.array([0.0, 0.0, 0.0])).all(axis=-1)
        edges = mask_outline(mask)
        for i in range(H):
            for j in range(W):
                if edges[i][j]:
                    normals[i][j] = np.array([0.0, 0.0, 2.0])
    
    return normals


def run_length_encoding(iterable):
    '''
    Accepts an iterable object.
    Returns a generator that yields (value, count)
    '''
    from itertools import groupby
    for key,group in groupby(iterable):
        yield (key, sum(1 for _ in group))


def encode_tiles(tiles, Lz):
    # Write the tiles in reverse order and with reversed rows. Columns are the same order.
    # The shader decrements its output buffer pointer as it progresses, so we have to compensate this way.

    rowbuf = []

    for tile in reversed(tiles):
        for row in reversed(tile):
            pixelbuf = []
            for normal in row:
                x,y,z = normal
                n_theta, m_log, b = solve_coefficients_gameboy(x, y, z, Lz)
                pixelbuf.append((n_theta, m_log, b))
            rowbuf.append(pixelbuf)
    

    data = bytearray()

    ZERO_ROW = [(0, mult.float_to_log(0), 0)]*8

    # Check for consecutive values to do run-length encoding
    for row,count in run_length_encoding(rowbuf):
        # If we ever have two rows or more of 0's, encode it as a run-length encoding of "0"s.
        if row == ZERO_ROW and count >= 2:
            n = count // 2
            while n >= 256:
                # Note: a run-length of 0 = 256.
                # Equivalent to writing 512 rows, or 64 tiles
                data += bytearray([0, 0])
                n -= 256

            data += bytearray([0, n])
            
            count -= (count//2)*2
            # Remaining rows will be encoded the normal way
        
        for _ in range(count):
            i = 0
            for n_theta, m_log, b in row:
                if i == 0 and n_theta == 0:
                    # If a row begins with 0, it represents RLE. We can't encode 0 here.
                    # An angle of 1/256 (1.4 degrees) is practically unnoticable.
                    n_theta = 1
                data += bytearray([n_theta, m_log, b])

                i += 1
    
    return data


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('input_images', nargs='+')
    parser.add_argument('--serialize', type=str, required=False)
    parser.add_argument('--normal_coordinates', '-n', choices=['Yup', 'Zup'], required=False, default='Yup')
    parser.add_argument('--noise', type=float, default=0.0)
    parser.add_argument('--outline', action='store_true', default=False)
    parser.add_argument('--light_theta', '-lt', type=float, default=45)
    parser.add_argument('--label_prefix', default='')
    parser.add_argument('--output_gb_data')

    args = parser.parse_args()

    # Make the noise reproducable
    np.random.seed(0)

    if len(args.input_images) == 1 and args.input_images[0].endswith('.npy'):
        # Load the serialized numpy array
        all_normals = np.load(args.input_images[0])
    else:
        all_normals = []
        for img_filepath in args.input_images:
            n = image_to_normals(
                img_filepath,
                normal_coordinates=args.normal_coordinates,
                noise=args.noise,
                outline=args.outline
            )
            all_normals.append(n)

        all_normals = np.stack(all_normals)
        # Shape: [images x H x W x 3]

    if args.serialize:
        np.save(args.serialize, all_normals)
        print(f'Wrote {args.serialize}')

    if args.output_gb_data:
        Lz = np.cos(args.light_theta / 360.0 * 2*np.pi)
        print(f'Light-z = {Lz}')

        filepath = args.output_gb_data

        colls = [convert_to_tiles(normals) for normals in all_normals]

        # Get the merged layout
        layout = set()
        for coll in colls:
            for (tx,ty,_) in coll:
                layout.add((tx, ty))
        
        # Sort by top-to-bottom, left-to-right
        layout = sorted([(y,x) for x,y in layout])
        layout = [
            (x,y) for y,x in layout
        ]

        outfile = open(filepath, 'w')

        def output(s):
            print(s, file=outfile)
        
        # Tile layout

        output(f'SECTION "TILEMAP_LAYOUT", ROM0')
        output(f'TILEMAP_LAYOUT::')

        prev_xy = (None, None)
        for i, (tx, ty) in enumerate(layout):
            if prev_xy[0] == tx-1 and prev_xy[1] == ty:
                # This tile is immediately to the right of the previous one
                # We don't need to encode POS
                pass
            else:
                output(f'T_POS {tx}, {ty}')
            output(f'T_TILE {i}')
            prev_xy = tx, ty
        output('T_END')
        
        # Tile chunks

        # Batch this as 15 tiles at a time. (because that's how many tiles can be processed per frame in double-speed)
        # TODO: consider empty rows
        MAX_CHUNK_SIZE = 15

        expected_num_chunks = math.ceil(len(layout) / MAX_CHUNK_SIZE)

        print(f'Number of tiles: {len(layout)}')

        output(f'SECTION "FRAMES::", ROM0')
        output(f'EXPORT DEF NUM_FRAMES EQU {len(colls)}')
        output(f'EXPORT DEF NUM_TILE_CHUNKS EQU {expected_num_chunks}')
        output(f'FRAMES::')
        for framenum in range(len(colls)):
            output(f'  dw FRAME{framenum:02}')

        allocator = RomBankAllocator(f'{args.label_prefix}NORMALDATA')

        for framenum, coll in enumerate(colls):
            all_tiles = [
                coll.get(x, y)
                for x,y in layout
            ]

            output(f'SECTION "FRAME{framenum:02}", ROM0')
            output(f'FRAME{framenum:02}::')

            num_chunks = math.ceil(len(all_tiles) / MAX_CHUNK_SIZE)

            assert(num_chunks == expected_num_chunks)

            for ch in range(num_chunks):
                tiles = all_tiles[ch*MAX_CHUNK_SIZE : (ch+1)*MAX_CHUNK_SIZE]
                num_tiles = len(tiles)
                first_tile_num = ch*MAX_CHUNK_SIZE

                data = encode_tiles(tiles, Lz)

                data_label = allocator.allocate(data)

                output(f'TILECHUNK {data_label}, {num_tiles}, {first_tile_num}')

        output(allocator.finish())

        outfile.close()

        print(f'Wrote {filepath}')
