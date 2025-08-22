import numpy as np
import math


BASE = 2**(1/6)
FIXED_DENOM = 127

# Enable to assign log(0) to a log-space value that yields a very small value near zero
ALLOW_LOG_ZERO = True

# Our program supports products of up to three (3) factors. The number cannot overflow into the signed bit.
MAX_FACTORS = 3

# Fixed-point numbers have the format:
# 0   = 0/127 = 0.0
# 1   = 1/127
# 127 = 127/127  = +1.0
# 128 = -128/127 = -1.00787
# 129 = -127/128 = -1.0
# 255 = -1/127

# Log-space numbers have the format:
# | sign bit | mantissa (7 bits) |


def byte_to_signed(x):
    x = x & 0xFF
    if x >= 128:
        return x - 256
    return x


class Mult:
    def __init__(self):
        pass

    def float_to_fixed(self, x):
        if x > 1:
            x = 1
        if x < -1:
            x = -1
        return round(x * FIXED_DENOM) & 0xFF

    def fixed_to_float(self, x):
        return byte_to_signed(x) / FIXED_DENOM

    def float_to_log(self, x):
        '''
        Convert a floating point number to log-space.
        x = [-1.0, 1.0]

        Returns: y = [0, 255]
        '''

        neg = x < 0
        x = abs(x)

        if x == 0:
            if ALLOW_LOG_ZERO:
                return math.floor(127 / MAX_FACTORS)
            raise Exception('log(0) is undefined')
        
        if x < 1 / FIXED_DENOM:
            x = 1 / FIXED_DENOM
        
        mantissa = -round(np.log(x)/np.log(BASE))
        return (128 if neg else 0) | mantissa

    def fixed_to_log(self, x):
        '''
        Convert a fixed point number to log-space.
        x = [0, 255]

        Returns: y = [0, 255]
        '''

        x = byte_to_signed(x)
        x = x / FIXED_DENOM
        return self.float_to_log(x)

    def cos_float_to_log(self, theta):
        '''theta is [0, 2pi)'''
        return self.float_to_log(np.cos(theta))

    def cos_fixed_to_log(self, theta):
        '''theta is [0, 256)'''
        return self.cos_float_to_log(theta / 256 * 2 * np.pi)

    def log_to_fixed(self, y):
        '''
        Convert a log-space number to fixed point.
        y = [0, 255]

        Returns: x = [0, 255]
        '''
        neg = (y & 0x80) != 0
        y = y & 0x7F

        x = round(BASE**(-y) * FIXED_DENOM)
        if neg:
            x = -x
        return x & 0xFF
    
    def log_to_float(self, y):
        '''
        Convert a log-space number to floating point.
        y = [0, 255]

        Returns: x = [-1.0, 1.0]
        '''
        x = byte_to_signed(self.log_to_fixed(y))
        return x / FIXED_DENOM
    
    def log_products(self, *ys):
        s = 0
        for y in ys:
            s = (s + y) & 0xFF
        return s


if __name__ == '__main__':
    # Run tests

    mult = Mult()

    assert(mult.fixed_to_log(1)*MAX_FACTORS < 128)
