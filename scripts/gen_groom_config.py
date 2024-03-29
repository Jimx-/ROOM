import json
import argparse
import os
import sys

if __name__ == '__main__':
    sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
    from room.types import HasCoreParams

    parser = argparse.ArgumentParser()
    parser.add_argument('input', type=str, help='GROOM JSON configuration')
    parser.add_argument('output', type=str, help='C header')

    args = parser.parse_args()

    with open(args.input, 'r') as f:
        params = json.load(f)

    core_params = HasCoreParams(params)

    with open(args.output, 'w') as f:
        f.write('''// Auto-generated by gen_groom_config.py.  DO NOT EDIT.

#ifndef _GROOM_CONFIG_H_
#define _GROOM_CONFIG_H_

''')

        f.write(f'#define USE_SMEM {int(core_params.use_smem)}\n')
        if core_params.use_smem:
            f.write(
                f'#define SMEM_BASE {hex(core_params.smem_base+core_params.smem_size)}\n'
            )
            f.write(f'#define SMEM_SIZE {hex(core_params.smem_size)}\n')
        f.write('\n')

        f.write('#endif\n')
