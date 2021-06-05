import common

import pathlib

# Args are SOURCEDIR OUTPUTDIR FILENAMES
common.require_command(f'{common.sphinx_exe} source docs -d cached'.split(),
    'Unable to build docs with sphinx')
