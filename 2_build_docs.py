import common

# Args are SOURCEDIR OUTPUTDIR FILENAMES
common.require_command(f'{common.sphinx_exe} source docs -d cached -E'.split(),
    'Unable to build docs with sphinx')
