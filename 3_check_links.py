import common

# Args are SOURCEDIR OUTPUTDIR FILENAMES
link_check_dir = 'docs/linkcheck'

common.require_command(f'{common.sphinx_exe} -b linkcheck source {link_check_dir}'.split(),
    'Unable to build docs with sphinx')
