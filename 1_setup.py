import common

import os
import sys

# Changes the current directory to the script directory so the virtual
# environment is created in a consistent place.
os.chdir(common.script_dir)

# Virtual environment exists.
if os.path.isdir ('venv/'):
    print('Virtual environment already exists at venv/')
    sys.exit(0)

common.require_command('python -m venv venv'.split(), 'Could not create virtual environment')
common.require_command(f'{common.python_exe} -m pip install --upgrade pip'.split(),
    'Could not upgrade pip')
common.require_command(f'{common.python_exe} -m pip install sphinx sphinx_rtd_theme'.split(),
    'Could not install sphinx and RTD theme')
