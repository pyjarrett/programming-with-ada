import os
import subprocess
import sys
from typing import List

def require_command(command_line : List[str], failure_msg : str) -> None:
    if subprocess.check_call(command_line) != 0:
        print(f'FAILED: {failure_msg}')
        sys.exit(1)


def os_flavored_exe(executable : str) -> str:
    return executable + '.exe' if os.name == 'win32' else executable


script_path = os.path.abspath(__file__)
script_dir = os.path.dirname(script_path)
venv_path = os.path.join(script_dir, 'venv')
python_path = os.path.join(venv_path, 'Scripts')
python_exe = os_flavored_exe(f'{python_path}/python')
sphinx_exe = os_flavored_exe(f'{python_path}/sphinx-build')

# Changes the current directory to the script directory so the virtual
# environment is created in a consistent place.
os.chdir(script_dir)
