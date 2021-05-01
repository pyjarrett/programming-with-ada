import os
import pathlib
import shutil

src = 'build/html'
dest = 'docs'
no_jekyll_filename = os.path.join(dest, '.nojekyll')

print(f'Copying generated html from {src} to {dest}')

if os.path.exists(dest):
    print(f'Removing existing destination: {dest}')
    shutil.rmtree(dest)

print(f'Copying {src} to {dest}')
shutil.copytree(src, dest)
pathlib.Path(no_jekyll_filename).touch()