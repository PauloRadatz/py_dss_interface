# Add this at the top if not already there
import os
import sys

# Already present, so just keep it
sys.path.insert(0, os.path.abspath('..'))
sys.path.insert(0, os.path.abspath("../../"))

# -- General configuration ---------------------------------------------

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.viewcode',
    'myst_parser',
]

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

# Other unchanged settings...
master_doc = 'index'
project = 'py_dss_interface'
year = '2020'
author = 'Paulo Radatz'
copyright = '{0}, {1}'.format(year, author)
# Get version dynamically from __init__.py
version = release = '2.3.0'  # Fallback
init_path = os.path.join(os.path.dirname(__file__), "..", "src", "py_dss_interface", "__init__.py")
if os.path.exists(init_path):
    with open(init_path, "r") as f:
        for line in f:
            if line.startswith("__version__"):
                version = release = line.split("=")[1].strip().strip("'").strip('"')
                break

language = 'en'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
pygments_style = 'sphinx'
todo_include_todos = False

# Use Read the Docs theme
html_theme = 'sphinx_rtd_theme'

# Optional: uncomment if you use static files
# html_static_path = ['_static']
