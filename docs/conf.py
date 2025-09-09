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
py_dss_interface_doc = 'py-dss-interface Documentation'
version = release = '2.2.1'
language = 'en'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
pygments_style = 'sphinx'
todo_include_todos = False

# Use Read the Docs theme
html_theme = 'sphinx_rtd_theme'

# Optional: uncomment if you use static files
# html_static_path = ['_static']
