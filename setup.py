#!/usr/bin/env python
# -*- encoding: utf-8 -*-
from __future__ import absolute_import
from __future__ import print_function

import io
import re
from glob import glob
from os.path import basename
from os.path import dirname
from os.path import join
from os.path import splitext

from setuptools import find_packages
from setuptools import setup


def read(*names, **kwargs):
    with io.open(
        join(dirname(__file__), *names),
        encoding=kwargs.get('encoding', 'utf8')
    ) as fh:
        return fh.read()


setup(
    name='py-dss-interface',
    version='0.0.9',
    license='MIT',
    description='opendsspy is a Windows Python package providing access to OpenDSS direct dll version of OpenDSS.',
    long_description='%s\n%s' % (
        re.compile('^.. start-badges.*^.. end-badges', re.M | re.S).sub('', read('README.rst')),
        re.sub(':[a-z]+:`~?(.*?)`', r'``\1``', read('CHANGELOG.rst'))
    ),
    author='Paulo Radatz',
    author_email='paulo.radatz@gmail.com',
    url='https://github.com/PauloRadatz/py_dss_interface',
    packages=find_packages('src'),
    package_dir={'': 'src'},
    package_data={'py-dss-interface': ['DDLL/x64/OpenDSSDirect.dll']},
    py_modules=[splitext(basename(path))[0] for path in glob('src/*.py')],
    include_package_data=True,
    zip_safe=False,
    classifiers=[
        # complete classifier list: http://pypi.python.org/pypi?%3Aaction=list_classifiers
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: Unix',
        'Operating System :: POSIX',
        'Operating System :: Microsoft :: Windows',
        #'Programming Language :: Python',
        #'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        #'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy',
        # uncomment if you test on these interpreters:
        # 'Programming Language :: Python :: Implementation :: IronPython',
        # 'Programming Language :: Python :: Implementation :: Jython',
        # 'Programming Language :: Python :: Implementation :: Stackless',
        'Topic :: Utilities',
    ],
    project_urls={
        'Documentation': 'https://py_dss_interface.readthedocs.io/',
        'Changelog': 'https://py_dss_interface.readthedocs.io/en/latest/changelog.html',
        'Issue Tracker': 'https://github.com/PauloRadatz/py_dss_interface/issues',
    },
    keywords=[
        # eg: 'keyword1', 'keyword2', 'keyword3',
    ],
    python_requires='>=3.6',
    install_requires=["comtypes"
        # eg: 'aspectlib==1.1.1', 'six>=1.7',
    ],
    extras_require={
           "dev": ["pytest", "pytest-cov", "sphinx-rtd-theme", "nbsphinx", "black", "pre-commit", "tox", "twine"],
        # eg:
        #   'rst': ['docutils>=0.11'],
        #   ':python_version=="2.6"': ['argparse'],
    },
)
