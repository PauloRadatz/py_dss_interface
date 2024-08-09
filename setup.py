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
    version='2.0.4',
    license='MIT',
    description='The py-dss-interface is a Python package that provides a Python interface to the OFFICIAL version of OpenDSS (Open-source Distribution System Simulator) software. OpenDSS is a free, open-source software for simulating and analyzing power distribution systems.',
    long_description='%s\n%s' % (
        re.compile('^.. start-badges.*^.. end-badges', re.M | re.S).sub('', read('README.rst')),
        re.sub(':[a-z]+:`~?(.*?)`', r'``\1``', read('CHANGELOG.rst'))
    ),
    author='Paulo Radatz, Ênio Viana, Rodolfo Londero',
    author_email='paulo.radatz@gmail.com, eniocc@gmail.com, rodolfopl@gmail.com ',
    url='https://github.com/PauloRadatz/py_dss_interface',
    packages=find_packages('src'),
    package_dir={'': 'src'},
    package_data={
        'py_dss_interface': [
            'opendss_official/windows/delphi/x64/*.dll',
            'opendss_official/windows/delphi/x86/*.dll',
            'opendss_official/windows/cpp/x64/*.dll',
            'opendss_official/windows/cpp/x86/*.dll',
            'opendss_official/linux/cpp/x64/*.so',
        ]
    },
    py_modules=[splitext(basename(path))[0] for path in glob('src/*.py')],
    include_package_data=True,
    zip_safe=False,
    classifiers=[
        # complete classifier list: http://pypi.python.org/pypi?%3Aaction=list_classifiers
        'Development Status :: 4 - Beta',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Operating System :: Microsoft :: Windows',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3 :: Only',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Programming Language :: Python :: 3.12',
        'Topic :: Scientific/Engineering',
        'Topic :: Software Development :: Libraries :: Python Modules',
    ],
    project_urls={
        'Documentation': 'https://py_dss_interface.readthedocs.io/',
        'Changelog': 'https://py_dss_interface.readthedocs.io/en/latest/changelog.html',
        'Issue Tracker': 'https://github.com/PauloRadatz/py_dss_interface/issues',
        'Source': 'https://github.com/PauloRadatz/py_dss_interface',
    },
    keywords=[
        'OpenDSS', 'power distribution', 'simulation', 'electrical engineering', 'education',
    ],
    python_requires='>=3.9',
    install_requires=["numpy", "pandas", "colorama", "comtypes"
                      ],
    extras_require={
        "dev": ["pytest", "pytest-cov", "sphinx-rtd-theme", "nbsphinx", "black", "pre-commit", "tox", "twine", "ipython"],
    },
)
