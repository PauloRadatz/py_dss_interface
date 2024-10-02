#!/usr/bin/env python
# -*- encoding: utf-8 -*-
from __future__ import absolute_import, print_function
import io
import re
from glob import glob
from os.path import basename, dirname, join, splitext
import os
import subprocess
from setuptools import setup, find_packages, Command
from setuptools.command.install import install
import logging

# Set up logging
log_file = 'build_log.txt'
logging.basicConfig(filename=log_file, level=logging.INFO)

class BuildOpenDSSLinux(Command):
    """Custom command to build OpenDSS for Linux systems."""
    description = "Build OpenDSS shared library for Linux."
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        # Clear the build_log.txt at the start of each installation
        with open(log_file, 'w') as f:
            f.write("")  # This will clear the file
        print("Starting custom build process for OpenDSS on Linux...")

        # Check if we are on a Linux system
        if os.name != 'posix':
            logging.info("Skipping OpenDSS build: Not on a Linux system.")
            print("Skipping OpenDSS build: Not on a Linux system.")
            return

        logging.info("Starting custom build process for OpenDSS on Linux...")
        # Step 1: Clone the OpenDSS source code
        repo_url = "https://svn.code.sf.net/p/electricdss/code/trunk/VersionC/"
        print("Downloading OpenDSS source code from SVN: ", repo_url)
        logging.info(f"Cloning OpenDSS from {repo_url}...")
        try:
            subprocess.run(["svn", "checkout", repo_url, "OpenDSSSource"], check=True)
        except FileNotFoundError as e:
            print(f"Error: SVN is not installed or not found in PATH. {e}")
            logging.error("SVN is not installed or not found in PATH.")
            raise
        except subprocess.CalledProcessError as e:
            print(f"Error: Unable to checkout SVN repository. {e}")
            logging.error(f"SVN checkout failed: {e}")
            raise

        # Check if the source code directory was created
        if not os.path.exists("OpenDSSSource"):
            logging.error("Error: OpenDSSSource directory not found. SVN checkout failed.")
            print("Error: OpenDSSSource directory not found. SVN checkout failed.")
            raise FileNotFoundError("OpenDSSSource directory not found. SVN checkout failed.")

        # Step 2: Navigate to the OpenDSS source directory
        os.chdir("OpenDSSSource")

        # Step 3: Compile the OpenDSS shared library using make
        print("Building OpenDSS shared library...")
        subprocess.run(["make"], check=True)

        # Step 4: Locate the compiled shared library (.so file)
        shared_library = "libOpenDSSC.so"
        if not os.path.exists(shared_library):
            raise FileNotFoundError(f"Build failed. {shared_library} not found.")

        # Step 5: Determine the destination directory
        dest_dir = os.path.join(
            os.path.dirname(__file__),
            "src",
            "py_dss_interface",
            "opendss_official",
            "linux",
            "cpp",
        )
        os.makedirs(dest_dir, exist_ok=True)

        # Step 6: Copy the shared library to the correct destination
        print(f"Copying {shared_library} to {dest_dir}...")
        subprocess.run(["cp", shared_library, dest_dir], check=True)

        print(f"OpenDSS shared library built and placed in {dest_dir}")

# Custom install command to ensure the build runs during installation
class CustomInstallCommand(install):
    """Customized install command to trigger OpenDSS Linux build automatically."""
    def run(self):
        logging.info("Running custom install command...")
        print("Running custom install command...")
        self.run_command('build_opendss_linux')
        install.run(self)

def read(*names, **kwargs):
    with io.open(join(dirname(__file__), *names), encoding=kwargs.get('encoding', 'utf8')) as fh:
        return fh.read()

setup(
    name='py-dss-interface',
    version='2.0.4',
    license='MIT',
    description='The py-dss-interface is a Python package that provides a Python interface to the OFFICIAL version of OpenDSS (Open-source Distribution System Simulator) software.',
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
    install_requires=["numpy", "pandas", "colorama", "comtypes"],
    extras_require={
        "dev": ["pytest", "pytest-cov", "sphinx-rtd-theme", "nbsphinx", "black", "pre-commit", "tox", "twine", "ipython", "flake8"],
    },
    cmdclass={
        'build_opendss_linux': BuildOpenDSSLinux,
        'install': CustomInstallCommand,  # Custom install command
    },
)
