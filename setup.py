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
import urllib.request
import zipfile
import shutil

# Set up logging
log_file = 'build_log.txt'
logging.basicConfig(filename=log_file, level=logging.INFO)

# Paths to source files and build directory
source_dir = "OpenDSSC_download"
build_dir = "build"
download_url = "https://github.com/PauloRadatz/py_dss_interface/archive/refs/heads/master.zip"
download_folder = "downloaded_source"

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
        # if os.name != 'posix':
        #     logging.info("Skipping OpenDSS build: Not on a Linux system.")
        #     print("Skipping OpenDSS build: Not on a Linux system.")
        #     return

        logging.info("Starting custom build process for OpenDSS on Linux...")

        logging.info("Checking for required dependencies...")


        # List of required dependencies
        # dependencies = ["cmake", "make", "gcc", "g++"]
        #
        # # Check each dependency and attempt installation if missing
        # for dep in dependencies:
        #     try:
        #         subprocess.check_call([dep, "--version"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        #         logging.info(f"{dep} is installed.")
        #     except:
        #         logging.warning(f"{dep} is not installed. Attempting to install...")
        #         try:
        #             # Install dependency using apt-get (Debian/Ubuntu)
        #             subprocess.check_call(["sudo", "apt-get", "update"])
        #             subprocess.check_call(["sudo", "apt-get", "install", "-y", dep])
        #             logging.info(f"Successfully installed {dep}.")
        #         except:
        #             logging.error(f"Failed to install {dep}. Please install it manually")

        dependencies = ["cmake", "gcc", "make"]

        for dep in dependencies:
            try:
                subprocess.check_call([dep, "--version"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                logging.info(f"{dep} is installed.")
            except:
                logging.warning(f"{dep} is not installed. Attempting to install...")
                try:
                    # First try winget
                    subprocess.check_call(["winget", "install", "-e", "--id", f"{dep}"])
                    logging.info(f"Successfully installed {dep} using winget.")
                except:
                    logging.warning(f"winget failed to install {dep}. Trying choco...")
                    try:
                        subprocess.check_call(["choco", "install", dep, "-y"])
                        logging.info(f"Successfully installed {dep} using choco.")
                    except:
                        logging.error(f"Failed to install {dep}. Please install it manually")

        logging.info(f"OpenDSS source folder '{source_dir}' not found. Downloading from {download_url}...")

        # Check if the OpenDSS source code is available; if not, download it
        if not os.path.exists(source_dir):
            self.download_source_code()

        # Create a build directory if it doesn't exist
        if not os.path.exists(build_dir):
            os.makedirs(build_dir)
            logging.info(f"Created build directory: {build_dir}")

        # Run CMake and make to build OpenDSS
        self.build_opendss()

    def download_source_code(self):
        """Download and extract OpenDSS source code from GitHub if not available locally."""
        logging.info(f"OpenDSS source folder '{source_dir}' not found. Downloading from {download_url}...")

        try:
            # Download the repository as a zip file
            zip_path = f"{download_folder}.zip"
            urllib.request.urlretrieve(download_url, zip_path)
            logging.info(f"Downloaded source code to {zip_path}.")

            # Extract the downloaded zip file
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall(download_folder)
                logging.info(f"Extracted source code to {download_folder}.")

            # Move the extracted source files to `OpenDSSC` directory
            extracted_folder = os.path.join(download_folder, "py_dss_interface-master", "OpenDSSC")
            if os.path.exists(extracted_folder):
                # Remove the existing `OpenDSSC_downloaded` directory if it exists
                if os.path.exists(source_dir):
                    shutil.rmtree(source_dir)
                os.rename(extracted_folder, source_dir)
                logging.info(f"Moved source code to '{source_dir}'.")
            else:
                logging.error(f"Failed to find extracted OpenDSS source code in {extracted_folder}.")

            # # Cleanup downloaded files
            # os.remove(zip_path)
            # subprocess.check_call(["rm", "-rf", download_folder])  # Remove extracted folder
            # logging.info(f"Cleaned up downloaded files.")

        except:
            logging.error(f"Error during source code download")

    def build_opendss(self):
        """Build OpenDSS shared library using CMake."""

        try:
            logging.info(f"Building OpenDSS from source in {source_dir}.")

            # # Run cmake to configure the project
            # subprocess.check_call(["cmake", f"-S{source_dir}", f"-B{build_dir}"])
            # logging.info("CMake configuration completed successfully.")
            #
            # # Run make to build the shared library
            # subprocess.check_call(["cmake", "--build", build_dir])
            # logging.info("OpenDSS build completed successfully.")

            # Define the target directory for the compiled library
            target_lib_dir = os.path.join("opendss_official", "linux")
            if not os.path.exists(target_lib_dir):
                os.makedirs(target_lib_dir)
                logging.info(f"Created target directory: {target_lib_dir}")

            # Move the compiled library (e.g., libopendssc.so) to the target folder
            compiled_lib = os.path.join(build_dir, "libopendssc.so")
            if os.path.exists(compiled_lib):
                subprocess.check_call(["cp", compiled_lib, target_lib_dir])
                logging.info(f"Successfully moved {compiled_lib} to {target_lib_dir}")
            else:
                logging.error(f"Compiled library not found: {compiled_lib}")

        except:
            logging.error(f"Error during OpenDSS build")

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
