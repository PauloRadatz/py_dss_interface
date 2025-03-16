#!/bin/bash

# Print the full path of this script
# echo "The full path of this script is: $(realpath "$0")"

# Remove the existing py_dss_interface directory if it exists

# chmod +rwx py_dss_interface
# Change directory to the cloned repository
# cd py_dss_interface 

# Unzip OpenDSSC file
unzip OpenDSSC

# Create build directory
mkdir buildlinux

# Remove the existing 'linux' directory (if it exists) and create a new one
rm -rf ./src/py_dss_interface/opendss_official/linux
mkdir -p ./src/py_dss_interface/opendss_official/linux/cpp

# Run CMake to configure the project (set Release and output type as shared library)
cmake -DCMAKE_BUILD_TYPE=Release -DMyOutputType:STRING=SHARED -S ./OpenDSSC -B ./buildlinux

# Build the project using 12 cores (adjust depending on your system's available cores)
cmake --build ./buildlinux -j 1
cd buildlinux
# Copy the built files from buildlinux to the target directory
cp -r . ../src/py_dss_interface/opendss_official/linux/cpp/
cd klusolve
cp -r libklusolve_all.so ../../src/py_dss_interface/opendss_official/linux/cpp
cp -r libklusolve_all.so.0 ../../src/py_dss_interface/opendss_official/linux/cpp
cp -r libklusolve_all.so.0.0.0 ../../src/py_dss_interface/opendss_official/linux/cpp
cd ..
# Clean up the build directory

rm -rf buildlinux
# cd ..


