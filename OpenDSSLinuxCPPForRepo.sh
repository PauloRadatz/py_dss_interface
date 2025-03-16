#!/bin/bash

# Print the full path of this script
# echo "The full path of this script is: $(realpath "$0")"

echo "🚀 Installing required dependencies..."

# Install required packages: CMake, build-essential (includes g++), and uuid-dev
sudo apt update && sudo apt install -y cmake build-essential uuid-dev

echo "✅ Dependencies installed successfully!"

# Unzip VersionC file
echo "📦 Unzipping VersionC..."
unzip VersionC

# Create build directory
echo "📂 Creating build directory..."
mkdir -p buildlinux

# Remove the existing 'linux' directory (if it exists) and create a new one
echo "🛠️ Removing old Linux build directory and creating a new one..."
rm -rf ./src/py_dss_interface/opendss_official/linux
mkdir -p ./src/py_dss_interface/opendss_official/linux/cpp

# Run CMake to configure the project (set Release and output type as shared library)
echo "⚙️ Running CMake configuration..."
cmake -DCMAKE_BUILD_TYPE=Release -DMyOutputType:STRING=SHARED -S ./VersionC -B ./buildlinux

# Build the project using 1 core (adjust depending on your system's available cores)
echo "🔨 Building the project..."
cmake --build ./buildlinux -j 1

# Move into build directory
cd buildlinux

# Copy the built files from buildlinux to the target directory
echo "📁 Copying build files to target directory..."
cp -r . ../src/py_dss_interface/opendss_official/linux/cpp/

# Move into klusolve directory and copy shared libraries
cd klusolve
echo "📦 Copying klusolve shared libraries..."
cp -r libklusolve_all.so ../../src/py_dss_interface/opendss_official/linux/cpp
cp -r libklusolve_all.so.0 ../../src/py_dss_interface/opendss_official/linux/cpp
cp -r libklusolve_all.so.0.0.0 ../../src/py_dss_interface/opendss_official/linux/cpp
cd ..

# Clean up the build directory
echo "🧹 Cleaning up build directory..."
rm -rf buildlinux

echo "✅ Build process completed successfully!"
