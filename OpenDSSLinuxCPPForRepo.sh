#!/bin/bash

# Function to check if a package is installed
is_installed() {
    dpkg -l | grep -q "$1"
}

echo "🚀 Checking and installing required dependencies..."

# Install missing dependencies
MISSING_PACKAGES=""

# Check and install CMake
if ! is_installed "cmake"; then
    echo "⚙️ Installing CMake..."
    MISSING_PACKAGES+=" cmake"
fi

# Check and install build-essential (includes g++)
if ! is_installed "build-essential"; then
    echo "🛠️ Installing build-essential (includes g++)..."
    MISSING_PACKAGES+=" build-essential"
fi

# Check and install uuid-dev
if ! is_installed "uuid-dev"; then
    echo "🔑 Installing uuid-dev..."
    MISSING_PACKAGES+=" uuid-dev"
fi

# Check and install unzip
if ! is_installed "unzip"; then
    echo "📦 Installing unzip..."
    MISSING_PACKAGES+=" unzip"
fi

# If any package is missing, install them
if [ -n "$MISSING_PACKAGES" ]; then
    sudo apt update && sudo apt install -y $MISSING_PACKAGES
    echo "✅ Required dependencies installed!"
else
    echo "✅ All required dependencies are already installed!"
fi


# Unzip VersionC file
# Delete VersionC folder if it exists
[ -d "VersionC" ] && rm -rf VersionC
echo "📦 Unzipping VersionC..."
unzip VersionC

# Create build directory
# Delete buildlinux folder if it exists
[ -d "buildlinux" ] && rm -rf buildlinux
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
for f in libklusolve_all.so libklusolve_all.so.0 libklusolve_all.so.0.0.0; do
    if [ -f "$f" ]; then
        cp "$f" ../../src/py_dss_interface/opendss_official/linux/cpp
    fi
done
cd ..

# Clean up the build directory
cd ../
echo "🧹 Cleaning up build directory..."
# Delete buildlinux folder if it exists
[ -d "buildlinux" ] && rm -rf buildlinux
# Delete VersionC folder if it exists
[ -d "VersionC" ] && rm -rf VersionC

echo "✅ Build process completed successfully!"
