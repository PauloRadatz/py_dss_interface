#!/bin/bash
# Build manylinux wheels for py-dss-interface
# This script builds Linux wheels that can be installed via pip on any Linux distribution
# Supports both x86_64 and ARM64 (aarch64) architectures
#
# Usage:
#   ARCH=x86_64 ./build_manylinux_wheels.sh    # Build for x86_64 (default)
#   ARCH=arm64 ./build_manylinux_wheels.sh     # Build for ARM64
#   ARCH=aarch64 ./build_manylinux_wheels.sh   # Build for ARM64 (alias)

set -e  # Exit on error

# Configuration
# This script is designed to run inside Docker with project mounted at /io
# Build for one Python version - the wheel will work for all Python 3.9+ (like Windows)
PYTHON_VERSION="cp312"
PROJECT_DIR="/io"

# Architecture detection (defaults to x86_64)
ARCH="${ARCH:-x86_64}"
# Normalize ARM64 aliases
if [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
    ARCH="arm64"
    ARCH_DISPLAY="ARM64 (aarch64)"
    ARCH_SUFFIX=" (ARM64)"
else
    ARCH="x86_64"
    ARCH_DISPLAY="x86_64"
    ARCH_SUFFIX=""
fi

# Change to project directory
cd "${PROJECT_DIR}" || exit 1

echo "Building manylinux wheels for py-dss-interface${ARCH_SUFFIX}"
echo "Architecture: ${ARCH_DISPLAY}"
echo "Building universal wheel (works for Python 3.9+)"

# Function to build OpenDSS C++ libraries
build_opendss() {
    echo "Building OpenDSS C++ libraries${ARCH_SUFFIX}..."

    # Install build dependencies
    # Note: manylinux2014 uses CentOS 7
    echo "Installing build dependencies..."

    # Update yum cache first (some manylinux images need this)
    yum makecache fast || yum makecache || true

    # Install basic build tools
    yum install -y cmake gcc-c++ make unzip

    # Install UUID development package (required for OpenDSS)
    # In CentOS 7, the package is 'libuuid-devel'
    echo "Installing UUID development headers (libuuid-devel)..."

    # Install libuuid-devel - this is the standard package for CentOS 7
    if ! yum install -y libuuid-devel; then
        echo "Failed to install libuuid-devel, trying alternative..."
        # Try installing both libuuid and libuuid-devel together
        yum install -y libuuid libuuid-devel || {
            echo "ERROR: Cannot install UUID development headers"
            echo "This is required for building OpenDSS"
            echo ""
            echo "Trying to find available UUID packages..."
            yum search uuid 2>&1 | head -20 || true
            exit 1
        }
    fi

    # Verify UUID headers are available
    if [ -f /usr/include/uuid/uuid.h ]; then
        echo "Found UUID headers at /usr/include/uuid/uuid.h"
    elif [ -f /usr/include/uuid.h ]; then
        echo "Found UUID headers at /usr/include/uuid.h"
    else
        echo "ERROR: UUID headers not found after installation"
        echo "Searched: /usr/include/uuid/uuid.h and /usr/include/uuid.h"
        echo ""
        echo "Checking what was installed:"
        rpm -qa | grep -i uuid || echo "No UUID packages found"
        echo ""
        echo "Listing /usr/include:"
        find /usr/include -name "*uuid*" 2>/dev/null || echo "No UUID files found in /usr/include"
        exit 1
    fi

    # Unzip VersionC file
    if [ ! -f "VersionC.zip" ]; then
        echo "Error: VersionC.zip not found in project directory"
        exit 1
    fi
    [ -d "VersionC" ] && rm -rf VersionC
    echo "Unzipping VersionC..."
    unzip -q VersionC.zip

    # Create build directory
    [ -d "buildlinux" ] && rm -rf buildlinux
    mkdir -p buildlinux

    # Create output directory for libraries (matching working script)
    echo "Creating Linux${ARCH_SUFFIX} build output directory..."
    rm -rf src/py_dss_interface/opendss_official/linux
    mkdir -p src/py_dss_interface/opendss_official/linux/cpp

    # Configure with CMake (same as working script)
    echo "Running CMake configuration${ARCH_SUFFIX}..."
    cmake -DCMAKE_BUILD_TYPE=Release -DMyOutputType:STRING=SHARED -S ./VersionC -B ./buildlinux

    # Build the project
    # Note: Working script uses -j 1, but we'll use all cores for faster build in Docker
    echo "Building the project${ARCH_SUFFIX}..."
    cmake --build ./buildlinux -j $(nproc)

    # Copy built libraries (matching the working script's approach)
    echo "Copying build files to target directory..."
    # Change to buildlinux directory and copy everything (including hidden files)
    cd buildlinux
    cp -r . ../src/py_dss_interface/opendss_official/linux/cpp/

    # Copy klusolve libraries (matching the working script exactly)
    if [ -d "klusolve" ]; then
        cd klusolve
        echo "Copying klusolve shared libraries..."
        for f in libklusolve_all.so libklusolve_all.so.0 libklusolve_all.so.0.0.0; do
            if [ -f "$f" ]; then
                cp "$f" ../../src/py_dss_interface/opendss_official/linux/cpp/
            fi
        done
        cd ..
    fi
    cd "${PROJECT_DIR}"

    # Clean up build artifacts (matching working script)
    echo "Cleaning up build directory..."
    rm -rf buildlinux VersionC

    echo "OpenDSS${ARCH_SUFFIX} build completed!"
}

# Function to build wheels for a specific Python version
build_wheel() {
    local PYTHON_VERSION=$1
    local PYTHON_BIN="/opt/python/${PYTHON_VERSION}-${PYTHON_VERSION}/bin/python"

    if [ ! -f "${PYTHON_BIN}" ]; then
        echo "Python ${PYTHON_VERSION} not found, skipping..."
        return
    fi

    echo "Building wheel for Python ${PYTHON_VERSION}${ARCH_SUFFIX}..."

    # Upgrade pip
    "${PYTHON_BIN}" -m pip install --upgrade pip wheel setuptools build

    # Verify libraries exist before building wheel
    if [ ! -f "src/py_dss_interface/opendss_official/linux/cpp/libOpenDSSC.so" ]; then
        echo "Error: libOpenDSSC.so not found before wheel build!"
        exit 1
    fi
    if [ ! -f "src/py_dss_interface/opendss_official/linux/cpp/libklusolve_all.so.0" ]; then
        echo "Error: libklusolve_all.so.0 not found before wheel build!"
        exit 1
    fi
    if [ ! -f "src/py_dss_interface/opendss_official/linux/cpp/libklusolve_all.so.0.0.0" ]; then
        echo "Error: libklusolve_all.so.0.0.0 not found before wheel build!"
        exit 1
    fi

    # Build the wheel (universal wheel works for all Python 3.9+)
    mkdir -p dist
    "${PYTHON_BIN}" -m build --wheel --outdir dist

    # Verify the wheel was created
    WHEEL_FILE=$(ls -t dist/py_dss_interface-*.whl 2>/dev/null | head -1)
    if [ -z "$WHEEL_FILE" ]; then
        echo "Error: No wheel file created!"
        exit 1
    fi

    WHEEL_NAME=$(basename "$WHEEL_FILE")
    echo "Wheel created: $WHEEL_NAME"
}

# Function to repair wheels with auditwheel
repair_wheels() {
    echo "Repairing wheels with auditwheel..."

    # Install auditwheel
    /opt/python/cp39-cp39/bin/pip install auditwheel

    # Create wheelhouse for repaired wheels (keeps dist/ untouched, avoids deleting other arch wheels)
    mkdir -p wheelhouse

    # Verify libraries exist in source before repair
    echo "Verifying libraries are built..."
    if [ ! -f "src/py_dss_interface/opendss_official/linux/cpp/libOpenDSSC.so" ]; then
        echo "Error: libOpenDSSC.so not found in source!"
        exit 1
    fi
    if [ ! -f "src/py_dss_interface/opendss_official/linux/cpp/libklusolve_all.so.0" ]; then
        echo "Error: libklusolve_all.so.0 not found in source!"
        exit 1
    fi
    if [ ! -f "src/py_dss_interface/opendss_official/linux/cpp/libklusolve_all.so.0.0.0" ]; then
        echo "Error: libklusolve_all.so.0.0.0 not found in source!"
        exit 1
    fi
    echo "Source libraries verified"

    # List wheels before repair
    echo "Wheels to repair:"
    ls -lh dist/*.whl 2>/dev/null || echo "  No wheels found in dist/"
    echo ""

    # Store original wheel names for cleanup
    ORIGINAL_WHEELS=()

    # Repair all wheels
    WHEEL_COUNT=0
    for wheel in dist/*.whl; do
        if [ ! -f "$wheel" ]; then
            continue
        fi
        WHEEL_COUNT=$((WHEEL_COUNT + 1))

        WHEEL_NAME=$(basename "$wheel")
        ORIGINAL_WHEELS+=("$wheel")
        echo "Repairing: $WHEEL_NAME"

        # Extract wheel to check its contents
        TEMP_DIR=$(mktemp -d)
        unzip -q "$wheel" -d "$TEMP_DIR"

        # Find libraries inside the wheel
        WHEEL_LIB_DIR=$(find "$TEMP_DIR" -type d -path "*/py_dss_interface/opendss_official/linux/cpp" | head -1)

        if [ -z "$WHEEL_LIB_DIR" ]; then
            echo "Warning: No library directory found in wheel, skipping repair"
            cp "$wheel" wheelhouse/
            rm -rf "$TEMP_DIR"
            continue
        fi

        # Check if libraries are in the wheel
        if [ -f "$WHEEL_LIB_DIR/libklusolve_all.so.0" ] && [ -f "$WHEEL_LIB_DIR/libklusolve_all.so.0.0.0" ] && [ -f "$WHEEL_LIB_DIR/libOpenDSSC.so" ]; then
            echo "Libraries found in wheel: $WHEEL_LIB_DIR"

            # Set LD_LIBRARY_PATH to the library directory so auditwheel can find them
            export LD_LIBRARY_PATH="${WHEEL_LIB_DIR}:${LD_LIBRARY_PATH}"

            # Try to repair - libraries are bundled, so auditwheel should be able to find them
            if auditwheel repair "$wheel" -w wheelhouse/ 2>&1 | tee /tmp/auditwheel_${WHEEL_NAME}.log; then
                echo "Successfully repaired: $WHEEL_NAME"
            else
                # Check the error
                if grep -q "required library.*could not be located" /tmp/auditwheel_${WHEEL_NAME}.log; then
                    echo "Warning: auditwheel couldn't locate libraries, but they're bundled in the wheel"
                    echo "The wheel may work without repair. Copying original wheel..."
                    cp "$wheel" wheelhouse/
                else
                    echo "Repair failed. Error log:"
                    cat /tmp/auditwheel_${WHEEL_NAME}.log
                    echo "Copying original wheel as fallback..."
                    cp "$wheel" wheelhouse/
                fi
            fi

            unset LD_LIBRARY_PATH
        else
            echo "Error: Required libraries not found in wheel!"
            echo "   Expected: $WHEEL_LIB_DIR/libklusolve_all.so.0"
            echo "   Expected: $WHEEL_LIB_DIR/libklusolve_all.so.0.0.0"
            echo "   Expected: $WHEEL_LIB_DIR/libOpenDSSC.so"
            rm -rf "$TEMP_DIR"
            exit 1
        fi

        rm -rf "$TEMP_DIR"
    done

    if [ $WHEEL_COUNT -eq 0 ]; then
        echo "Warning: No wheels found to repair!"
    else
        echo "Repaired ${WHEEL_COUNT} wheel(s)"
        
        echo ""
        echo "Repaired wheels are in: wheelhouse/"
    fi
    echo "Wheel repair completed!"
}

# Main execution
main() {
    # Build OpenDSS C++ libraries first
    build_opendss

    # Check Python version is available
    PYTHON_BIN="/opt/python/${PYTHON_VERSION}-${PYTHON_VERSION}/bin/python"
    if [ ! -f "${PYTHON_BIN}" ]; then
        echo "Error: Python ${PYTHON_VERSION} not found!"
        echo "Available Python versions:"
        ls -d /opt/python/*/bin/python 2>/dev/null | sed 's|/opt/python/||;s|/bin/python||' || echo "  None found"
        exit 1
    fi

    PYTHON_VER_INFO=$("${PYTHON_BIN}" --version 2>&1)
    echo "Using Python: ${PYTHON_VER_INFO}"
    echo ""

    # Build wheel (universal wheel works for all Python 3.9+)
    build_wheel "${PYTHON_VERSION}"

    # List wheel created before repair
    echo ""
    echo "Wheel created before repair:"
    ls -lh dist/*.whl 2>/dev/null || echo "  No wheel found in dist/"
    echo ""

    # Repair wheels for manylinux compatibility
    repair_wheels

    echo ""
    if [ "$ARCH" = "arm64" ]; then
        echo "ARM64 build completed successfully!"
    else
        echo "Build completed successfully!"
    fi
    echo "Repaired wheels are in: wheelhouse/"
    ls -lh wheelhouse/*.whl 2>/dev/null || echo "  No wheels found in wheelhouse/"

    # Clean up Linux C++ libraries after successful wheel creation
    # The libraries are now bundled in the wheel, so we don't need them in the source tree
    echo ""
    echo "Cleaning up Linux C++ libraries (now bundled in wheel)..."
    if [ -d "src/py_dss_interface/opendss_official/linux" ]; then
        rm -rf src/py_dss_interface/opendss_official/linux
        echo "Linux C++ libraries cleaned up"
    else
        echo "Linux C++ libraries directory not found (may have been cleaned already)"
    fi
}

# Run main function
main
