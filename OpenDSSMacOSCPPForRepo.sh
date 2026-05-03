#!/bin/bash
# Build the OpenDSSC C++ engine as a macOS arm64 dylib and stage it for
# inclusion in the py-dss-interface package.
#
# Mirrors OpenDSSLinuxCPPForRepo.sh.  The OpenDSS C++ source ships as
# VersionC.zip at the repo root; this script extracts it, applies
# macos/macos.patch (Apple Clang / Apple Silicon portability fixes), runs
# CMake/Clang, and copies the resulting dylibs into
# src/py_dss_interface/opendss_official/macos/cpp/.
#
# Prerequisites: cmake, unzip, install_name_tool, otool (the latter two ship
# with Xcode Command Line Tools).  The script does not auto-install anything;
# missing tools are reported with a brew install hint.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_ZIP="${REPO_ROOT}/VersionC.zip"
PATCH_FILE="${REPO_ROOT}/macos/macos.patch"
WORK_DIR="${REPO_ROOT}/build_macos_src"
OUT_DIR="${REPO_ROOT}/src/py_dss_interface/opendss_official/macos/cpp"
ARCH="${ARCH:-arm64}"

require_tool() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "ERROR: required tool '$1' not found on PATH." >&2
        echo "       Install with: $2" >&2
        exit 1
    fi
}

echo "==> Checking build prerequisites"
require_tool cmake "brew install cmake"
require_tool unzip "(included with macOS)"
require_tool install_name_tool "xcode-select --install"
require_tool otool "xcode-select --install"

if [[ ! -f "${SRC_ZIP}" ]]; then
    echo "ERROR: ${SRC_ZIP} not found." >&2
    exit 1
fi
if [[ ! -f "${PATCH_FILE}" ]]; then
    echo "ERROR: ${PATCH_FILE} not found." >&2
    exit 1
fi

echo "==> Preparing working directory at ${WORK_DIR}"
rm -rf "${WORK_DIR}"
mkdir -p "${WORK_DIR}"
unzip -q "${SRC_ZIP}" -d "${WORK_DIR}"

echo "==> Applying ${PATCH_FILE}"
patch -p1 -d "${WORK_DIR}/VersionC" < "${PATCH_FILE}"

echo "==> Configuring CMake (${ARCH}, Release, SHARED)"
cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DMyOutputType:STRING=SHARED \
    -DCMAKE_OSX_ARCHITECTURES=${ARCH} \
    -S "${WORK_DIR}/VersionC" \
    -B "${WORK_DIR}/build"

echo "==> Building"
cmake --build "${WORK_DIR}/build" --config Release -j"$(sysctl -n hw.logicalcpu)"

echo "==> Staging dylibs into ${OUT_DIR}"
mkdir -p "${OUT_DIR}"
# Copy the main dylib and the klusolve dependency.  Preserve symlinks so the
# loader resolves @rpath/libklusolve_all.0.dylib without an extra fixup.
cp -P "${WORK_DIR}/build/libOpenDSSC.dylib" "${OUT_DIR}/"
cp -P "${WORK_DIR}/build/libklusolve_all"*.dylib "${OUT_DIR}/"

echo "==> Patching @rpath to @loader_path"
# CMake bakes the build directory into LC_RPATH; rewrite to @loader_path so the
# dylib finds its klusolve dependency next to itself once relocated into the
# package tree.
for dylib in "${OUT_DIR}/libOpenDSSC.dylib" "${OUT_DIR}"/libklusolve_all*.dylib; do
    [[ -L "${dylib}" ]] && continue
    chmod u+w "${dylib}"
    while read -r rpath; do
        [[ -n "${rpath}" ]] && install_name_tool -delete_rpath "${rpath}" "${dylib}" || true
    done < <(otool -l "${dylib}" | awk '/cmd LC_RPATH/{flag=1} flag && /path /{print $2; flag=0}')
    install_name_tool -add_rpath @loader_path "${dylib}"
done

echo
echo "==> Done."
echo "    Architecture: $(lipo -archs "${OUT_DIR}/libOpenDSSC.dylib")"
echo "    Dependencies:"
otool -L "${OUT_DIR}/libOpenDSSC.dylib" | sed 's/^/        /'
