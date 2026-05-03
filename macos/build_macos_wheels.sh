#!/bin/bash
# Build native macOS arm64 wheels for py-dss-interface across Python 3.9-3.13.
#
# Mirrors linux/build_manylinux_wheels.sh but native; macOS arm64 wheels can't
# be cross-built from Linux, so this runs on the host. Calls
# OpenDSSMacOSCPPForRepo.sh first to stage the dylib, then loops once per
# installed CPython, building and delocating one wheel per interpreter.
#
# Output: wheelhouse/py_dss_interface-X.Y.Z-cp3X-cp3X-macosx_11_0_arm64.whl
#
# Prerequisites: cmake (brew install cmake), Xcode CLT, and either pyenv or
# Python.framework installs covering 3.9-3.13. Per Python version: build and
# delocate are pip-installed inside that interpreter as needed.
#
# Override architecture (advanced, not validated in CI):
#   ARCH=x86_64 bash macos/build_macos_wheels.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-11.0}"
ARCH="${ARCH:-arm64}"
PLAT_NAME="macosx_${MACOSX_DEPLOYMENT_TARGET//./_}_${ARCH}"

PYTHON_VERSIONS=(3.9 3.10 3.11 3.12 3.13)

resolve_python() {
    local ver="$1"
    # 1. pyenv shim
    if command -v pyenv >/dev/null 2>&1; then
        local prefix
        prefix="$(pyenv prefix "${ver}" 2>/dev/null || true)"
        if [[ -n "${prefix}" && -x "${prefix}/bin/python${ver}" ]]; then
            echo "${prefix}/bin/python${ver}"
            return 0
        fi
    fi
    # 2. python.org Framework install
    local fw="/Library/Frameworks/Python.framework/Versions/${ver}/bin/python${ver}"
    if [[ -x "${fw}" ]]; then
        echo "${fw}"
        return 0
    fi
    # 3. Homebrew or system on PATH
    if command -v "python${ver}" >/dev/null 2>&1; then
        echo "$(command -v "python${ver}")"
        return 0
    fi
    # 4. Fall back to plain python3 if its version matches.
    if command -v python3 >/dev/null 2>&1; then
        local actual
        actual="$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')"
        if [[ "${actual}" == "${ver}" ]]; then
            echo "$(command -v python3)"
            return 0
        fi
    fi
    return 1
}

cd "${REPO_ROOT}"

echo "==> Building libOpenDSSC.dylib (arch=${ARCH:-arm64})"
bash OpenDSSMacOSCPPForRepo.sh

mkdir -p dist wheelhouse
rm -f dist/*.whl wheelhouse/*.whl

built_any=false
for ver in "${PYTHON_VERSIONS[@]}"; do
    if py="$(resolve_python "${ver}")"; then
        echo
        echo "==> Building wheel for Python ${ver} (${py})"
        "${py}" -m pip install --quiet --upgrade pip build delocate
        # python -m build uses bdist_wheel under the hood; --plat-name and
        # --python-tag override the default 'py3-none-any' to produce a
        # cp3X-none-macosx_11_0_arm64 wheel. python-tag is derived from the
        # interpreter version so each loop iteration gets the right tag.
        py_tag="cp${ver//./}"
        "${py}" -m build --wheel --outdir dist \
            --config-setting="--build-option=--plat-name=${PLAT_NAME}" \
            --config-setting="--build-option=--python-tag=${py_tag}"
        built_any=true
    else
        echo "==> skip: python ${ver} not found on PATH, in pyenv, or under /Library/Frameworks"
    fi
done

if ! ${built_any}; then
    echo "ERROR: no Python interpreter resolved for any of: ${PYTHON_VERSIONS[*]}." >&2
    echo "       Install pyenv (recommended) or python.org Framework builds." >&2
    exit 1
fi

echo
echo "==> Delocating wheels into wheelhouse/"
shopt -s nullglob
for whl in dist/*macosx*arm64*.whl; do
    delocate-wheel -v -w wheelhouse "${whl}"
done
shopt -u nullglob

echo
echo "==> Wheels in wheelhouse/:"
ls -lh wheelhouse/
