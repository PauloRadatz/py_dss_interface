# Building the macOS arm64 dylib and wheels

## What this builds

The OpenDSS C++ engine as `libOpenDSSC.dylib` for Apple Silicon, plus optional
Python wheels for 3.9–3.13. Both come from `VersionC.zip` at the repo root via
`OpenDSSMacOSCPPForRepo.sh` and `macos/build_macos_wheels.sh`.

## Prerequisites

- macOS 11 (Big Sur) or newer on Apple Silicon.
- Xcode Command Line Tools: `xcode-select --install`. Provides `patch`,
  `install_name_tool`, `otool`, `unzip`, and Apple Clang.
- CMake: `brew install cmake`.
- For wheels only: `pyenv` (or system installs) covering Python 3.9–3.13, and
  `pip install build delocate` inside whichever venv runs the script.

## Build the dylib

```bash
bash OpenDSSMacOSCPPForRepo.sh
```

Output goes to `src/py_dss_interface/opendss_official/macos/cpp/`:

- `libOpenDSSC.dylib` — the engine, with `LC_RPATH` rewritten to
  `@loader_path` so it resolves `libklusolve_all.0.dylib` next to itself.
- `libklusolve_all.0.0.0.dylib` and the two compatibility symlinks.

Verify:

```bash
lipo -archs src/py_dss_interface/opendss_official/macos/cpp/libOpenDSSC.dylib
otool -l   src/py_dss_interface/opendss_official/macos/cpp/libOpenDSSC.dylib | grep -A2 LC_RPATH
```

The first should print `arm64`; the second `path @loader_path`.

## Build wheels

```bash
bash macos/build_macos_wheels.sh
```

Output goes to `wheelhouse/`. One wheel per Python version found on PATH,
tagged `cp3X-cp3X-macosx_11_0_arm64`. The script calls
`OpenDSSMacOSCPPForRepo.sh` first to stage dylibs, then loops `python -m
build --wheel` per interpreter and runs `delocate-wheel` to bundle the
klusolve dependency inside each wheel.

## What the patch does

`macos/macos.patch` is applied to an extracted working copy of `VersionC.zip`;
the bundled archive itself is never modified. The patch header (top of the
file) lists every change. Two-line summary: it adds an `elseif(APPLE)` branch
to `VersionC/CMakeLists.txt`, and it gates glibc-only headers and APIs
(`<linux/stat.h>`, `<linux/unistd.h>`, `<error.h>`, `__errno_location`,
`strerror_l`, `syscall(SYS_exit_group)`, `__LC_*`, `_NL_MONETARY_CRNCYSTR`,
`/proc/self/exe`, `xdg-open`, `SYS_getdents64`) behind `#ifndef __APPLE__`
with macOS equivalents.

## Troubleshooting

- `patch: command not found` — Xcode Command Line Tools not installed. Run
  `xcode-select --install`.
- `ld: library 'klusolve_all' not found` — dylibs were copied without
  symlinks. Re-run `OpenDSSMacOSCPPForRepo.sh`; it uses `cp -P` to preserve
  the symlink chain.
- `delocate-wheel: error` — `delocate` is not installed in the venv used to
  build. Run `pip install delocate` and rerun.
- The build hangs or produces an `x86_64` dylib on an Apple Silicon Mac —
  confirm `lipo -archs` reports `arm64`. CMake reads `CMAKE_OSX_ARCHITECTURES`
  from the script.

## Advanced: build for a different architecture

The build script reads `ARCH` from the environment (default `arm64`):

```bash
ARCH=x86_64 bash OpenDSSMacOSCPPForRepo.sh
```

`x86_64` is plumbed through the script but not validated in CI. Universal2
(`arm64;x86_64`) requires a CMake variable list and is not supported here.
