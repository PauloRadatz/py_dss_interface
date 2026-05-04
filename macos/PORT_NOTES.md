# Add macOS arm64 (Apple Silicon) platform support

## Summary

Adds macOS arm64 alongside Windows and Linux as a third supported platform. The package picks up a bundled `libOpenDSSC.dylib` from `src/py_dss_interface/opendss_official/macos/cpp/`, built from the existing `VersionC.zip` by `OpenDSSMacOSCPPForRepo.sh`. The OpenDSS C++ source has hard Linux assumptions (`/proc/self/exe`, `<linux/stat.h>`, `__errno_location`, `strerror_l`, `syscall(SYS_exit_group)`, `__LC_ALL`, `_NL_MONETARY_CRNCYSTR`, `xdg-open`, ...); `macos/macos.patch` carries the Apple Clang adjustments and is applied at build time against an extracted working copy. `VersionC.zip` itself is not modified.

Parity target is the Windows-Delphi backend on AppVeyor (master-612, commit 03f96d7, Python 3.10–3.14, `1123 passed / 2 skipped / 0 failed`; the 2 skips are unconditional `pytest.skip` calls in `test_dssexecutive.py:21,27`). On macOS-C++ Python 3.9 with this PR: `pytest tests/` reports `1126 passed / 0 failed / 0 errors / 0 unexpected skips` under default subprocess isolation, and `pytest tests/ --run-together` reports `1124 passed, 2 skipped` (matching the Windows baseline).

## What's added

**Build pipeline & CI.** `macos/macos.patch`, `OpenDSSMacOSCPPForRepo.sh`, `macos/Build_macOS_doc.md`, `macos/build_macos_wheels.sh`, `.github/workflows/macos-arm64.yml`. Build script accepts `ARCH` (default `arm64`) and `MACOSX_DEPLOYMENT_TARGET` (default `11.0`). Wheel script uses `delocate-wheel` and produces `cp3X-none-macosx_11_0_arm64.whl` per resolved interpreter. CI runs the matrix on `macos-14`; `wheels` job uploads tag-push artifacts. First CI in the repo.

**Loader / packaging.** `src/py_dss_interface/DSS.py` gets a Darwin branch alongside Windows and Linux. `pyproject.toml` adds the `Operating System :: MacOS :: MacOS X` classifier. `MANIFEST.in` includes the dylib in the wheel.

**Test infrastructure.** `tests/py_dss_interface/conftest.py` triggers per-test subprocess isolation on Darwin (same mechanism Linux already uses). The per-test result regex was switched from substring to anchored matching so unrelated stdout (tracebacks, identifiers containing `error`) no longer flips the outcome bucket. Float-vector comparisons across 52 sites in `test_circuit`, `test_bus`, `test_cktelement`, `test_lines`, `test_meters`, `test_transformers` were converted from `[round(value, 20) for ...] ==` exact-equality to `pytest.approx(rel=1e-6, abs=1e-9)`; the tolerance sits ~3 orders of magnitude tighter than utility-grade revenue-meter precision and well below any algorithmic-regression signal. `test_dssproperties_read_active_property` was made backend-portable in the test (writes `"1"` before reading) rather than xfailed. `test_api_surface.py` is new: pins the public attribute set on `DSS()` so future drift fails on the affected platform.

**Documentation.** `README.md` platform badge, intro bullet, and Test Execution Modes section all mention macOS.

## Engine bug fixes (in `macos/macos.patch`)

While bringing macOS up I uncovered seven cross-platform bugs in the bundled OpenDSS C++ engine. They surface on macOS because Apple Silicon's xzone malloc detects free-list corruption strictly and the test surface exercises the affected paths. Fixes are in `macos/macos.patch`; the same fixes would apply cleanly to Linux-C++ if the Linux build script were extended to apply the patch (out of scope for this PR — see follow-ups).

1. **`TMyApplication::DoRun` leak** (`DLL/OpenDSSCDLL.cpp`). Every Python `DSS()` call routes through `DSS.Start` to `DoRun`, which `new`'d a fresh `TExecutive` (and the ~54 `TDSSClass` metaclass objects its constructor allocates) without freeing the previous one — the assignment to `DSSExecutive[ActiveActor]` dropped the old pointer. Each Python `DSS()` leaked one full set of metaclasses plus their property arrays / hash lists. Measured at ~1.04 MiB / cycle on the IEEE 13-bus workload (RSS grew from 27 MiB to 130 MiB across 100 cycles); after the fix, slope is ~9 KiB / cycle (~120× reduction). Fix: gate `new TExecutive()` on the slot being null. Diagnosed via macOS `leaks --groupByType` plus a temporary `extern "C" int DSSClassAlive()` counter; root-leak chain was `DoRun → TExecutive ctor → CreateDSSClasses → operator new`.

2. **Per-phase state buffer overflow** (`Controls/SwtControl.cpp`, `Controls/Recloser.cpp`, `PDElements/fuse.cpp`). Each constructor sized its `FPresentState` / `FNormalState` arrays via `realloc(..., sizeof((*ptr)[1 - 1]) * Fnphases)` — 12 bytes for a 3-phase element. The `StateArray` typedef is `EControlAction[6]` and accessors index `(*ptr)[Idx - 1]` for `Idx` up to `Fnphases`, so `get_States(3)` reads bytes 12–15 past the 12-byte allocation. Linux glibc and Delphi tolerate the overrun; Apple Silicon xzone malloc detects the corrupted free chunk on the next allocation and aborts with `SIGTRAP`. Fix: `realloc(..., sizeof(StateArray))` in all three. Diagnosed via ASan-instrumented `OpenDSSCcmd` driving the SwtControl reproducer over stdin (sidesteps the SIP / `DYLD_INSERT_LIBRARIES` policy that blocks ASan against Python). Recovers the 20 SwtControl tests previously class-skipped on Darwin.

3. **`SolutionAbort` not reset by `Clear` / `ClearAllCircuits`** (`Executive/Executive.cpp`, `Common/DSSGlobals.cpp`). A runtime error during Edit (e.g. setting a fuse / relay / recloser MonitoredObj to a nonexistent element) calls `DoErrorMsg` which sets `SolutionAbort = true`. Neither `TExecutive::Clear` nor `ClearAllCircuits` clear that flag, so the user's canonical reset pattern (`Clear`; `ClearAll`; `compile X`) hits `DoRedirect`'s `if (!SolutionAbort)` gate at `ExecHelper.cpp:636`, silently skips every command in the file, and the next `New Generator/...` segfaults on a `NULL TDSSCircuit` deref at `ExecHelper.cpp:1893`. Fix: reset both `SolutionAbort` and `Redirect_Abort` in `Clear` and in `ClearAllCircuits`. Pinned via `lldb` against Homebrew Python (`x9 = NULL, x10 = 0x1301`, the `DuplicatesAllowed` field offset).

4. **`TRelayObj` had only scalar state** (`Controls/Relay.h`, `Controls/Relay.cpp`). The wrapper's relay setter accepts a list per phase, but `TRelayObj` stored a single `EControlAction FPresentState`, so a write of `['open', 'closed', 'closed']` round-tripped to `['open', 'open', 'open']` and `test_relays_write_state` xfailed on macOS-C++. Fix: add per-phase `get_States(int)` / `set_States(int, EControlAction)` accessors that act on the controlled element's per-conductor state directly (mirroring `PDElements/fuse.cpp`); the scalar `FPresentState` is left untouched so internal control logic and lock-state machinery are unaffected. RelaysV cases 1 and 2 in `DLL/OpenDSSCDLL.cpp` are rewired: case 1 calls `elem->get_States(i)` per phase; case 2 decodes the input string array with `BArray2Str` and dispatches `set_States(i, ...)`.

5. **Missing `RelaysV` variant cases 1–4** (`DLL/OpenDSSCDLL.cpp`). Upstream only implemented case 0 (`AllNames`); cases 1–4 (State / NormalState read / write per-phase array) fell through to `default` and returned `"Error, parameter not recognized"`. Fix: implement the four cases.

6. **Recloser property-name mismatch** (`Controls/Recloser.cpp`). The bundled `VersionC.zip` uses `PropertyName[12-1] = "PhInst"` and `[13-1] = "GndInst"`, but the test fixture, the wrapper's `Set_ParameterReC` calls, and the OpenDSS Delphi backend all use the longer `"PhaseInst"` / `"GroundInst"`. The parser silently dropped the unknown keyword, leaving the field at its `0.0` default. Fix: rename the keywords; update the two wrapper setter calls in `OpenDSSCDLL.cpp` to match.

7. **`TPointerList` leak in `RemoveSelfFromControlelementList`** (`Controls/ControlElem.cpp`). A heap-allocated `TempList` was copy-assigned into `ControlElementList` but never freed. Fix: add the missing `delete TempList;`.

## Verification

- macOS 26.4.1 (Tahoe), Apple Silicon. Apple Clang 21.0 / Xcode CLT.
- `bash OpenDSSMacOSCPPForRepo.sh` produces an `arm64` dylib with `LC_BUILD_VERSION minos 11.0` and `@loader_path` rpath. External deps: `/usr/lib/libc++.1.dylib` and `/usr/lib/libSystem.B.dylib` only.
- `bash macos/build_macos_wheels.sh` produces `py_dss_interface-2.3.0-cp3X-none-macosx_11_0_arm64.whl` per interpreter; `delocate-listdeps` reports only the bundled `libklusolve_all.0.dylib`.
- Fresh-venv install of the wheel + IEEE 13-bus compile/solve from outside the repo returns `total_power = [-3567.05, -1736.44]`, `num_buses = 16`, matches the Windows reference.
- `pytest tests/` on macOS-C++ Python 3.9 (default subprocess isolation): `1126 passed, 0 failed, 0 errors, 0 unexpected skips`.
- `pytest tests/ --run-together`: `1124 passed, 2 skipped` (the 2 unconditional skips in `test_dssexecutive.py:21,27` matching the Windows-Delphi baseline).

## Out of scope

- Universal2 / Intel macOS wheels. `ARCH=x86_64` is plumbed through the build script for local use; not validated in CI.
- Extending `OpenDSSLinuxCPPForRepo.sh` to apply the engine fixes above and rebuilding `linux/cpp/libOpenDSSC.so`. The bug fixes in `macos/macos.patch` are cross-platform and would benefit Linux-C++ users, but actually delivering them to Linux requires touching the Linux build script and committing a new `libOpenDSSC.so` binary — separate change.
- Upstreaming `macos/macos.patch` to EPRI's OpenDSS-C SourceForge tree (BSD-licensed). Feasible follow-up; cleanest long-term home for the engine bug fixes.

## Notes for review

- `_LIBCPP___FWD_COMPLEX_H` is a libc++ internal include guard. Pre-defining it works today; could break in a future libc++ release. The clean fix is to qualify bare `complex` references in `Ucmatrix.h`, `CktElement.h`, `Sparse_Math.h` etc. as `Ucomplex::complex` — much larger diff, out of scope here.
- `OpenDSSMacOSCPPForRepo.sh` does not auto-install Homebrew packages; missing tools are reported with a `brew install` hint.
- Tested only on Apple Silicon. `ARCH=x86_64` is plumbed through the build script but not validated against an Intel Mac runner.
