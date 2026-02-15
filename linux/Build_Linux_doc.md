# Complete Build and Test Process - Step by Step Documentation

This document explains the complete process of building manylinux wheels for **both x86_64 and ARM64 architectures** and testing them, with PowerShell commands you can run manually.

## Overview

The process consists of building wheels for two architectures:
1. **x86_64** - For Intel/AMD 64-bit Linux systems
2. **ARM64 (aarch64)** - For ARM64 Linux systems (Apple Silicon Macs, ARM servers, RHEL9 ARM64)

The process includes:
1. **Build manylinux wheels** - Compile OpenDSS C++ and create Python wheels (Required)
2. **Test basic functionality** - Verify wheel can be installed and imported (Recommended)
3. **Run pytest tests** - Run full test suite individually (Optional, but recommended before PyPI upload)
4. **Summary** - Review results and prepare for PyPI upload

---

## Prerequisites

### 1. Check Docker

```powershell
# Check if Docker is in PATH
docker --version

# If not found, try full path
& "C:\Program Files\Docker\Docker\resources\bin\docker.exe" --version

# Check if Docker daemon is running
docker ps
```

**Expected output:**
```
Docker version 29.2.0, build 0b9d198
CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS    PORTS     NAMES
```

### 2. Check VersionC.zip

```powershell
Test-Path "VersionC.zip"
```

**Expected output:** `True`

---

## Part 1: Building x86_64 Wheels

### What This Does:
1. Pulls the manylinux2014_x86_64 Docker image (CentOS 7 based)
2. Builds OpenDSS C++ libraries for x86_64 inside Docker
3. Creates Python wheels for Python 3.9-3.14
4. Repairs wheels with `auditwheel` for manylinux compatibility

### Commands:

```powershell
# Set project directory
$projectDir = (Get-Location).Path

# Fix line endings in build script (Windows CRLF to Unix LF)
$scriptPath = Join-Path $projectDir "linux\build_manylinux_wheels.sh"
$content = Get-Content $scriptPath -Raw
$content = $content -replace "`r`n", "`n"
[System.IO.File]::WriteAllText($scriptPath, $content, [System.Text.Encoding]::UTF8)

# Pull Docker image (first time only, ~500MB download)
docker pull quay.io/pypa/manylinux2014_x86_64

# Build wheels (takes 10-30 minutes)
docker run --rm `
    -v "${projectDir}:/io" `
    quay.io/pypa/manylinux2014_x86_64 `
    /bin/bash -c "cd /io && tr -d '\r' < linux/build_manylinux_wheels.sh > /tmp/build_script.sh && chmod +x /tmp/build_script.sh && ARCH=x86_64 /tmp/build_script.sh"
```

### What Happens Inside Docker:

1. **Install build dependencies:**
   ```bash
   yum install -y cmake gcc-c++ make unzip libuuid-devel
   ```

2. **Build OpenDSS C++:**
   ```bash
   unzip VersionC.zip
   cmake -DCMAKE_BUILD_TYPE=Release -DMyOutputType:STRING=SHARED -S ./VersionC -B ./buildlinux
   cmake --build ./buildlinux -j $(nproc)
   cp -r buildlinux/* src/py_dss_interface/opendss_official/linux/cpp/
   ```

3. **Build Python wheel:**
   ```bash
   /opt/python/cp312-cp312/bin/python -m build --wheel
   ```

4. **Repair wheels:**
   ```bash
   auditwheel repair dist/*.whl -w wheelhouse/
   # Repaired wheels are saved in wheelhouse/ (dist/ is left unchanged)
   ```

### Check Results:

```powershell
# List built wheels
Get-ChildItem wheelhouse\*x86_64*.whl

# Expected output:
# py_dss_interface-2.3.0-py3-none-manylinux2014_x86_64.manylinux_2_17_x86_64.whl
```

---

## Part 2: Building ARM64 Wheels

### What This Does:
1. Pulls the manylinux2014_aarch64 Docker image (CentOS 7 ARM64 based)
2. Builds OpenDSS C++ libraries for ARM64 inside Docker
3. Creates Python wheels for Python 3.9-3.14
4. Repairs wheels with `auditwheel` for manylinux compatibility

### Commands:

```powershell
# Set project directory
$projectDir = (Get-Location).Path

# Fix line endings in build script (Windows CRLF to Unix LF)
$scriptPath = Join-Path $projectDir "linux\build_manylinux_wheels.sh"
$content = Get-Content $scriptPath -Raw
$content = $content -replace "`r`n", "`n"
[System.IO.File]::WriteAllText($scriptPath, $content, [System.Text.Encoding]::UTF8)

# Pull Docker image (first time only, ~500MB download)
docker pull quay.io/pypa/manylinux2014_aarch64

# Build ARM64 wheels (takes 10-30 minutes, may be slower with emulation)
docker run --rm `
    --platform linux/arm64 `
    -v "${projectDir}:/io" `
    quay.io/pypa/manylinux2014_aarch64 `
    /bin/bash -c "cd /io && tr -d '\r' < linux/build_manylinux_wheels.sh > /tmp/build_script.sh && chmod +x /tmp/build_script.sh && ARCH=arm64 /tmp/build_script.sh"
```

### What Happens Inside Docker:

Same process as x86_64, but:
- Uses ARM64 Docker image (`manylinux2014_aarch64`)
- Compiles OpenDSS C++ for ARM64 architecture
- Creates wheels tagged with `manylinux2014_aarch64`

### Check Results:

```powershell
# List built ARM64 wheels
Get-ChildItem wheelhouse\*aarch64*.whl

# Expected output:
# py_dss_interface-2.3.0-py3-none-any-manylinux_2_17_aarch64.manylinux2014_aarch64.whl
```

---

## Part 3: Testing Wheels

### Step 1: Test Basic Wheel Functionality (x86_64)

```powershell
$projectDir = (Get-Location).Path

# Create test script
$testScript = @'
#!/bin/bash
set -e
apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3 python3-pip python3-venv > /dev/null
python3 -m venv test_env
source test_env/bin/activate
pip install --quiet --upgrade pip
pip install /wheels/*x86_64*.whl
python3 -c "import py_dss_interface; print('Import successful')"
python3 -c "import py_dss_interface; dss = py_dss_interface.DSS(); print('DSS object created'); print('Backend:', dss.backend)"
deactivate
rm -rf test_env
'@

$testScript | Out-File -FilePath "test_basic_temp.sh" -Encoding ASCII -NoNewline

# Run test (mount project dir - Docker on Windows creates dirs when mounting single files)
docker run --rm `
    -v "${projectDir}/wheelhouse:/wheels" `
    -v "${projectDir}:/project" `
    ubuntu:22.04 `
    bash -c "tr -d '\r' < /project/test_basic_temp.sh > /tmp/test_script_fixed.sh && chmod +x /tmp/test_script_fixed.sh && bash /tmp/test_script_fixed.sh"

# Cleanup
Remove-Item "test_basic_temp.sh"
```

**Expected output:**
```
Import successful
DSS object created
Backend: Linux-C++
```

### Step 2: Test Basic Wheel Functionality (ARM64)

**Note:** ARM64 testing requires ARM64 emulation or an ARM64 machine. For local testing, you may need to:
- Use an actual ARM64 machine
- Skip ARM64 testing locally if emulation is not available

If you have ARM64 emulation:

```powershell
$projectDir = (Get-Location).Path

# Create test script
$testScript = @'
#!/bin/bash
set -e
apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3 python3-pip python3-venv > /dev/null
python3 -m venv test_env
source test_env/bin/activate
pip install --quiet --upgrade pip
pip install /wheels/*aarch64*.whl
python3 -c "import py_dss_interface; print('Import successful')"
python3 -c "import py_dss_interface; dss = py_dss_interface.DSS(); print('DSS object created'); print('Backend:', dss.backend)"
deactivate
rm -rf test_env
'@

$testScript | Out-File -FilePath "test_basic_temp_arm64.sh" -Encoding ASCII -NoNewline

# Run test (requires ARM64 emulation; mount project dir - Docker on Windows creates dirs when mounting single files)
docker run --rm `
    --platform linux/arm64 `
    -v "${projectDir}/wheelhouse:/wheels" `
    -v "${projectDir}:/project" `
    ubuntu:22.04 `
    bash -c "tr -d '\r' < /project/test_basic_temp_arm64.sh > /tmp/test_script_fixed.sh && chmod +x /tmp/test_script_fixed.sh && bash /tmp/test_script_fixed.sh"

# Cleanup
Remove-Item "test_basic_temp_arm64.sh"
```

### Step 3: Run Pytest Tests (x86_64) - Optional

**Note:** This step is optional but recommended to verify the wheel works correctly before uploading to PyPI. You can skip this if you're confident the wheel is correct or if you want to test later.

```powershell
$projectDir = (Get-Location).Path

# Create pytest test script
$pytestScript = @'
#!/bin/bash
set -e
apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3 python3-pip python3-venv > /dev/null
python3 -m venv test_env
source test_env/bin/activate
pip install --quiet --upgrade pip
pip install /wheels/*x86_64*.whl
pip install --quiet pytest pytest-cov
mkdir -p /test_runner
cp -r /tests /test_runner/
cp -r /test_cases /test_runner/tests/py_dss_interface/ 2>/dev/null || true
if [ -f /project/pytest.ini ]; then cp /project/pytest.ini /test_runner/pytest.ini; fi
cd /test_runner
pytest tests/ -v --tb=short
deactivate
rm -rf test_env /test_runner
'@

$pytestScript | Out-File -FilePath "test_pytest_temp.sh" -Encoding ASCII -NoNewline

# Run pytest tests (takes 10-30 minutes)
docker run --rm `
    -v "${projectDir}/wheelhouse:/wheels" `
    -v "${projectDir}/tests:/tests" `
    -v "${projectDir}/tests/py_dss_interface/cases:/test_cases" `
    -v "${projectDir}:/project" `
    ubuntu:22.04 `
    bash -c "tr -d '\r' < /project/test_pytest_temp.sh > /tmp/test_script_fixed.sh && chmod +x /tmp/test_script_fixed.sh && bash /tmp/test_script_fixed.sh"

# Cleanup
Remove-Item "test_pytest_temp.sh"
```

**Expected output:**
```
test_activeclass.py::TestActiveClass13Bus::test_next [1%] PASSED
test_activeclass.py::TestActiveClass13Bus::test_num_elements [2%] PASSED
...
======================================== test session starts =========================================
1125 passed in XXX.XXs
```

---

## Part 4: Summary and Next Steps

### Check Built Wheels:

```powershell
# List all wheels (both architectures)
Get-ChildItem wheelhouse\*.whl | Format-Table Name, Length, LastWriteTime

# Expected output:
# py_dss_interface-2.3.0-py3-none-manylinux2014_x86_64.manylinux_2_17_x86_64.whl
# py_dss_interface-2.3.0-py3-none-manylinux2014_aarch64.manylinux_2_17_aarch64.whl
```

### Upload to PyPI:

```powershell
# Install twine if needed
pip install twine

# Test on TestPyPI first (recommended)
twine upload --repository testpypi wheelhouse\*.whl

# Then upload to PyPI (both architectures)
twine upload wheelhouse\*.whl
```

### Verify on PyPI:
- https://pypi.org/project/py-dss-interface/
- Check that both x86_64 and ARM64 wheels are available

---

## Quick Reference: Complete Process

### Option 1: Use PowerShell Commands (Recommended)

**For x86_64:**
Use the PowerShell commands from "Part 1: Building x86_64 Wheels" above.

**For ARM64:**
Use the PowerShell commands from "Part 2: Building ARM64 Wheels" above.

---

## Understanding the Process

### Why Two Architectures?

- **x86_64**: For Intel/AMD 64-bit Linux systems (most common)
- **ARM64**: For ARM64 Linux systems:
  - Apple Silicon Macs running Linux
  - ARM servers (AWS Graviton, Azure Ampere)
  - RHEL9 ARM64
  - Raspberry Pi 4+ (64-bit)

### Why Docker?

- **Isolation**: Build environment is consistent and reproducible
- **Manylinux**: Uses CentOS 7 base (manylinux2014) for maximum compatibility
- **Cross-platform**: Build Linux wheels on Windows
- **Architecture support**: Can build for different architectures

### Why Manylinux?

- **Compatibility**: Wheels work on most Linux distributions
- **No compilation**: Users don't need to build from source
- **Standard**: Follows PEP 513/571/600 standards

### Why Individual Tests?

- **Memory management**: Each test runs in isolation
- **Debugging**: Easier to identify which test fails
- **Progress**: Can see percentage completion

### File Locations:

- **Wheels**: `wheelhouse/` (repaired wheels; `dist/` keeps intermediate unrepaired wheels)
- **x86_64 wheels**: `*manylinux2014_x86_64.whl`
- **ARM64 wheels**: `*manylinux2014_aarch64.whl`
- **Build script**:
  - `linux/build_manylinux_wheels.sh` (unified script, supports both architectures via ARCH parameter)
- **Test scripts**: Created temporarily, deleted after use
- **OpenDSS libraries**: `src/py_dss_interface/opendss_official/linux/cpp/`

---

## Time Estimates

### x86_64 Build:
- **Step 1 (Build)**: 10-30 minutes (Required)
- **Step 2 (Basic Test)**: 1-2 minutes (Recommended)
- **Step 3 (Pytest)**: 10-30 minutes (Optional)
- **Total (Build only)**: 10-30 minutes
- **Total (with tests)**: 20-60 minutes

### ARM64 Build:
- **Step 1 (Build)**: 10-30 minutes (Required, longer with emulation, faster on native ARM64)
- **Step 2 (Basic Test)**: 1-2 minutes (Recommended, if emulation available)
- **Step 3 (Pytest)**: 10-30 minutes (Optional, if emulation available)
- **Total (Build only)**: 10-30 minutes
- **Total (with tests)**: 20-60 minutes (faster on native ARM64 hardware)

### Combined:
- **Both architectures**: 40-120 minutes locally

---

## Troubleshooting

### Docker Not Found
```powershell
# Check if Docker Desktop is running
Get-Process "Docker Desktop" -ErrorAction SilentlyContinue

# If not running, start it
Start-Process "C:\Program Files\Docker\Docker\Docker Desktop.exe"

# Wait 1-2 minutes, then check again
docker ps
```

### Build Fails
- Check `VersionC.zip` exists
- Check Docker has enough disk space
- Check Docker logs: `docker logs <container_id>`

### ARM64 Build Fails / "ARM64 emulation is not available" Warning

**Why did it work before and not now?**

Two common causes:
1. **PowerShell exit code bug:** The test used `| Out-Null` which breaks `$LASTEXITCODE`. We fixed this (use `$null = docker run ...` instead). Re-run the test.
2. **Docker Desktop containerd setting:** After a Docker update or reinstall, "Use containerd for pulling and storing images" may be disabled. Upgrades from older versions don't auto-enable it.

**Error: "exec format error" or "exec /usr/local/bin/manylinux-entrypoint: exec format error"**

This means ARM64 emulation is not working in Docker Desktop. Try:

1. **Enable ARM64 emulation in Docker Desktop:**
   - Open Docker Desktop
   - Go to Settings → General (or "Features in development" in older versions)
   - Enable "Use containerd for pulling and storing images"
   - Click Apply & Restart
   - Test manually: `docker run --rm --platform linux/arm64 alpine:latest uname -m`
   - Should output: `aarch64`

2. **Check Docker Desktop version:**
   - Update to latest Docker Desktop (ARM64 emulation requires recent version)
   - Check: `docker --version` (should be 4.0+)

3. **Force ARM64 build (ignore the test):** If you believe emulation works, run the ARM64 commands directly (see Part 2) without the `if ($arm64EmulationAvailable)` block. The test can have false negatives.

4. **Alternative solutions:**
   - Build on an actual ARM64 machine (Apple Silicon Mac, ARM server)
   - Use WSL2 with ARM64 support (if available)
   - Skip ARM64 builds locally and build only x86_64

**Other ARM64 issues:**
- **Image pull fails**: Check Docker Desktop supports ARM64 emulation
- **Build slow**: Normal with emulation, use native ARM64 hardware for faster builds
- **Emulation not available**: Use an actual ARM64 machine for faster builds

### Tests Fail
- Check wheel was built successfully
- Check test files are accessible
- Run individual test: `pytest tests/py_dss_interface/test_activeclass.py -v`

### Line Ending Issues
```powershell
# Fix all .sh files
Get-ChildItem *.sh | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $content = $content -replace "`r`n", "`n"
    [System.IO.File]::WriteAllText($_.FullName, $content, [System.Text.Encoding]::UTF8)
}
```

---

## Next Steps After Success

1. ✅ Wheels built and tested (both architectures)
2. 📤 Upload to TestPyPI (test first):
   ```powershell
   twine upload --repository testpypi wheelhouse\*.whl
   ```
3. 📤 Upload to PyPI (production):
   ```powershell
   twine upload wheelhouse\*.whl
   ```
4. ✅ Verify installation works:
   - On x86_64 Linux: `pip install py-dss-interface`
   - On ARM64 Linux: `pip install py-dss-interface`
5. 📝 Update documentation if needed

---

## Architecture Comparison

| Feature | x86_64 | ARM64 |
|---------|--------|-------|
| **Docker Image** | `manylinux2014_x86_64` | `manylinux2014_aarch64` |
| **Platform Tag** | `manylinux2014_x86_64` | `manylinux2014_aarch64` |
| **Build Script** | `linux/build_manylinux_wheels.sh` (with `ARCH=x86_64`) | `linux/build_manylinux_wheels.sh` (with `ARCH=arm64`) |
| **ARCH Parameter** | `ARCH=x86_64` (or default) | `ARCH=arm64` or `ARCH=aarch64` |
| **Local Build** | ✅ Fast (native) | ⚠️ Slow (emulation), ✅ Fast (native ARM64) |
| **Use Cases** | Intel/AMD servers | Apple Silicon, ARM servers |

---

## Related Files

- `linux/build_manylinux_wheels.sh` - Unified build script (runs in Docker, supports both architectures via ARCH parameter)
- `linux/Build_Linux_doc.md` - This file (complete process for both architectures)

---

## Summary Checklist

### Before Building:
- [ ] Docker Desktop installed and running
- [ ] VersionC.zip in project root
- [ ] Project directory accessible

### Build x86_64:
- [ ] Use PowerShell commands from "Part 1: Building x86_64 Wheels" above (Required)
- [ ] Verify wheel created: `*manylinux2014_x86_64.whl`
- [ ] Test basic functionality (Recommended)
- [ ] Run pytest tests (Optional, but recommended before PyPI upload)

### Build ARM64:
- [ ] Use PowerShell commands from "Part 2: Building ARM64 Wheels" above (Required)
- [ ] Verify wheel created: `*manylinux2014_aarch64.whl`
- [ ] Test basic functionality (Recommended, if emulation available or on native ARM64)
- [ ] Run pytest tests (Optional, but recommended before PyPI upload)

### Upload:
- [ ] Test on TestPyPI first
- [ ] Upload both architectures to PyPI
- [ ] Verify both wheels available on PyPI
- [ ] Test installation on both architectures

---

**You now have complete support for both x86_64 and ARM64 Linux systems!** 🎉
