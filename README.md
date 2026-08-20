# 🐍 py-dss-interface: Python Control for OpenDSS Powered by EPRI

![PyPI](https://img.shields.io/pypi/v/py-dss-interface)
![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux%20%7C%20macOS-brightgreen)
![License](https://img.shields.io/github/license/PauloRadatz/py_dss_interface)
![AppVeyor](https://ci.appveyor.com/api/projects/status/github/PauloRadatz/py_dss_interface?branch=master&svg=true)
![PyPI Downloads](https://static.pepy.tech/badge/py-dss-interface)

`py-dss-interface` is a Python package to control **OpenDSS Powered by [EPRI](https://www.epri.com/)** — **the official EPRI-maintained distribution of OpenDSS** — using the Python programming language. **It is designed to work with OpenDSS Powered by EPRI only**. This tool is actively used across multiple EPRI research projects and by utilities and consultants, and it also serves as a foundation for automated testing workflows for OpenDSS.

---

## 🚀 What Can You Do With `py-dss-interface`?

Use the full power of Python to control and automate your OpenDSS workflows:

- ⚙️ Automate repetitive OpenDSS tasks.
- 🧠 Manipulate circuit, element, and bus properties with Python logic.
- 📊 Extract simulation results and generate custom reports.
- 🧮 Implement advanced analysis and algorithms using Python.

---

## 🔄 Why Use `py-dss-interface` Instead of the COM Interface?

Compared to the COM interface, `py-dss-interface` provides a modern, Pythonic experience:

- 💡 **Code Completion**: Works seamlessly with IDEs like PyCharm for better developer productivity.
- 📦 **No OpenDSS Installation Required**: No need to install OpenDSS since `py-dss-interface` comes bundled with a tested version of OpenDSS (Windows, Linux, and macOS). However, you may also use a different OpenDSS version if desired. Please note that compatibility is only guaranteed for the bundled version.
- 🔄 **Supports Multiple DSS Objects**: Interact with multiple OpenDSS objects at the same time.
- 🌐 **Cross platform**: Runs on **Windows**, **Linux**, and **macOS** (Apple Silicon). Pre-built wheels are published for Windows and Linux; macOS users build from source today.
- ⚡ **Better Performance**: Some examples comparing performance will be provided later.
- 🔐 **Version Control Friendly**: Avoid breaking your Python code when updating the OpenDSS version on your computer.

---

## 💻 Platform Support

### ✅ Windows
```bash
pip install py-dss-interface
```

### 🐧 Linux

Pre-built wheels are available for x86_64 and ARM64. Start with pip install:

```bash
pip install py-dss-interface
```

If pip install doesn't work for your environment, you can build from source as described below.

#### 🔧 Build from source (fallback)

To use `py-dss-interface` on Linux, you'll need to build the OpenDSS C++ engine locally using `OpenDSSLinuxCPPForRepo.sh`.

1. **Clone the repository**
   ```bash
   git clone https://github.com/PauloRadatz/py_dss_interface.git
   cd py_dss_interface
   ```

2. **Build the OpenDSS C++ engine**
   ```bash
   bash py_dss_interface/OpenDSSLinuxCPPForRepo.sh
   ```
   📌 **Note:** You'll need tools for building C++ code (e.g., `g++`, `cmake`, etc.).

3. **Install the package**
   ```bash
   pip install path/to/py_dss_interface
   ```

### 🍎 macOS

No pre-built wheels are published for macOS. Build the OpenDSS C++ engine from source and install in editable mode.

1. **Clone the repository**
   ```bash
   git clone https://github.com/PauloRadatz/py_dss_interface.git
   cd py_dss_interface
   ```

2. **Build the OpenDSS C++ engine**
   ```bash
   bash OpenDSSMacOSCPPForRepo.sh
   ```
   📌 **Notes:**
   - Requires Xcode Command Line Tools and `cmake` (`brew install cmake`).
   - Builds for Apple Silicon (`arm64`) only. Intel users can edit `OpenDSSMacOSCPPForRepo.sh` and change `-DCMAKE_OSX_ARCHITECTURES=arm64` to `x86_64`.
   - The build script applies `macos/macos.patch` to a working copy of `VersionC.zip`; the bundled archive is left untouched.

3. **Install the package**
   ```bash
   pip install path/to/py_dss_interface
   ```

---

## 📦 Quickstart Example

```python
import py_dss_interface

dss = py_dss_interface.DSS()
dss.text("compile path/to/circuit.dss")
dss.text("solve")
print(dss.circuit.total_power)
```

---

## 🧪 Running Tests

The project includes a comprehensive test suite using pytest. Tests can be run individually or together depending on your platform and backend.

### Basic Test Execution

```bash
# Run all tests
pytest tests/
```

### Test Execution Modes

#### Automatic Individual Execution (Default)

On **Linux**, **macOS**, or when using the **C++ backend on Windows**, tests automatically run individually in separate subprocesses to prevent memory leak issues. The test framework handles this automatically.

#### Running All Tests Together

To run all tests together (faster execution, but may have memory issues if there are leaks), use the `--run-together` flag:

```bash
# Run all tests together (even on Linux/macOS/C++)
pytest tests/ --run-together
```

**Note:** Use `--run-together` when you want to test memory management fixes or need faster test execution. Individual execution is recommended for normal testing to prevent memory leaks.

### Test Summary

When tests run individually, a detailed summary is displayed at the end showing:
- Total tests executed
- Passed/Failed/Error counts
- List of failed tests (if any)
- Execution time

---

## 📖 Documentation

📖 **Full documentation available at:**
👉 [https://py-dss-interface.readthedocs.io/en/latest/](https://py-dss-interface.readthedocs.io/en/latest/)

---

## 🎓 Learn More

### 📘 Comprehensive Online Course

The best way to master `py-dss-interface` is through the official course:

👉 [Try the first modules for free](https://www.pauloradatz.me/course-py-dss-interface)

### 📺 YouTube Playlists

- 🔗 [Why Use Python with OpenDSS](https://www.youtube.com/watch?v=BIMcjZWpJek&list=PLhdRxvt3nJ8w36keL4uGBNbWs5SRxEyW0)
- 🔗 [py-dss-interface Version 2 Overview](https://www.youtube.com/watch?v=3KpQ_ORK3ew&list=PLhdRxvt3nJ8xURfBipVoAx8du1a-S5YsL)
- 🔗 [Version 1 Introduction](https://www.youtube.com/watch?v=QRnpLuMipFs&list=PLhdRxvt3nJ8zlzp6b_-7s3_YwwlunTNRC)

---

## 🧩 Tools Built on py-dss-interface

- 📈 [OpenDER_Interface](https://github.com/epri-dev/OpenDER_interface): EPRI's tool for Distributed Energy Resource simulation and control, powered by `py-dss-interface`.
  More on OpenDER: [GitHub](https://github.com/epri-dev/OpenDER)

- 📊 [2023 Hosting Capacity Webinar](https://epri.app.box.com/s/l1y0vyrj1dg3i0dadoseo97c9wj66pys): Examples shown using this package.

- 🔧 [py-dss-toolkit](https://github.com/PauloRadatz/py_dss_toolkit): A new package built on top of `py-dss-interface`, available in PyPI.

---

## 📂 Where to Find Examples

- 📁 [OpenDSS Repository](https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/Examples/Python/py-dss-interface/)
- 📁 Local Installation:
  After installing OpenDSS on your Windows computer, navigate to:
  `C:\\Program Files\\OpenDSS\\Examples\\Python\\py-dss-interface`

---

## 🤝 Community and Support

- 💬 Open an [Issue on GitHub](https://github.com/PauloRadatz/py_dss_interface/issues) for bugs or feature requests.
- 🙌 Contributions welcome! Please open a pull request with clear descriptions.
- 💻 Questions? Join discussions in the OpenDSS user forums or comment on relevant YouTube videos.

---

## 📚 How to Cite

If you use `py-dss-interface` in your academic work, please reference it as follows:

**APA Style:**

> Radatz, P. (2026). *py-dss-interface: A Python package that interfaces with OpenDSS powered by EPRI (Version 2.3.0)* [Computer software]. GitHub. https://github.com/PauloRadatz/py_dss_interface

**BibTeX Entry:**


```bibtex
@software{radatz2026pydssinterface,
  author = {Paulo Radatz},
  title = {py-dss-interface: A Python package that interfaces with OpenDSS powered by EPRI},
  year = {2026},
  version = {2.3.0},
  url = {https://github.com/PauloRadatz/py_dss_interface}
}
```
---

## 🙏 Acknowledgements

Developed and maintained by [Paulo Radatz](https://www.linkedin.com/in/pauloradatz/), with support from EPRI and the global OpenDSS community.

Special thanks to **Ênio Viana** and **Rodolfo Pilar Londero** for their contributions to the first version of this tool.

---
