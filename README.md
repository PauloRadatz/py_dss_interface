# 🐍 py-dss-interface: Python Control for OpenDSS Powered by EPRI

![PyPI](https://img.shields.io/pypi/v/py-dss-interface)
![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux-brightgreen)
![License](https://img.shields.io/github/license/PauloRadatz/py_dss_interface)
![AppVeyor](https://ci.appveyor.com/api/projects/status/github/PauloRadatz/py_dss_interface?branch=master&svg=true)

`py-dss-interface` is a Python package endorsed by [EPRI](https://www.epri.com/) to control **OpenDSS Powered by EPRI** using the Python programming language. This tool is actively used across multiple EPRI research projects and by many utilities and consultants. It also serves as the foundation for the automated testing process of OpenDSS itself.


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
- 📦 **No OpenDSS Installation Required (Windows)**: No need to install OpenDSS since `py-dss-interface` comes bundled with a tested version of OpenDSS. However, you may also use a different OpenDSS version if desired. Please note that compatibility is only guaranteed for the bundled version.
- 🔄 **Supports Multiple DSS Objects**: Interact with multiple OpenDSS objects at the same time.
- 🌐 **Multi-Platform**: Supports both **Windows** and now **Linux** (via local OpenDSS C++ build).
- ⚡ **Better Performance**: Some examples comparing performance will be provided later.
- 🔐 **Version Control Friendly (Windows)**: Avoid breaking your Python code when updating the OpenDSS version on your computer.

---

## 💻 Platform Support

### ✅ Windows
```bash
pip install py-dss-interface
```

### 🐧 Linux (OpenDSS C++ Build Required)

To use `py-dss-interface` on Linux, you'll need to build the OpenDSS C++ engine locally.

#### 🔧 Step 1: Clone the repository

```bash
git clone https://github.com/PauloRadatz/py_dss_interface.git
cd py_dss_interface
```

> This will create a folder named `py_dss_interface` — you’ll use this path later when installing the package.

#### ⚙️ Step 2: Build the OpenDSS C++ engine

```bash
bash py_dss_interface/OpenDSSLinuxCPPForRepo.sh
```

📌 **Note:** You'll need tools for building C++ code on your system (e.g., `g++`, `cmake`, etc.).

#### 📦 Step 3: Install the package in your Python environment

If you're using `py-dss-interface` in your own Python project or script, install it like this:

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

- 🔧 [py-dss-tools](https://github.com/PauloRadatz/py_dss_tools): A new package built on top of `py-dss-interface`, coming soon to PyPI.

---

## 📂 Where to Find Examples

- 📁 [OpenDSS Repository]()
- 📁 Local Installation:
  After installing OpenDSS in your Windows computer, navigate to:
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

> Radatz, P. (2025). *py-dss-interface: A Python package that interfaces with OpenDSS powered by EPRI (Version X.X.X)* [Computer software]. GitHub. https://github.com/PauloRadatz/py_dss_interface

**BibTeX Entry:**


```bibtex
@software{radatz2024bdgd2opendss,
  author = {Paulo Radatz},
  title = {py-dss-interface: A Python package that interfaces with OpenDSS powered by EPRI},
  year = {2025},
  version = {X.X.X},
  url = {https://github.com/PauloRadatz/py_dss_interface}
}
```

> 📌 Please replace X.X.X with the version of the package you are using.

---

## 🙏 Acknowledgements

Developed and maintained by [Paulo Radatz](https://www.linkedin.com/in/pauloradatz/), with support from EPRI and the global OpenDSS community.

Special thanks to **Ênio Viana** and **Rodolfo Pilar Londero** for their contributions to the first version of this tool.

---
