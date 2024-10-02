# -*- coding: utf-8 -*-
# @Time    : 10/2/2024 11:41 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : build_delete.py
# @Software: PyCharm

import os
import subprocess
import sys
import platform

def build_opendss_linux():
    """
    Custom build step to compile OpenDSS for Linux during the pip install process.
    """

    # Ensure this is only executed on Linux systems
    if platform.system() != "Linux":
        print("Skipping OpenDSS build: Not on a Linux system.")
        return

    # Step 1: Clone the OpenDSS source code
    print("Cloning OpenDSS source code...")
    repo_url = "https://git.code.sf.net/p/electricdss/code"
    subprocess.run(["git", "clone", repo_url, "OpenDSSSource"], check=True)

    # Step 2: Navigate to the OpenDSS source directory
    os.chdir("OpenDSSSource/trunk/VersionC")

    # Step 3: Compile the OpenDSS shared library using make
    print("Building OpenDSS shared library...")
    subprocess.run(["make"], check=True)

    # Step 4: Locate the compiled shared library (.so file)
    shared_library = "libOpenDSSC.so"

    if not os.path.exists(shared_library):
        raise FileNotFoundError(f"Build failed. {shared_library} not found.")

    # Step 5: Determine the destination directory
    dest_dir = os.path.join(
        os.path.dirname(__file__),
        "src",
        "py_dss_interface",
        "opendss_official",
        "linux",
        "cpp",
    )
    os.makedirs(dest_dir, exist_ok=True)

    # Step 6: Copy the shared library to the correct destination
    print(f"Copying {shared_library} to {dest_dir}...")
    subprocess.run(["cp", shared_library, dest_dir], check=True)

    print(f"OpenDSS shared library built and placed in {dest_dir}")

# Entry point for the build script
if __name__ == "__main__":
    build_opendss_linux()
