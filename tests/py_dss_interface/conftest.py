# -*- coding: utf-8 -*-
import gc
import os
import pathlib
import pytest

import py_dss_interface
from py_dss_interface.utils.System import System

script_path = os.path.dirname(os.path.abspath(__file__))


@pytest.fixture(scope='function')
def solve_snap_13bus():
    """
    Fixture that creates a DSS instance for testing.
    Uses single_instance=False for C++ backends (Linux and Windows C++)
    to isolate memory workspace between tests.
    """
    # Detect platform and environment variables
    use_cpp = (
        os.environ.get('PY_DSS_INTERFACE_CPP', '').lower() == 'true' or
        System.detect_platform() == 'Linux'
    )

    if use_cpp:
        dss = py_dss_interface.DSS(windows_version="cpp", single_instance=False)
    else:
        # Delphi on Windows: single instance is safe and fast
        dss = py_dss_interface.DSS()

    dss.text("Clear")
    dss.text("ClearAll")
    dss.text("set DefaultBaseFrequency=60")
    dss.text("Set EventLogDefault=yes")
    dss13_path = pathlib.Path(script_path).joinpath("cases", "13Bus", "IEEE13Nodeckt.dss")
    dss.text(f"compile [{dss13_path}]")

    dss.dssinterface.allow_forms = 0

    yield dss

    # Cleanup after test: Clear DSS state to help free memory
    try:
        dss.text("Clear")
        dss.text("ClearAll")
    except Exception:
        pass

    # Force garbage collection to trigger weakref finalizer and clean up DLL files
    gc.collect()
