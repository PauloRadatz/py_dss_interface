# -*- coding: utf-8 -*-
# @Time    : 6/4/2021 7:45 AM
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com
# @File    : conftest.py
# @Software: PyCharm

import pytest
import pathlib
import os
import py_dss_interface

script_path = os.path.dirname(os.path.abspath(__file__))


@pytest.fixture
def solve_snap_13bus():
    dss = py_dss_interface.DSSDLL()
    actual = dss.started
    expected = True

    message = ("OpenDSSDirectDLL has been loaded: {}".format(actual))

    assert actual is expected, message

    dss.text("set DefaultBaseFrequency=60")
    dss13_path = os.path.join(pathlib.Path(script_path), "cases", "13Bus", "IEEE13Nodeckt.dss")
    dss.text("compile " + dss13_path)  # It already performs power flow

    return dss
