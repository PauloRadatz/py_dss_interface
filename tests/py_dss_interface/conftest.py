# -*- coding: utf-8 -*-
# @Time    : 6/4/2021 7:45 AM
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com
# @File    : conftest.py
# @Software: PyCharm

import os
import pathlib

import pytest

import py_dss_interface

script_path = os.path.dirname(os.path.abspath(__file__))


@pytest.fixture(scope='function')
def solve_snap_13bus():
    dss = py_dss_interface.DSS()

    dss.text("set DefaultBaseFrequency=60")
    dss.text("Set EventLogDefault=yes")
    dss13_path = os.path.join(pathlib.Path(script_path), "cases", "13Bus", "IEEE13Nodeckt.dss")
    dss.text(f"compile {dss13_path}")

    dss.dssinterface.allow_forms = 0
    return dss
