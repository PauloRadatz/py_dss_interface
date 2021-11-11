# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 09:15 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_cmathlib.py
# @Software: PyCharm


import math
import pytest


class TestCMathLib13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus

        return dss

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_cmathlib_cabs(self, dss):
        real, imag = -3, 4
        expected = (real ** 2 + imag ** 2) ** 0.5
        actual = dss.cmathlib_cabs(real, imag)
        assert actual == expected

    def test_cmathlib_cdang(self, dss):
        real, imag = 1, 1
        expected = 45
        actual = dss.cmathlib_cdang(1, 1)
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_cmathlib_cmplx(self, dss):
        real, imag = 1, 1
        expected = complex(real, imag)
        actual = dss.cmathlib_cmplx(real, imag)
        assert actual == expected

    def test_cmathlib_ctopolardeg(self, dss):
        real, imag = 1, 3
        expected = (abs(complex(real, imag)), math.atan2(imag, real))
        actual = dss.cmathlib_ctopolardeg(real, imag)
        assert actual == expected

    def test_cmathlib_pdegtocomplex(self, dss):
        real, imag = 3.1622776601683795, 1.2490457723982544
        actual = dss.cmathlib_pdegtocomplex(real, imag)
        expected = complex(real, imag)
