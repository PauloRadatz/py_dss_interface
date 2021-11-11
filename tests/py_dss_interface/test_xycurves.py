# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:58 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_xycurves.py
# @Software : VSCode

import pytest


class TestXYCurves13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text(r'New XYCurve.xycurve1 npts=4  xarray=[.1  .2  .4  1.0]  yarray=[.86  .9  .93  .97]')
        dss.text(r'New XYCurve.xycurve2 npts=4  xarray=[.1  .2  .4  1.0]  yarray=[.86  .9  .93  .97]')
        dss.solution_solve()
        dss.xycurves_write_name('xycurve1')

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_xycurves_count(self, dss):
        expected = 2
        actual = dss.xycurves_count()
        assert actual == expected

    def test_xycurves_first(self, dss):
        expected = 1
        actual = dss.xycurves_first()
        assert actual == expected

    def test_xycurves_next(self, dss):
        expected = 2
        actual = dss.xycurves_next()
        assert actual == expected

    def test_xycurves_read_npts(self, dss):
        expected = 4
        actual = dss.xycurves_read_npts()
        assert actual == expected

    def test_xycurves_write_npts(self, dss):
        expected = 8
        dss.xycurves_write_npts(expected)
        actual = dss.xycurves_read_npts()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_xycurves_read_x(self, dss):
        expected = 0.1
        actual = dss.xycurves_read_x()
        assert actual == expected

    def test_xycurves_write_x(self, dss):
        expected = 1
        dss.xycurves_write_x(expected)
        actual = dss.xycurves_read_x()
        assert actual == expected

    def test_xycurves_read_y(self, dss):
        expected = 1
        actual = dss.xycurves_read_y()
        assert actual == expected

    def test_xycurves_write_y(self, dss):
        expected = 1
        dss.xycurves_write_y(expected)
        actual = dss.xycurves_read_y()
        assert actual == expected

    def test_xycurves_read_x_shift(self, dss):
        expected = 0
        actual = dss.xycurves_read_x_shift()
        assert actual == expected

    def test_xycurves_write_x_shift(self, dss):
        expected = 1
        dss.xycurves_write_x_shift(expected)
        actual = dss.xycurves_read_x_shift()
        assert actual == expected

    def test_xycurves_read_y_shift(self, dss):
        expected = 0
        actual = dss.xycurves_read_y_shift()
        assert actual == expected

    def test_xycurves_write_y_shift(self, dss):
        expected = 1
        dss.xycurves_write_y_shift(expected)
        actual = dss.xycurves_read_y_shift()
        assert actual == expected

    def test_xycurves_read_x_scale(self, dss):
        expected = 1
        actual = dss.xycurves_read_x_scale()
        assert actual == expected

    def test_xycurves_write_x_scale(self, dss):
        expected = 1
        dss.xycurves_write_x_scale(expected)
        actual = dss.xycurves_read_x_scale()
        assert actual == expected

    def test_xycurves_read_y_scale(self, dss):
        expected = 1
        actual = dss.xycurves_read_y_scale()
        assert actual == expected

    def test_xycurves_write_y_scale(self, dss):
        expected = 1
        dss.xycurves_write_y_scale(expected)
        actual = dss.xycurves_read_y_scale()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_xycurves_read_name(self, dss):
        expected = 'xycurve1'
        actual = dss.xycurves_read_name()
        assert actual == expected

    def test_xycurves_write_name(self, dss):
        expected = 'xycurve2'
        dss.xycurves_write_name(expected)
        actual = dss.xycurves_read_name()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_xycurves_read_x_array(self, dss):
        expected = [.1, .2, .4, 1.0]
        actual = dss.xycurves_read_x_array()
        assert actual == expected

    def test_xycurves_write_x_array(self, dss):
        expected = [1, 1, 1, 1]
        dss.xycurves_write_x_array(str(expected))
        actual = dss.xycurves_read_x_array()
        assert actual == expected

    def test_xycurves_read_y_array(self, dss):
        expected = [.86, .9, .93, .97]
        actual = dss.xycurves_read_y_array()
        assert actual == expected

    def test_xycurves_write_y_array(self, dss):
        expected = [1, 1, 1, 1]
        dss.xycurves_write_y_array(str(expected))
        actual = dss.xycurves_read_y_array()
        assert actual == expected
