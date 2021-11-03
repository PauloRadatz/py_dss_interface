# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:58 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_xycurves.py
# @Software : VSCode

import pytest


class TestXYCurves13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text(r'New XYCurve.xycurve1 npts=4  xarray=[.1  .2  .4  1.0]  yarray=[.86  .9  .93  .97]')
        self.dss.text(r'New XYCurve.xycurve2 npts=4  xarray=[.1  .2  .4  1.0]  yarray=[.86  .9  .93  .97]')
        self.dss.solution_solve()
        self.dss.xycurves_write_name('xycurve1')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_xycurves_count(self):
        expected = 2
        actual = self.dss.xycurves_count()
        assert actual == expected

    def test_xycurves_first(self):
        expected = 1
        actual = self.dss.xycurves_first()
        assert actual == expected

    def test_xycurves_next(self):
        expected = 2
        actual = self.dss.xycurves_next()
        assert actual == expected

    def test_xycurves_read_npts(self):
        expected = 4
        actual = self.dss.xycurves_read_npts()
        assert actual == expected

    def test_xycurves_write_npts(self):
        expected = 8
        self.dss.xycurves_write_npts(expected)
        actual = self.dss.xycurves_read_npts()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_xycurves_read_x(self):
        expected = 0.1
        actual = self.dss.xycurves_read_x()
        assert actual == expected

    def test_xycurves_write_x(self):
        expected = 1
        self.dss.xycurves_write_x(expected)
        actual = self.dss.xycurves_read_x()
        assert actual == expected

    def test_xycurves_read_y(self):
        expected = 1
        actual = self.dss.xycurves_read_y()
        assert actual == expected

    def test_xycurves_write_y(self):
        expected = 1
        self.dss.xycurves_write_y(expected)
        actual = self.dss.xycurves_read_y()
        assert actual == expected

    def test_xycurves_read_x_shift(self):
        expected = 0
        actual = self.dss.xycurves_read_x_shift()
        assert actual == expected

    def test_xycurves_write_x_shift(self):
        expected = 1
        self.dss.xycurves_write_x_shift(expected)
        actual = self.dss.xycurves_read_x_shift()
        assert actual == expected

    def test_xycurves_read_y_shift(self):
        expected = 0
        actual = self.dss.xycurves_read_y_shift()
        assert actual == expected

    def test_xycurves_write_y_shift(self):
        expected = 1
        self.dss.xycurves_write_y_shift(expected)
        actual = self.dss.xycurves_read_y_shift()
        assert actual == expected

    def test_xycurves_read_x_scale(self):
        expected = 1
        actual = self.dss.xycurves_read_x_scale()
        assert actual == expected

    def test_xycurves_write_x_scale(self):
        expected = 1
        self.dss.xycurves_write_x_scale(expected)
        actual = self.dss.xycurves_read_x_scale()
        assert actual == expected

    def test_xycurves_read_y_scale(self):
        expected = 1
        actual = self.dss.xycurves_read_y_scale()
        assert actual == expected

    def test_xycurves_write_y_scale(self):
        expected = 1
        self.dss.xycurves_write_y_scale(expected)
        actual = self.dss.xycurves_read_y_scale()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_xycurves_read_name(self):
        expected = 'xycurve1'
        actual = self.dss.xycurves_read_name()
        assert actual == expected

    def test_xycurves_write_name(self):
        expected = 'xycurve2'
        self.dss.xycurves_write_name(expected)
        actual = self.dss.xycurves_read_name()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_xycurves_read_x_array(self):
        expected = [.1, .2, .4, 1.0]
        actual = self.dss.xycurves_read_x_array()
        assert actual == expected

    def test_xycurves_write_x_array(self):
        expected = [1, 1, 1, 1]
        self.dss.xycurves_write_x_array(str(expected))
        actual = self.dss.xycurves_read_x_array()
        assert actual == expected

    def test_xycurves_read_y_array(self):
        expected = [.86, .9, .93, .97]
        actual = self.dss.xycurves_read_y_array()
        assert actual == expected

    def test_xycurves_write_y_array(self):
        expected = [1, 1, 1, 1]
        self.dss.xycurves_write_y_array(str(expected))
        actual = self.dss.xycurves_read_y_array()
        assert actual == expected
