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
        dss.solution.solve()
        dss.xycurves.name = 'xycurve1'

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_xycurves_count(self, dss):
        expected = 2
        actual = dss.xycurves.count
        assert actual == expected

    def test_xycurves_first(self, dss):
        expected = 1
        actual = dss.xycurves.first()
        assert actual == expected

    def test_xycurves_next(self, dss):
        expected = 2
        actual = dss.xycurves.next()
        assert actual == expected

    def test_xycurves_read_npts(self, dss):
        expected = 4
        actual = dss.xycurves.npts
        assert actual == expected

    def test_xycurves_write_npts(self, dss):
        expected = 8
        dss.xycurves.npts = expected
        actual = dss.xycurves.npts
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_xycurves_read_x(self, dss):
        expected = 0.1
        actual = dss.xycurves.x
        assert actual == expected

    def test_xycurves_write_x(self, dss):
        expected = 1
        dss.xycurves.x = expected
        actual = dss.xycurves.x
        assert actual == expected

    def test_xycurves_read_y(self, dss):
        expected = 1
        actual = dss.xycurves.y
        assert actual == expected

    def test_xycurves_write_y(self, dss):
        expected = 1
        dss.xycurves.y = expected
        actual = dss.xycurves.y
        assert actual == expected

    def test_xycurves_read_x_shift(self, dss):
        expected = 0
        actual = dss.xycurves.x_shift
        assert actual == expected

    def test_xycurves_write_x_shift(self, dss):
        expected = 1
        dss.xycurves.x_shift = expected
        actual = dss.xycurves.x_shift
        assert actual == expected

    def test_xycurves_read_y_shift(self, dss):
        expected = 0
        actual = dss.xycurves.y_shift
        assert actual == expected

    def test_xycurves_write_y_shift(self, dss):
        expected = 1
        dss.xycurves.y_shift = expected
        actual = dss.xycurves.y_shift
        assert actual == expected

    def test_xycurves_read_x_scale(self, dss):
        expected = 1
        actual = dss.xycurves.x_scale
        assert actual == expected

    def test_xycurves_write_x_scale(self, dss):
        expected = 1
        dss.xycurves.x_scale = expected
        actual = dss.xycurves.x_scale
        assert actual == expected

    def test_xycurves_read_y_scale(self, dss):
        expected = 1
        actual = dss.xycurves.y_scale
        assert actual == expected

    def test_xycurves_write_y_scale(self, dss):
        expected = 1
        dss.xycurves.y_scale = expected
        actual = dss.xycurves.y_scale
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_xycurves_read_name(self, dss):
        expected = 'xycurve1'
        actual = dss.xycurves.name
        assert actual == expected

    def test_xycurves_write_name(self, dss):
        expected = 'xycurve2'
        dss.xycurves.name = expected
        actual = dss.xycurves.name
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_xycurves_read_x_array(self, dss):
        expected = [.1, .2, .4, 1.0]
        actual = dss.xycurves.x_array
        assert actual == expected

    def test_xycurves_write_x_array(self, dss):
        expected = [1, 1, 1, 1]
        dss.xycurves.x_array = expected
        actual = dss.xycurves.x_array
        assert actual == expected

    def test_xycurves_read_y_array(self, dss):
        expected = [.86, .9, .93, .97]
        actual = dss.xycurves.y_array
        assert actual == expected

    def test_xycurves_write_y_array(self, dss):
        expected = [1, 1, 1, 1]
        dss.xycurves.y_array = expected
        actual = dss.xycurves.y_array
        assert actual == expected
