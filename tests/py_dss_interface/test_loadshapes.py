# -*- coding: utf-8 -*-
# @Time    : 7/30/2021 02:01 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_loadshapes.py
# @Software: PyCharm

import pytest


class TestLoadShapes13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.name_write('default')

        return dss

    @staticmethod
    def new_loadshape(dss, activate: bool = False):
        dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        if activate:
            dss.name_write('test')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_loadshapes_count(self, dss):
        expected = 1
        actual = dss.count()
        assert actual == expected

        self.new_loadshape(dss)

        expected = 2
        actual = dss.count()
        assert actual == expected

    def test_loadshapes_first(self, dss):
        expected = 1
        actual = dss.first()
        assert actual == expected

    def test_loadshapes_next(self, dss):
        expected = 0
        actual = dss.next()
        assert actual == expected

    def test_loadshapes_read_npts(self, dss):
        expected = 24
        actual = dss.npts_read()
        assert actual == expected

    def test_loadshapes_write_npts(self, dss):
        expected = 48
        dss.npts_write(expected)
        actual = dss.npts_read()
        assert actual == expected

    def test_loadshapes_normalize(self, dss):
        expected = 0
        actual = dss.normalize()
        assert actual == expected

    def test_loadshapes_read_use_actual(self, dss):
        expected = 0
        actual = dss.use_actual_read()
        assert actual == expected

    def test_loadshapes_write_use_actual(self, dss):
        expected = 1
        dss.use_actual_write(expected)
        actual = dss.use_actual_read()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_loadshapes_read_name(self, dss):
        expected = 'default'
        actual = dss.name_read()
        assert actual == expected

    def test_loadshapes_write_name(self, dss):
        self.new_loadshape(dss)
        expected = 'test'
        dss.name_write(expected)
        actual = dss.name_read()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_loadshapes_read_hr_interval(self, dss):
        self.new_loadshape(dss, True)
        expected = 1
        actual = dss.hr_interval_read()
        assert actual == expected

    def test_loadshapes_write_hr_interval(self, dss):
        self.new_loadshape(dss, True)
        expected = 2
        dss.hr_interval_write(expected)
        actual = dss.hr_interval_read()
        assert actual == expected

    def test_loadshapes_read_min_interval(self, dss):
        self.new_loadshape(dss, True)
        expected = 60
        actual = dss.min_interval_read()
        assert actual == expected

    def test_loadshapes_write_min_interval(self, dss):
        self.new_loadshape(dss, True)
        expected = 240
        dss.min_interval_read(expected)
        actual = dss.min_interval_read()
        assert actual == expected

    def test_loadshapes_read_s_interval(self, dss):
        self.new_loadshape(dss, True)
        expected = 3600
        actual = dss.s_interval_read()
        assert actual == expected

    def test_loadshapes_write_s_interval(self, dss):
        self.new_loadshape(dss, True)
        expected = 4800
        dss.s_interval_write(expected)
        actual = dss.s_interval_read()
        assert actual == expected

    def test_loadshapes_read_p_base(self, dss):
        self.new_loadshape(dss, True)
        expected = 100
        actual = dss.p_base_read()
        assert actual == expected

    def test_loadshapes_write_p_base(self, dss):
        self.new_loadshape(dss, True)
        expected = 100.0
        dss.p_base_write(expected)
        actual = dss.p_base_read()
        assert actual == expected

    def test_loadshapes_read_q_base(self, dss):
        self.new_loadshape(dss, True)
        expected = 50
        actual = dss.q_base_read()
        assert actual == expected

    def test_loadshapes_write_q_base(self, dss):
        self.new_loadshape(dss, True)
        expected = 50
        dss.q_base_write(expected)
        actual = dss.q_base_read()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_loadshapes_all_names(self, dss):
        expected = ['default']
        actual = dss.names()
        assert actual == expected

        self.new_loadshape(dss)

        expected = ['default', 'test']
        actual = dss.names()
        assert actual == expected

    def test_loadshapes_read_p_mult(self, dss):
        self.new_loadshape(dss, True)
        expected = [0.18000001, 0.19, 0.23999999, 0.33000001, 0.38999999, 0.41, 0.64999998, 1.23000002,
                    1.88999999, 1.88999999, 1.96000004, 1.98000002, 1.45000005, 1.62, 1.88999999, 1.79999995,
                    1.78999996, 1.19000006, 0.80000001, 0.66000003, 0.51999998, 0.40000001, 0.28, 0.23]
        actual = dss.p_mult_read()
        assert actual == expected

    def test_loadshapes_write_p_mult(self, dss):
        self.new_loadshape(dss, True)
        expected = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        dss.p_mult_write(expected)
        actual = dss.p_mult_read()
        assert actual == expected

    def test_loadshapes_read_q_mult(self, dss):
        self.new_loadshape(dss, True)
        expected = [0]
        actual = dss.q_mult_read()
        assert actual == expected

    def test_loadshapes_write_q_mult(self, dss):
        self.new_loadshape(dss, True)
        expected = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        dss.q_mult_write(expected)
        actual = dss.q_mult_read()
        assert actual == expected

    def test_loadshapes_read_time_array(self, dss):
        self.new_loadshape(dss, True)
        expected = [0]
        actual = dss.time_array_read()
        assert actual == expected

    def test_loadshapes_write_time_array(self, dss):
        self.new_loadshape(dss, True)
        expected = [-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        dss.time_array_write(expected)
        actual = dss.time_array_read()
        assert actual == expected
