# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:40 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_transformers.py
# @Software : VSCode

import pytest


class TestTransformers13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.solution_solve()
        dss.transformers_write_name('sub')

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_transformers_read_num_windings(self, dss):
        expected = 2
        actual = dss.transformers_read_num_windings()
        assert actual == expected

    def test_transformers_write_num_windings(self, dss):
        expected = 3
        dss.transformers_write_num_windings(expected)
        actual = dss.transformers_read_num_windings()
        assert actual == expected

    def test_transformers_read_wdg(self, dss):
        expected = 2
        actual = dss.transformers_read_wdg()
        assert actual == expected

    def test_transformers_write_wdg(self, dss):
        expected = 1
        dss.transformers_write_wdg(expected)
        actual = dss.transformers_read_wdg()
        assert actual == expected

    def test_transformers_read_num_taps(self, dss):
        expected = 32
        actual = dss.transformers_read_num_taps()
        assert actual == expected

    def test_transformers_write_num_taps(self, dss):
        expected = 16
        dss.transformers_write_num_taps(expected)
        actual = dss.transformers_read_num_taps()
        assert actual == expected

    def test_transformers_read_is_delta(self, dss):
        expected = 0
        actual = dss.transformers_read_is_delta()
        assert actual == expected

    def test_transformers_write_is_delta(self, dss):
        expected = 1
        dss.transformers_write_is_delta(expected)
        actual = dss.transformers_read_is_delta()
        assert actual == expected

    def test_transformers_first(self, dss):
        expected = 1
        actual = dss.transformers_first()
        assert actual == expected

    def test_transformers_next(self, dss):
        expected = 2
        actual = dss.transformers_next()
        assert actual == expected

    def test_transformers_count(self, dss):
        expected = 5
        actual = dss.transformers_count()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_transformers_read_r(self, dss):
        expected = 0.0005
        actual = dss.transformers_read_r()
        assert actual == expected

    def test_transformers_write_r(self, dss):
        expected = 0.01
        dss.transformers_write_r(expected)
        actual = dss.transformers_read_r()
        assert actual == expected

    def test_transformers_read_tap(self, dss):
        expected = 1
        actual = dss.transformers_read_tap()
        assert actual == expected

    def test_transformers_write_tap(self, dss):
        expected = 5
        dss.transformers_write_tap(expected)
        actual = dss.transformers_read_tap()
        assert actual == expected

    def test_transformers_read_min_tap(self, dss):
        expected = 0.9
        actual = dss.transformers_read_min_tap()
        assert actual == expected

    def test_transformers_write_min_tap(self, dss):
        expected = 0.5
        dss.transformers_write_min_tap(expected)
        actual = dss.transformers_read_min_tap()
        assert actual == expected

    def test_transformers_read_max_tap(self, dss):
        expected = 1.1
        actual = dss.transformers_read_max_tap()
        assert actual == expected

    def test_transformers_write_max_tap(self, dss):
        expected = 1.5
        dss.transformers_write_max_tap(expected)
        actual = dss.transformers_read_max_tap()
        assert actual == expected

    def test_transformers_read_kv(self, dss):
        expected = 4.16
        actual = dss.transformers_read_kv()
        assert actual == expected

    def test_transformers_write_kv(self, dss):
        expected = 3.8
        dss.transformers_write_kv(expected)
        actual = dss.transformers_read_kv()
        assert actual == expected

    def test_transformers_read_kva(self, dss):
        expected = 5000
        actual = dss.transformers_read_kva()
        assert actual == expected

    def test_transformers_write_kva(self, dss):
        expected = 10000
        dss.transformers_write_kva(expected)
        actual = dss.transformers_read_kva()
        assert actual == expected

    def test_transformers_read_x_neut(self, dss):
        expected = 0
        actual = dss.transformers_read_x_neut()
        assert actual == expected

    def test_transformers_write_x_neut(self, dss):
        expected = 1
        dss.transformers_write_x_neut(expected)
        actual = dss.transformers_read_x_neut()
        assert actual == expected

    def test_transformers_read_r_neut(self, dss):
        expected = -1
        actual = dss.transformers_read_r_neut()
        assert actual == expected

    def test_transformers_write_r_neut(self, dss):
        expected = 1
        dss.transformers_write_r_neut(expected)
        actual = dss.transformers_read_r_neut()
        assert actual == expected

    def test_transformers_read_xhl(self, dss):
        expected = 0.008
        actual = dss.transformers_read_xhl()
        assert actual == expected

    def test_transformers_write_xhl(self, dss):
        expected = 0.008
        dss.transformers_write_xhl(expected)
        actual = dss.transformers_read_xhl()
        assert actual == expected

    def test_transformers_read_xht(self, dss):
        expected = 4
        actual = dss.transformers_read_xht()
        assert actual == expected

    def test_transformers_write_xht(self, dss):
        expected = 5
        dss.transformers_write_xht(expected)
        actual = dss.transformers_read_xht()
        assert actual == expected

    def test_transformers_read_xlt(self, dss):
        expected = 4
        actual = dss.transformers_read_xlt()
        assert actual == expected

    def test_transformers_write_xlt(self, dss):
        expected = 5
        dss.transformers_write_xlt(expected)
        actual = dss.transformers_read_xlt()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_transformers_read_xfmr_code(self, dss):
        expected = ''
        actual = dss.transformers_read_xfmr_code()
        assert actual == expected

    def test_transformers_write_xfmr_code(self, dss):
        dss.text(r'New XfmrCode.test phases=1 xhl=0.01 kvas=[1666 1666] kvs=[2.4 2.4] %LoadLoss=0.01 ')
        expected = 'test'
        dss.transformers_write_xfmr_code(expected)
        actual = dss.transformers_read_xfmr_code()
        assert actual == expected

    def test_transformers_read_name(self, dss):
        expected = 'sub'
        actual = dss.transformers_read_name()
        assert actual == expected

    def test_transformers_write_name(self, dss):
        expected = 'reg1'
        dss.transformers_write_name(expected)
        actual = dss.transformers_read_name()
        assert actual == expected

    def test_transformers_str_wdg_voltages(self, dss):
        expected = '1'
        actual = dss.transformers_str_wdg_voltages()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_transformers_all_Names(self, dss):
        expected = ['sub', 'reg1', 'reg2', 'reg3', 'xfm1']
        actual = dss.transformers_all_Names()
        assert actual == expected

    def test_transformers_wdg_voltages(self, dss):
        expected = [2401.5628121109403,
                    -0.4668923729244497,
                    -1201.237672392959,
                    -2079.717523220085,
                    -1200.311654294895,
                    2080.141951753078]
        actual = dss.transformers_wdg_voltages()
        assert actual == expected

    def test_transformers_wdg_currents(self, dss):
        expected = [10.886376124155504,
                    -5.958628293446964,
                    -10.886371940479876,
                    5.958628292748472,
                    -521.2527855311055,
                    285.3058254830539,
                    521.2527854638174,
                    -285.3061724174768,
                    -7.086427310190629,
                    -5.676542717425036,
                    7.086425217828946,
                    5.676539094769396,
                    339.30622922163457,
                    271.7999201430939,
                    -339.3065296616405,
                    -271.7997466106899,
                    -0.771484338270966,
                    13.030897319840733,
                    0.771482246927917,
                    -13.0308936964866,
                    36.940006016753614,
                    -623.934813240543,
                    -36.93970551621169,
                    623.9349866397679]
        actual = dss.transformers_wdg_currents()
        assert actual == expected
