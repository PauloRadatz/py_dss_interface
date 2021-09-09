# -*- coding: utf-8 -*-
# @Time    : 9/03/2021 08:20 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_transformers.py
# @Software: PyCharm

import pytest


class TestTransformers13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.solution_solve()
        self.dss.transformers_write_name('sub')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_transformers_read_num_windings(self):
        expected = 2
        actual = self.dss.transformers_read_num_windings()
        assert expected == actual

    def test_transformers_write_num_windings(self):
        expected = 3
        self.dss.transformers_write_num_windings(expected)
        actual = self.dss.transformers_read_num_windings()
        assert expected == actual

    def test_transformers_read_wdg(self):
        expected = 2
        actual = self.dss.transformers_read_wdg()
        assert expected == actual

    def test_transformers_write_wdg(self):
        expected = 1
        self.dss.transformers_write_wdg(expected)
        actual = self.dss.transformers_read_wdg()
        assert expected == actual

    def test_transformers_read_num_taps(self):
        expected = 32
        actual = self.dss.transformers_read_num_taps()
        assert expected == actual

    def test_transformers_write_num_taps(self):
        expected = 16
        self.dss.transformers_write_num_taps(expected)
        actual = self.dss.transformers_read_num_taps()
        assert expected == actual

    def test_transformers_read_is_delta(self):
        expected = 0
        actual = self.dss.transformers_read_is_delta()
        assert expected == actual

    def test_transformers_write_is_delta(self):
        expected = 1
        self.dss.transformers_write_is_delta(expected)
        actual = self.dss.transformers_read_is_delta()
        assert expected == actual

    def test_transformers_first(self):
        expected = 1
        actual = self.dss.transformers_first()
        assert expected == actual

    def test_transformers_next(self):
        expected = 2
        actual = self.dss.transformers_next()
        assert expected == actual

    def test_transformers_count(self):
        expected = 5
        actual = self.dss.transformers_count()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_transformers_read_r(self):
        expected = 5e-6
        actual = self.dss.transformers_read_r()
        assert expected == actual

    def test_transformers_write_r(self):
        expected = 0.01
        self.dss.transformers_write_r(expected)
        actual = self.dss.transformers_read_r()
        assert expected == actual * 100

    def test_transformers_read_tap(self):
        expected = 1
        actual = self.dss.transformers_read_tap()
        assert expected == actual

    def test_transformers_write_tap(self):
        expected = 5
        self.dss.transformers_write_tap(expected)
        actual = self.dss.transformers_read_tap()
        assert expected == actual

    def test_transformers_read_min_tap(self):
        expected = 0.9
        actual = self.dss.transformers_read_min_tap()
        assert expected == actual

    def test_transformers_write_min_tap(self):
        expected = 0.5
        self.dss.transformers_write_min_tap(expected)
        actual = self.dss.transformers_read_min_tap()
        assert expected == actual

    def test_transformers_read_max_tap(self):
        expected = 1.1
        actual = self.dss.transformers_read_max_tap()
        assert expected == actual

    def test_transformers_write_max_tap(self):
        expected = 1.5
        self.dss.transformers_write_max_tap(expected)
        actual = self.dss.transformers_read_max_tap()
        assert expected == actual

    def test_transformers_read_kv(self):
        expected = 4.16
        actual = self.dss.transformers_read_kv()
        assert expected == actual

    def test_transformers_write_kv(self):
        expected = 3.8
        self.dss.transformers_write_kv(expected)
        actual = self.dss.transformers_read_kv()
        assert expected == actual

    def test_transformers_read_kva(self):
        expected = 5000
        actual = self.dss.transformers_read_kva()
        assert expected == actual

    def test_transformers_write_kva(self):
        expected = 10000
        self.dss.transformers_write_kva(expected)
        actual = self.dss.transformers_read_kva()
        assert expected == actual

    def test_transformers_read_x_neut(self):
        expected = 0
        actual = self.dss.transformers_read_x_neut()
        assert expected == actual

    def test_transformers_write_x_neut(self):
        expected = 1
        self.dss.transformers_write_x_neut(expected)
        actual = self.dss.transformers_read_x_neut()
        assert expected == actual

    def test_transformers_read_r_neut(self):
        expected = -1
        actual = self.dss.transformers_read_r_neut()
        assert expected == actual

    def test_transformers_write_r_neut(self):
        expected = 1
        self.dss.transformers_write_r_neut(expected)
        actual = self.dss.transformers_read_r_neut()
        assert expected == actual

    def test_transformers_read_xhl(self):
        expected = 8e-5
        actual = self.dss.transformers_read_xhl()
        assert expected == actual

    def test_transformers_write_xhl(self):
        expected = 8e-3
        self.dss.transformers_write_xhl(expected)
        actual = self.dss.transformers_read_xhl()
        assert expected == actual * 100

    def test_transformers_read_xht(self):
        expected = 0.04
        actual = self.dss.transformers_read_xht()
        assert expected == actual

    def test_transformers_write_xht(self):
        expected = 0.5
        self.dss.transformers_write_xht(expected)
        actual = self.dss.transformers_read_xht()
        assert expected == actual * 100

    def test_transformers_read_xlt(self):
        expected = 0.04
        actual = self.dss.transformers_read_xlt()
        assert expected == actual

    def test_transformers_write_xlt(self):
        expected = 0.5
        self.dss.transformers_write_xlt(expected)
        actual = self.dss.transformers_read_xlt()
        assert expected == actual * 100

    # ===================================================================
    # String methods
    # ===================================================================
    def test_transformers_read_xfmr_code(self):
        expected = ''
        actual = self.dss.transformers_read_xfmr_code()
        assert expected == actual

    def test_transformers_write_xfmr_code(self):
        self.dss.text(r'New XfmrCode.test phases=1 xhl=0.01 kvas=[1666 1666] kvs=[2.4 2.4] %LoadLoss=0.01 ')
        expected = 'test'
        self.dss.transformers_write_xfmr_code(expected)
        actual = self.dss.transformers_read_xfmr_code()
        assert expected == actual

    def test_transformers_read_name(self):
        expected = 'sub'
        actual = self.dss.transformers_read_name()
        assert expected == actual

    def test_transformers_write_name(self):
        expected = 'reg1'
        self.dss.transformers_write_name(expected)
        actual = self.dss.transformers_read_name()
        assert expected == actual

    def test_transformers_str_wdg_voltages(self):
        expected = '1'
        actual = self.dss.transformers_str_wdg_voltages()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_transformers_all_Names(self):
        expected = ['sub', 'reg1', 'reg2', 'reg3', 'xfm1']
        actual = self.dss.transformers_all_Names()
        assert expected == actual

    def test_transformers_wdg_voltages(self):
        expected = [2401.5628101024668, -0.4668918200941385, -1201.2376716779909,
                    -2079.7175222941146, -1200.311653100436, 2080.141949991459]
        actual = self.dss.transformers_wdg_voltages()
        assert expected == actual

    def test_transformers_wdg_currents(self):
        expected = [10.886379602870875, -5.958667666767724, -10.886375419202523, 5.958667666069232,
                    -521.2529520965181, 285.30771072395146, 521.2529520289972, -285.3080576583743,
                    -7.086441140418174, -5.6765349296765635, 7.086439048056491, 5.676531307020923,
                    339.306891429238, 271.7995472564362, -339.307191869244, -271.79937372310087,
                    -0.7714520632580388, 13.030924175051041, 0.7714499719149899, -13.030920551682357,
                    36.93846064992249, -623.9360991017893, -36.93816014844924, 623.9362725010142]
        actual = self.dss.transformers_wdg_currents()
        assert expected == actual
