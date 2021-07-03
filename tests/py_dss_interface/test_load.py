# -*- coding: utf-8 -*-
# @Time    : 6/24/2021 10:51 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_load.py
# @Software: PyCharm

import pytest
import platform
import math as m


class TestLoad13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.loads_write_name('671')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_loads_count(self):
        expected = 15
        actual = self.dss.loads_count()
        assert actual == expected

    def test_loads_first(self):
        expected = 1
        actual = self.dss.loads_first()
        assert actual == expected

    def test_loads_next(self):
        expected = 2
        actual = self.dss.loads_next()
        assert expected == actual

    def test_loads_read_idx(self):
        expected = 1
        actual = self.dss.loads_read_idx()
        assert actual == expected

    def test_loads_write_idx(self):
        expected = 2
        self.dss.loads_write_idx(expected)
        actual = self.dss.loads_read_idx()
        assert actual == expected

    def test_loads_read_class(self):
        expected = 1
        actual = self.dss.loads_read_class()
        assert actual == expected

    def test_loads_write_class(self):
        expected = 2
        self.dss.loads_write_class(expected)
        actual = self.dss.loads_read_class()
        assert actual == expected

    def test_loads_read_model(self):
        expected = 1
        actual = self.dss.loads_read_model()
        assert actual == expected

    def test_loads_write_model(self):
        expected = 2
        self.dss.loads_write_model(expected)
        actual = self.dss.loads_read_model()
        assert actual == expected

    def test_loads_read_num_cust(self):
        expected = 1
        actual = self.dss.loads_read_num_cust()
        assert actual == expected

    def test_loads_write_num_cust(self):
        expected = 12
        self.dss.loads_write_num_cust(expected)
        actual = self.dss.loads_read_num_cust()
        assert actual == expected

    def test_loads_read_status(self):
        expected = 0
        actual = self.dss.loads_read_status()
        assert actual == expected

    def test_loads_write_status(self):
        expected = 1
        self.dss.loads_write_status(expected)
        actual = self.dss.loads_read_status()
        assert actual == expected

    def test_loads_read_is_delta(self):
        expected = 1
        actual = self.dss.loads_read_is_delta()
        assert actual == expected

    def test_loads_write_is_delta(self):
        expected = 0
        self.dss.loads_write_is_delta(expected)
        actual = self.dss.loads_read_is_delta()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_loads_read_name(self):
        expected = '671'
        actual = self.dss.loads_read_name()
        assert actual == expected

    def test_loads_write_name(self):
        expected = '634a'
        actual = self.dss.loads_write_name(expected)
        actual = self.dss.loads_read_name()
        assert actual == expected

    def test_loads_read_cvr_curve(self):
        expected = ''
        actual = self.dss.loads_read_cvr_curve()
        assert actual == expected

    def test_loads_write_cvr_curve(self):
        # TODO What values should be used to test?
        expected = '1'
        actual = self.dss.loads_write_cvr_curve(expected)
        actual = self.dss.loads_read_cvr_curve()
        assert actual == expected

    def test_loads_read_daily(self):
        expected = ''
        actual = self.dss.loads_read_daily()
        assert actual == expected

    def test_loads_write_daily(self):
        self.dss.text("New Loadshape.Teste npts=24 interval=1 mult=(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Teste'
        actual = self.dss.loads_write_daily(expected)
        actual = self.dss.loads_read_daily()
        assert actual == expected

    def test_loads_read_duty(self):
        expected = ''
        actual = self.dss.loads_read_duty()
        assert actual == expected

    def test_loads_read_spectrum(self):
        expected = 'defaultload'
        actual = self.dss.loads_read_spectrum()
        assert actual == expected

    def test_loads_write_spectrum(self):
        # TODO What values should be used to test?
        expected = 'test'
        self.dss.loads_write_spectrum(expected)
        actual = self.dss.loads_read_spectrum()
        assert actual == expected

    def test_loads_read_yearly(self):
        expected = ''
        actual = self.dss.loads_read_yearly()
        assert actual == expected

    def test_loads_write_yearly(self):
        self.dss.text("New Loadshape.Teste npts=24 interval=1 mult=(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Teste'
        actual = self.dss.loads_write_yearly(expected)
        actual = self.dss.loads_read_yearly()
        assert actual == expected

    def test_loads_read_growth(self):
        expected = ''
        actual = self.dss.loads_read_growth()
        assert actual == expected

    def test_loads_write_growth(self):
        self.dss.text("New GrowthShape.default npts=2 year=(1, 20, ) mult=(1.025, 1.025, )")
        expected = 'default'
        self.dss.loads_write_growth(expected)
        actual = self.dss.loads_read_growth()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_loads_read_kv(self):
        expected = 4.16
        actual = self.dss.loads_read_kv()
        assert actual == expected

    def test_loads_write_kv(self):
        expected = 0.48
        self.dss.loads_write_kv(expected)
        actual = self.dss.loads_read_kv()
        assert actual == expected

    def test_loads_read_kw(self):
        expected = 1155
        actual = self.dss.loads_read_kw()
        assert actual == expected

    def test_loads_write_kw(self):
        expected = 1000.0   # TODO it only works when we set decimal number. Like in capacitors.
        self.dss.loads_write_kw(expected)
        actual = self.dss.loads_read_kw()
        assert actual == expected

    def test_loads_read_kvar(self):
        expected = 660
        actual = self.dss.loads_read_kvar()
        assert actual == expected

    def test_loads_write_kvar(self):
        expected = 600.0   # TODO it only works when we set decimal number.
        self.dss.loads_write_kvar(expected)
        actual = self.dss.loads_read_kvar()
        assert actual == expected

    def test_loads_read_pf(self):
        if platform.architecture()[0] == "64bit":
            expected = 0.8682431421244591
            actual = self.dss.loads_read_pf()
            assert actual == expected
        else:
            assert 1 == 1

    def test_loads_write_pf(self):
        expected = 1.0  # TODO it only works when we set decimal number. Like in capacitors.
        self.dss.loads_write_pf(expected)
        actual = self.dss.loads_read_pf()
        assert actual == expected

    def test_loads_read_pct_mean(self):
        expected = 50
        actual = self.dss.loads_read_pct_mean()
        assert actual == expected

    def test_loads_write_pct_mean(self):
        expected = 123.0    # TODO it only works when we set decimal number.
        self.dss.loads_write_pct_mean(expected)
        actual = self.dss.loads_read_pct_mean()
        assert actual == expected

    def test_loads_read_pct_std_dev(self):
        expected = 10
        actual = self.dss.loads_read_pct_std_dev()
        assert actual == expected

    def test_loads_write_pct_std_dev(self):
        expected = 123.0    # TODO it only works when we set decimal number.
        self.dss.loads_write_pct_std_dev(expected)
        actual = self.dss.loads_read_pct_std_dev()
        assert actual == expected

    def test_loads_read_pct_series_rl(self):
        expected = 50
        actual = self.dss.loads_read_pct_series_rl()
        assert actual == expected

    def test_loads_write_pct_series_rl(self):
        expected = 123    # TODO it only works when we set decimal number.
        self.dss.loads_write_pct_series_rl(expected)
        actual = self.dss.loads_read_pct_series_rl()
        assert actual == expected

    def test_loads_read_allocation_factor(self):
        expected = 0.5
        actual = self.dss.loads_read_allocation_factor()
        assert actual == expected

    def test_loads_write_allocation_factor(self):
        expected = 123.1   # TODO it only works when we set decimal number.
        self.dss.loads_write_allocation_factor(expected)
        actual = self.dss.loads_read_allocation_factor()
        assert actual == expected

    def test_loads_read_c_factor(self):
        expected = 4
        actual = self.dss.loads_read_c_factor()
        assert actual == expected

    def test_loads_write_c_factor(self):
        expected = 123.1  # TODO it only works when we set decimal number.
        self.dss.loads_write_c_factor(expected)
        actual = self.dss.loads_read_c_factor()
        assert actual == expected

    def test_loads_read_cvr_watts(self):
        expected = 1
        actual = self.dss.loads_read_cvr_watts()
        assert actual == expected

    def test_loads_write_cvr_watts(self):
        expected = 123.1  # TODO it only works when we set decimal number.
        self.dss.loads_write_cvr_watts(expected)
        actual = self.dss.loads_read_cvr_watts()
        assert actual == expected

    def test_loads_read_cvr_vars(self):
        expected = 2
        actual = self.dss.loads_read_cvr_vars()
        assert actual == expected

    def test_loads_write_cvr_vars(self):
        expected = 123.1  # TODO it only works when we set decimal number.
        self.dss.loads_write_cvr_vars(expected)
        actual = self.dss.loads_read_cvr_vars()
        assert actual == expected

    def test_loads_read_kva(self):
        if platform.architecture()[0] == "64bit":
            expected = (self.dss.loads_read_kw()**2 + self.dss.loads_read_kvar()**2)**(1/2)
            actual = self.dss.loads_read_kva()
            assert actual == expected
        else:
            assert 1 == 1

    def test_loads_write_kva(self):
        expected = 1500.0   # TODO it only works when we set decimal number.
        self.dss.loads_write_kva(expected)
        actual = self.dss.loads_read_kva()
        assert actual == expected

    def test_loads_read_kwh(self):
        expected = 0
        actual = self.dss.loads_read_kwh()
        assert actual == expected

    def test_loads_write_kwh(self):
        expected = 123.0    # TODO it only works when we set decimal number.
        self.dss.loads_write_kwh(expected)
        actual = self.dss.loads_read_kwh()
        assert actual == expected

    def test_loads_read_kwh_days(self):
        expected = 30
        actual = self.dss.loads_read_kwh_days()
        assert actual == expected

    def test_loads_write_kwh_days(self):
        expected = 60.0    # TODO it only works when we set decimal number.
        self.dss.loads_write_kwh_days(expected)
        actual = self.dss.loads_read_kwh_days()
        assert actual == expected

    def test_loads_read_r_neut(self):
        expected = -1
        actual = self.dss.loads_read_r_neut()
        assert actual == expected

    def test_loads_write_r_neut(self):
        expected = 60.0    # TODO it only works when we set decimal number.
        self.dss.loads_write_r_neut(expected)
        actual = self.dss.loads_read_r_neut()
        assert actual == expected

    def test_loads_read_x_neut(self):
        expected = 0
        actual = self.dss.loads_read_x_neut()
        assert actual == expected

    def test_loads_write_x_neut(self):
        expected = 60.0    # TODO it only works when we set decimal number.
        self.dss.loads_write_x_neut(expected)
        actual = self.dss.loads_read_x_neut()
        assert actual == expected

    def test_loads_read_vmax_pu(self):
        expected = 1.05
        actual = self.dss.loads_read_vmax_pu()
        assert actual == expected

    def test_loads_write_vmax_pu(self):
        expected = 1.1    # TODO it only works when we set decimal number.
        self.dss.loads_write_vmax_pu(expected)
        actual = self.dss.loads_read_vmax_pu()
        assert actual == expected

    def test_loads_read_vmin_pu(self):
        expected = 0.95
        actual = self.dss.loads_read_vmin_pu()
        assert actual == expected

    def test_loads_write_vmin_pu(self):
        expected = 0.9    # TODO it only works when we set decimal number.
        self.dss.loads_write_vmin_pu(expected)
        actual = self.dss.loads_read_vmin_pu()
        assert actual == expected

    def test_loads_read_vmin_emerg(self):
        expected = 0
        actual = self.dss.loads_read_vmin_emerg()
        assert actual == expected

    def test_loads_write_vmin_emerg(self):
        expected = 0.5   # TODO it only works when we set decimal number.
        self.dss.loads_write_vmin_emerg(expected)
        actual = self.dss.loads_read_vmin_emerg()
        assert actual == expected

    def test_loads_read_vmin_norm(self):
        expected = 0
        actual = self.dss.loads_read_vmin_norm()
        assert actual == expected

    def test_loads_write_vmin_norm(self):
        expected = 0.8   # TODO it only works when we set decimal number.
        self.dss.loads_write_vmin_norm(expected)
        actual = self.dss.loads_read_vmin_norm()
        assert actual == expected

    def test_loads_read_xfkva(self):
        expected = 0
        actual = self.dss.loads_read_xfkva()
        assert actual == expected

    def test_loads_write_xfkva(self):
        expected = 123.1   # TODO it only works when we set decimal number.
        self.dss.loads_write_xfkva(expected)
        actual = self.dss.loads_read_xfkva()
        assert actual == expected

    def test_loads_read_rel_weight(self):
        expected = 1
        actual = self.dss.loads_read_rel_weight()
        assert actual == expected

    def test_loads_write_rel_weight(self):
        expected = 123.1   # TODO it only works when we set decimal number.
        self.dss.loads_write_rel_weight(expected)
        actual = self.dss.loads_read_rel_weight()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_loads_all_names(self):
        expected = ['671', '634a', '634b', '634c', '645', '646', '692',
                    '675a', '675b', '675c', '611', '652', '670a', '670b',
                    '670c']
        actual = self.dss.loads_all_names()
        assert actual == expected

    def test_loads_read_zipv(self):
        expected = []
        actual = self.dss.loads_read_zipv()
        assert actual == expected

    def test_loads_write_zipv(self):
        expected = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
        self.dss.loads_write_zipv(str(expected))    # TODO it works only when we convert the number array to a string
        actual = self.dss.loads_read_zipv()
        assert actual == expected
