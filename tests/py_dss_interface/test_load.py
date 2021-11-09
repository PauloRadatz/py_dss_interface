# -*- coding: utf-8 -*-
# @Time    : 6/24/2021 10:51 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_load.py
# @Software: PyCharm

import pytest
import platform


class TestLoad13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.loads_write_name('671')

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_loads_count(self, dss):
        expected = 15
        actual = dss.loads_count()
        assert actual == expected

    def test_loads_first(self, dss):
        expected = 1
        actual = dss.loads_first()
        assert actual == expected

    def test_loads_next(self, dss):
        expected = 2
        actual = dss.loads_next()
        assert actual == expected

    def test_loads_read_idx(self, dss):
        expected = 1
        actual = dss.loads_read_idx()
        assert actual == expected

    def test_loads_write_idx(self, dss):
        expected = 2
        dss.loads_write_idx(expected)
        actual = dss.loads_read_idx()
        assert actual == expected

    def test_loads_read_class(self, dss):
        expected = 1
        actual = dss.loads_read_class()
        assert actual == expected

    def test_loads_write_class(self, dss):
        expected = 2
        dss.loads_write_class(expected)
        actual = dss.loads_read_class()
        assert actual == expected

    def test_loads_read_model(self, dss):
        expected = 1
        actual = dss.loads_read_model()
        assert actual == expected

    def test_loads_write_model(self, dss):
        expected = 2
        dss.loads_write_model(expected)
        actual = dss.loads_read_model()
        assert actual == expected

    def test_loads_read_num_cust(self, dss):
        expected = 1
        actual = dss.loads_read_num_cust()
        assert actual == expected

    def test_loads_write_num_cust(self, dss):
        expected = 12
        dss.loads_write_num_cust(expected)
        actual = dss.loads_read_num_cust()
        assert actual == expected

    def test_loads_read_status(self, dss):
        expected = 0
        actual = dss.loads_read_status()
        assert actual == expected

    def test_loads_write_status(self, dss):
        expected = 1
        dss.loads_write_status(expected)
        actual = dss.loads_read_status()
        assert actual == expected

    def test_loads_read_is_delta(self, dss):
        expected = 1
        actual = dss.loads_read_is_delta()
        assert actual == expected

    def test_loads_write_is_delta(self, dss):
        expected = 0
        dss.loads_write_is_delta(expected)
        actual = dss.loads_read_is_delta()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_loads_read_name(self, dss):
        expected = '671'
        actual = dss.loads_read_name()
        assert actual == expected

    def test_loads_write_name(self, dss):
        expected = '634a'
        dss.loads_write_name(expected)
        actual = dss.loads_read_name()
        assert actual == expected

    def test_loads_read_cvr_curve(self, dss):
        expected = ''
        actual = dss.loads_read_cvr_curve()
        assert actual == expected

    def test_loads_write_cvr_curve(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Test'
        dss.loads_write_cvr_curve(expected)
        actual = dss.loads_read_cvr_curve()
        assert actual == expected

    def test_loads_read_daily(self, dss):
        expected = ''
        actual = dss.loads_read_daily()
        assert actual == expected

    def test_loads_write_daily(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Test'
        dss.loads_write_daily(expected)
        actual = dss.loads_read_daily()
        assert actual == expected

    def test_loads_read_duty(self, dss):
        expected = ''
        actual = dss.loads_read_duty()
        assert actual == expected

    def test_loads_read_spectrum(self, dss):
        expected = 'defaultload'
        actual = dss.loads_read_spectrum()
        assert actual == expected

    def test_loads_write_spectrum(self, dss):
        dss.text("New Spectrum.Test "
                      "NumHarm=7 "
                      "harmonic=(1, 3, 5, 7, 9, 11, 13, ) "
                      "%mag=(100, 1.5, 20, 14, 1, 9, 7, ) "
                      "angle=(0, 180, 180, 180, 180, 180, 180, )")
        expected = 'Test'
        dss.loads_write_spectrum(expected)
        actual = dss.loads_read_spectrum()
        assert actual == expected

    def test_loads_read_yearly(self, dss):
        expected = ''
        actual = dss.loads_read_yearly()
        assert actual == expected

    def test_loads_write_yearly(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Test'
        dss.loads_write_yearly(expected)
        actual = dss.loads_read_yearly()
        assert actual == expected

    def test_loads_read_growth(self, dss):
        expected = ''
        actual = dss.loads_read_growth()
        assert actual == expected

    def test_loads_write_growth(self, dss):
        dss.text("New GrowthShape.default npts=2 year=(1, 20, ) mult=(1.025, 1.025, )")
        expected = 'default'
        dss.loads_write_growth(expected)
        actual = dss.loads_read_growth()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_loads_read_kv(self, dss):
        expected = 4.16
        actual = dss.loads_read_kv()
        assert actual == expected

    def test_loads_write_kv(self, dss):
        expected = 0.48
        dss.loads_write_kv(expected)
        actual = dss.loads_read_kv()
        assert actual == expected

    def test_loads_read_kw(self, dss):
        expected = 1155
        actual = dss.loads_read_kw()
        assert actual == expected

    def test_loads_write_kw(self, dss):
        expected = 1000
        dss.loads_write_kw(expected)
        actual = dss.loads_read_kw()
        assert actual == expected

    def test_loads_read_kvar(self, dss):
        expected = 660
        actual = dss.loads_read_kvar()
        assert actual == expected

    def test_loads_write_kvar(self, dss):
        expected = 600
        dss.loads_write_kvar(expected)
        actual = dss.loads_read_kvar()
        assert actual == expected

    def test_loads_read_pf(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = 0.8682431421244591
            actual = dss.loads_read_pf()
            assert actual == expected
        else:
            assert 1 == 1

    def test_loads_write_pf(self, dss):
        expected = 1
        dss.loads_write_pf(expected)
        actual = dss.loads_read_pf()
        assert actual == expected

    def test_loads_read_pct_mean(self, dss):
        expected = 50
        actual = dss.loads_read_pct_mean()
        assert actual == expected

    def test_loads_write_pct_mean(self, dss):
        expected = 123
        dss.loads_write_pct_mean(expected)
        actual = dss.loads_read_pct_mean()
        assert actual == expected

    def test_loads_read_pct_std_dev(self, dss):
        expected = 10
        actual = dss.loads_read_pct_std_dev()
        assert actual == expected

    def test_loads_write_pct_std_dev(self, dss):
        expected = 123
        dss.loads_write_pct_std_dev(expected)
        actual = dss.loads_read_pct_std_dev()
        assert actual == expected

    def test_loads_read_pct_series_rl(self, dss):
        expected = 50
        actual = dss.loads_read_pct_series_rl()
        assert actual == expected

    def test_loads_write_pct_series_rl(self, dss):
        expected = 123
        dss.loads_write_pct_series_rl(expected)
        actual = dss.loads_read_pct_series_rl()
        assert actual == expected

    def test_loads_read_allocation_factor(self, dss):
        expected = 0.5
        actual = dss.loads_read_allocation_factor()
        assert actual == expected

    def test_loads_write_allocation_factor(self, dss):
        expected = 123
        dss.loads_write_allocation_factor(expected)
        actual = dss.loads_read_allocation_factor()
        assert actual == expected

    def test_loads_read_c_factor(self, dss):
        expected = 4
        actual = dss.loads_read_c_factor()
        assert actual == expected

    def test_loads_write_c_factor(self, dss):
        expected = 123
        dss.loads_write_c_factor(expected)
        actual = dss.loads_read_c_factor()
        assert actual == expected

    def test_loads_read_cvr_watts(self, dss):
        expected = 1
        actual = dss.loads_read_cvr_watts()
        assert actual == expected

    def test_loads_write_cvr_watts(self, dss):
        expected = 123
        dss.loads_write_cvr_watts(expected)
        actual = dss.loads_read_cvr_watts()
        assert actual == expected

    def test_loads_read_cvr_vars(self, dss):
        expected = 2
        actual = dss.loads_read_cvr_vars()
        assert actual == expected

    def test_loads_write_cvr_vars(self, dss):
        expected = 123
        dss.loads_write_cvr_vars(expected)
        actual = dss.loads_read_cvr_vars()
        assert actual == expected

    def test_loads_read_kva(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = (dss.loads_read_kw() ** 2 + dss.loads_read_kvar() ** 2) ** (1 / 2)
            actual = dss.loads_read_kva()
            assert actual == expected
        else:
            assert 1 == 1

    def test_loads_write_kva(self, dss):
        expected = 1500
        dss.loads_write_kva(expected)
        actual = dss.loads_read_kva()
        assert actual == expected

    def test_loads_read_kwh(self, dss):
        expected = 0
        actual = dss.loads_read_kwh()
        assert actual == expected

    def test_loads_write_kwh(self, dss):
        expected = 123
        dss.loads_write_kwh(expected)
        actual = dss.loads_read_kwh()
        assert actual == expected

    def test_loads_read_kwh_days(self, dss):
        expected = 30
        actual = dss.loads_read_kwh_days()
        assert actual == expected

    def test_loads_write_kwh_days(self, dss):
        expected = 60
        dss.loads_write_kwh_days(expected)
        actual = dss.loads_read_kwh_days()
        assert actual == expected

    def test_loads_read_r_neut(self, dss):
        expected = -1
        actual = dss.loads_read_r_neut()
        assert actual == expected

    def test_loads_write_r_neut(self, dss):
        expected = 60.0
        dss.loads_write_r_neut(expected)
        actual = dss.loads_read_r_neut()
        assert actual == expected

    def test_loads_read_x_neut(self, dss):
        expected = 0
        actual = dss.loads_read_x_neut()
        assert actual == expected

    def test_loads_write_x_neut(self, dss):
        expected = 60.0
        dss.loads_write_x_neut(expected)
        actual = dss.loads_read_x_neut()
        assert actual == expected

    def test_loads_read_vmax_pu(self, dss):
        expected = 1.05
        actual = dss.loads_read_vmax_pu()
        assert actual == expected

    def test_loads_write_vmax_pu(self, dss):
        expected = 1.1
        dss.loads_write_vmax_pu(expected)
        actual = dss.loads_read_vmax_pu()
        assert actual == expected

    def test_loads_read_vmin_pu(self, dss):
        expected = 0.95
        actual = dss.loads_read_vmin_pu()
        assert actual == expected

    def test_loads_write_vmin_pu(self, dss):
        expected = 0.9
        dss.loads_write_vmin_pu(expected)
        actual = dss.loads_read_vmin_pu()
        assert actual == expected

    def test_loads_read_vmin_emerg(self, dss):
        expected = 0
        actual = dss.loads_read_vmin_emerg()
        assert actual == expected

    def test_loads_write_vmin_emerg(self, dss):
        expected = 0.5
        dss.loads_write_vmin_emerg(expected)
        actual = dss.loads_read_vmin_emerg()
        assert actual == expected

    def test_loads_read_vmin_norm(self, dss):
        expected = 0
        actual = dss.loads_read_vmin_norm()
        assert actual == expected

    def test_loads_write_vmin_norm(self, dss):
        expected = 0.8
        dss.loads_write_vmin_norm(expected)
        actual = dss.loads_read_vmin_norm()
        assert actual == expected

    def test_loads_read_xfkva(self, dss):
        expected = 0
        actual = dss.loads_read_xfkva()
        assert actual == expected

    def test_loads_write_xfkva(self, dss):
        expected = 123.1
        dss.loads_write_xfkva(expected)
        actual = dss.loads_read_xfkva()
        assert actual == expected

    def test_loads_read_rel_weight(self, dss):
        expected = 1
        actual = dss.loads_read_rel_weight()
        assert actual == expected

    def test_loads_write_rel_weight(self, dss):
        expected = 123.1
        dss.loads_write_rel_weight(expected)
        actual = dss.loads_read_rel_weight()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_loads_all_names(self, dss):
        expected = ['671', '634a', '634b', '634c', '645', '646', '692',
                    '675a', '675b', '675c', '611', '652', '670a', '670b',
                    '670c']
        actual = dss.loads_all_names()
        assert actual == expected

    def test_loads_read_zipv(self, dss):
        expected = []
        actual = dss.loads_read_zipv()
        assert actual == expected

    def test_loads_write_zipv(self, dss):
        expected = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
        dss.loads_write_zipv(str(expected))
        actual = dss.loads_read_zipv()
        assert actual == expected
