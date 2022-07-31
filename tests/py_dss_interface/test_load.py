# -*- coding: utf-8 -*-
# @Time    : 6/24/2021 10:51 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_load.py
# @Software: PyCharm

import platform

import pytest


class TestLoad13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.name_write('671')

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_loads_count(self, dss):
        expected = 15
        actual = dss.count()
        assert actual == expected

    def test_loads_first(self, dss):
        expected = 1
        actual = dss.first()
        assert actual == expected

    def test_loads_next(self, dss):
        expected = 2
        actual = dss.next()
        assert actual == expected

    def test_loads_read_idx(self, dss):
        expected = 1
        actual = dss.idx_read()
        assert actual == expected

    def test_loads_write_idx(self, dss):
        expected = 2
        dss.idx_write(expected)
        actual = dss.idx_read()
        assert actual == expected

    def test_loads_read_class(self, dss):
        expected = 1
        actual = dss.class_read()
        assert actual == expected

    def test_loads_write_class(self, dss):
        expected = 2
        dss.class_write(expected)
        actual = dss.class_read()
        assert actual == expected

    def test_loads_read_model(self, dss):
        expected = 1
        actual = dss.model_read()
        assert actual == expected

    def test_loads_write_model(self, dss):
        expected = 2
        dss.model_write(expected)
        actual = dss.model_read()
        assert actual == expected

    def test_loads_read_num_cust(self, dss):
        expected = 1
        actual = dss.num_cust_read()
        assert actual == expected

    def test_loads_write_num_cust(self, dss):
        expected = 12
        dss.num_cust_write(expected)
        actual = dss.num_cust_read()
        assert actual == expected

    def test_loads_read_status(self, dss):
        expected = 0
        actual = dss.status_read()
        assert actual == expected

    def test_loads_write_status(self, dss):
        expected = 1
        dss.status_write(expected)
        actual = dss.status_read()
        assert actual == expected

    def test_loads_read_is_delta(self, dss):
        expected = 1
        actual = dss.is_delta()
        assert actual == expected

    def test_loads_write_is_delta(self, dss):
        expected = 0
        dss.is_delta_write(expected)
        actual = dss.is_delta()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_loads_read_name(self, dss):
        expected = '671'
        actual = dss.name_read()
        assert actual == expected

    def test_loads_write_name(self, dss):
        expected = '634a'
        dss.name_write(expected)
        actual = dss.name_read()
        assert actual == expected

    def test_loads_read_cvr_curve(self, dss):
        expected = ''
        actual = dss.cvr_curve_read()
        assert actual == expected

    def test_loads_write_cvr_curve(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Test'
        dss.cvr_curve_write(expected)
        actual = dss.cvr_curve_read()
        assert actual == expected

    def test_loads_read_daily(self, dss):
        expected = ''
        actual = dss.daily_read()
        assert actual == expected

    def test_loads_write_daily(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Test'
        dss.daily_write(expected)
        actual = dss.daily_read()
        assert actual == expected

    def test_loads_read_duty(self, dss):
        expected = ''
        actual = dss.duty_read()
        assert actual == expected

    def test_loads_read_spectrum(self, dss):
        expected = 'defaultload'
        actual = dss.spectrum_read()
        assert actual == expected

    def test_loads_write_spectrum(self, dss):
        dss.text("New Spectrum.Test "
                 "NumHarm=7 "
                 "harmonic=(1, 3, 5, 7, 9, 11, 13, ) "
                 "%mag=(100, 1.5, 20, 14, 1, 9, 7, ) "
                 "angle=(0, 180, 180, 180, 180, 180, 180, )")
        expected = 'Test'
        dss.spectrum_write(expected)
        actual = dss.spectrum_read()
        assert actual == expected

    def test_loads_read_yearly(self, dss):
        expected = ''
        actual = dss.yearly_read()
        assert actual == expected

    def test_loads_write_yearly(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'Test'
        dss.yearly_write(expected)
        actual = dss.yearly_read()
        assert actual == expected

    def test_loads_read_growth(self, dss):
        expected = ''
        actual = dss.growth_read()
        assert actual == expected

    def test_loads_write_growth(self, dss):
        dss.text("New GrowthShape.default npts=2 year=(1, 20, ) mult=(1.025, 1.025, )")
        expected = 'default'
        dss.growth_write(expected)
        actual = dss.growth_read()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_loads_read_kv(self, dss):
        expected = 4.16
        actual = dss.kv_read()
        assert actual == expected

    def test_loads_write_kv(self, dss):
        expected = 0.48
        dss.kv_write(expected)
        actual = dss.kv_read()
        assert actual == expected

    def test_loads_read_kw(self, dss):
        expected = 1155
        actual = dss.kw_read()
        assert actual == expected

    def test_loads_write_kw(self, dss):
        expected = 1000
        dss.kw_write(expected)
        actual = dss.kw_read()
        assert actual == expected

    def test_loads_read_kvar(self, dss):
        expected = 660
        actual = dss.kvar_read()
        assert actual == expected

    def test_loads_write_kvar(self, dss):
        expected = 600
        dss.kvar_write(expected)
        actual = dss.kvar_read()
        assert actual == expected

    def test_loads_read_pf(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = 0.8682431421244591
            actual = dss.pf_read()
            assert actual == expected

    def test_loads_write_pf(self, dss):
        expected = 1
        dss.pf_write(expected)
        actual = dss.pf_read()
        assert actual == expected

    def test_loads_read_pct_mean(self, dss):
        expected = 50
        actual = dss.pct_mean_read()
        assert actual == expected

    def test_loads_write_pct_mean(self, dss):
        expected = 123
        dss.pct_mean_write(expected)
        actual = dss.pct_mean_read()
        assert actual == expected

    def test_loads_read_pct_std_dev(self, dss):
        expected = 10
        actual = dss.pct_std_dev_read()
        assert actual == expected

    def test_loads_write_pct_std_dev(self, dss):
        expected = 123
        dss.pct_std_dev_write(expected)
        actual = dss.pct_std_dev_read()
        assert actual == expected

    def test_loads_read_pct_series_rl(self, dss):
        expected = 50
        actual = dss.rl_read()
        assert actual == expected

    def test_loads_write_pct_series_rl(self, dss):
        expected = 123
        dss.rl_write(expected)
        actual = dss.rl_read()
        assert actual == expected

    def test_loads_read_allocation_factor(self, dss):
        expected = 0.5
        actual = dss.allocation_factor_read()
        assert actual == expected

    def test_loads_write_allocation_factor(self, dss):
        expected = 123
        dss.allocation_factor_write(expected)
        actual = dss.allocation_factor_read()
        assert actual == expected

    def test_loads_read_c_factor(self, dss):
        expected = 4
        actual = dss.c_factor_read()
        assert actual == expected

    def test_loads_write_c_factor(self, dss):
        expected = 123
        dss.c_factor_write(expected)
        actual = dss.c_factor_read()
        assert actual == expected

    def test_loads_read_cvr_watts(self, dss):
        expected = 1
        actual = dss.cvr_watts_read()
        assert actual == expected

    def test_loads_write_cvr_watts(self, dss):
        expected = 123
        dss.cvr_watts_write(expected)
        actual = dss.cvr_watts_read()
        assert actual == expected

    def test_loads_read_cvr_vars(self, dss):
        expected = 2
        actual = dss.cvr_vars_read()
        assert actual == expected

    def test_loads_write_cvr_vars(self, dss):
        expected = 123
        dss.cvr_vars_write(expected)
        actual = dss.cvr_vars_read()
        assert actual == expected

    def test_loads_read_kva(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = (dss.kw_read() ** 2 + dss.kvar_read() ** 2) ** (1 / 2)
            actual = dss.kva_read()
            assert actual == expected

    def test_loads_write_kva(self, dss):
        expected = 1500
        dss.kva_write(expected)
        actual = dss.kva_read()
        assert actual == expected

    def test_loads_read_kwh(self, dss):
        expected = 0
        actual = dss.kwh_read()
        assert actual == expected

    def test_loads_write_kwh(self, dss):
        expected = 123
        dss.kwh_write(expected)
        actual = dss.kwh_read()
        assert actual == expected

    def test_loads_read_kwh_days(self, dss):
        expected = 30
        actual = dss.kwh_days_read()
        assert actual == expected

    def test_loads_write_kwh_days(self, dss):
        expected = 60
        dss.kwh_days_write(expected)
        actual = dss.kwh_days_read()
        assert actual == expected

    def test_loads_read_r_neut(self, dss):
        expected = -1
        actual = dss.r_neut_read()
        assert actual == expected

    def test_loads_write_r_neut(self, dss):
        expected = 60.0
        dss.r_neut_write(expected)
        actual = dss.r_neut_read()
        assert actual == expected

    def test_loads_read_x_neut(self, dss):
        expected = 0
        actual = dss.x_neut_read()
        assert actual == expected

    def test_loads_write_x_neut(self, dss):
        expected = 60.0
        dss.x_neut_write(expected)
        actual = dss.x_neut_read()
        assert actual == expected

    def test_loads_read_vmax_pu(self, dss):
        expected = 1.05
        actual = dss.vmax_pu_read()
        assert actual == expected

    def test_loads_write_vmax_pu(self, dss):
        expected = 1.1
        dss.loads_write_vmax_pu(expected)
        actual = dss.vmax_pu_read()
        assert actual == expected

    def test_loads_read_vmin_pu(self, dss):
        expected = 0.95
        actual = dss.vmin_pu_read()
        assert actual == expected

    def test_loads_write_vmin_pu(self, dss):
        expected = 0.9
        dss.vmin_pu_write(expected)
        actual = dss.vmin_pu_read()
        assert actual == expected

    def test_loads_read_vmin_emerg(self, dss):
        expected = 0
        actual = dss.vmin_emerg_read()
        assert actual == expected

    def test_loads_write_vmin_emerg(self, dss):
        expected = 0.5
        dss.vmin_emerg_write(expected)
        actual = dss.vmin_emerg_read()
        assert actual == expected

    def test_loads_read_vmin_norm(self, dss):
        expected = 0
        actual = dss.vmin_norm_read()
        assert actual == expected

    def test_loads_write_vmin_norm(self, dss):
        expected = 0.8
        dss.vmin_norm_write(expected)
        actual = dss.vmin_norm_read()
        assert actual == expected

    def test_loads_read_xfkva(self, dss):
        expected = 0
        actual = dss.xfkva_read()
        assert actual == expected

    def test_loads_write_xfkva(self, dss):
        expected = 123.1
        dss.xfkva_write(expected)
        actual = dss.xfkva_read()
        assert actual == expected

    def test_loads_read_rel_weight(self, dss):
        expected = 1
        actual = dss.rel_weight_read()
        assert actual == expected

    def test_loads_write_rel_weight(self, dss):
        expected = 123.1
        dss.rel_weight_write(expected)
        actual = dss.rel_weight_read()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_loads_all_names(self, dss):
        expected = ['671', '634a', '634b', '634c', '645', '646', '692',
                    '675a', '675b', '675c', '611', '652', '670a', '670b',
                    '670c']
        actual = dss.names()
        assert actual == expected

    def test_loads_read_zipv(self, dss):
        expected = []
        actual = dss.zipv_read()
        assert actual == expected

    def test_loads_write_zipv(self, dss):
        expected = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
        dss.zipv_write(str(expected))
        actual = dss.zipv_read()
        assert actual == expected
