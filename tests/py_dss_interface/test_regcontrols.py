# -*- coding: utf-8 -*-
# @Time     : 09/07/2021 02:18 AM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_regcontrols.py
# @Software : VSCode

import pytest


class TestRegcontrols13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.solution.solve()

        dss.regcontrols.name = 'reg1'

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_regcontrols_first(self, dss):
        expected = 1
        actual = dss.regcontrols.first()
        assert actual == expected

    def test_regcontrols_next(self, dss):
        expected = 2
        actual = dss.regcontrols.next()
        assert actual == expected

    def test_regcontrols_read_tap_winding(self, dss):
        expected = 2
        actual = dss.regcontrols.tap_winding
        assert actual == expected

    def test_regcontrols_write_tap_winding(self, dss):
        expected = 1
        dss.regcontrols.tap_winding = expected
        actual = dss.regcontrols.tap_winding
        assert actual == expected

    def test_regcontrols_read_winding(self, dss):
        expected = 2
        actual = dss.regcontrols.winding
        assert actual == expected

    def test_regcontrols_write_winding(self, dss):
        expected = 1
        dss.regcontrols.winding = expected
        actual = dss.regcontrols.winding
        assert actual == expected

    def test_regcontrols_read_is_reversible(self, dss):
        expected = 0
        actual = dss.regcontrols.is_reversible
        assert actual == expected

    def test_regcontrols_write_is_reversible(self, dss):
        expected = 1
        dss.regcontrols.is_reversible = expected
        actual = dss.regcontrols.is_reversible
        assert actual == expected

    def test_regcontrols_read_is_inverse_time(self, dss):
        expected = 0
        actual = dss.regcontrols.is_inverse_time
        assert actual == expected

    def test_regcontrols_write_is_inverse_time(self, dss):
        expected = 1
        dss.regcontrols.is_inverse_time = expected
        actual = dss.regcontrols.is_inverse_time
        assert actual == expected

    def test_regcontrols_read_max_tap_change(self, dss):
        expected = 16
        actual = dss.regcontrols.max_tap_change
        assert actual == expected

    def test_regcontrols_write_max_tap_change(self, dss):
        expected = 12
        dss.regcontrols.max_tap_change = expected
        actual = dss.regcontrols.max_tap_change
        assert actual == expected

    def test_regcontrols_count(self, dss):
        expected = 3
        actual = dss.regcontrols.count
        assert actual == expected

    def test_regcontrols_read_tap_number(self, dss):
        expected = 9
        actual = dss.regcontrols.tap_number
        assert actual == expected

    def test_regcontrols_write_tap_number(self, dss):
        expected = 16
        dss.regcontrols.tap_number = expected
        actual = dss.regcontrols.tap_number
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_regcontrols_read_ct_primary(self, dss):
        expected = 700
        actual = dss.regcontrols.ct_primary
        assert actual == expected

    def test_regcontrols_write_ct_primary(self, dss):
        expected = 600
        dss.regcontrols.ct_primary = expected
        actual = dss.regcontrols.ct_primary
        assert actual == expected

    def test_regcontrols_read_pt_ratio(self, dss):
        expected = 20
        actual = dss.regcontrols.pt_ratio
        assert actual == expected

    def test_regcontrols_write_pt_ratio(self, dss):
        expected = 15
        dss.regcontrols.pt_ratio = expected
        actual = dss.regcontrols.pt_ratio
        assert actual == expected

    def test_regcontrols_read_forward_r(self, dss):
        expected = 3
        actual = dss.regcontrols.forward_r
        assert actual == expected

    def test_regcontrols_write_forward_r(self, dss):
        expected = 4
        dss.regcontrols.forward_r = expected
        actual = dss.regcontrols.forward_r
        assert actual == expected

    def test_regcontrols_read_forward_x(self, dss):
        expected = 9
        actual = dss.regcontrols.forward_x
        assert actual == expected

    def test_regcontrols_write_forward_x(self, dss):
        expected = 8
        dss.regcontrols.forward_x = expected
        actual = dss.regcontrols.forward_x
        assert actual == expected

    def test_regcontrols_read_reverse_r(self, dss):
        expected = 0
        actual = dss.regcontrols.reverse_r
        assert actual == expected

    def test_regcontrols_write_reverse_r(self, dss):
        expected = 5
        dss.regcontrols.reverse_r = expected
        actual = dss.regcontrols.reverse_r
        assert actual == expected

    def test_regcontrols_read_reverse_x(self, dss):
        expected = 0
        actual = dss.regcontrols.reverse_x
        assert actual == expected

    def test_regcontrols_write_reverse_x(self, dss):
        expected = 5
        dss.regcontrols.reverse_x = expected
        actual = dss.regcontrols.reverse_x
        assert actual == expected

    def test_regcontrols_read_delay(self, dss):
        expected = 15
        actual = dss.regcontrols.delay
        assert actual == expected

    def test_regcontrols_write_delay(self, dss):
        expected = 10
        dss.regcontrols.delay = expected
        actual = dss.regcontrols.delay
        assert actual == expected

    def test_regcontrols_read_tap_delay(self, dss):
        expected = 2
        actual = dss.regcontrols.tap_delay
        assert actual == expected

    def test_regcontrols_write_tap_delay(self, dss):
        expected = 1
        dss.regcontrols.tap_delay = expected
        actual = dss.regcontrols.tap_delay
        assert actual == expected

    def test_regcontrols_read_voltage_limit(self, dss):
        expected = 0
        actual = dss.regcontrols.voltage_limit
        assert actual == expected

    def test_regcontrols_write_voltage_limit(self, dss):
        expected = 1
        dss.regcontrols.voltage_limit = expected
        actual = dss.regcontrols.voltage_limit
        assert actual == expected

    def test_regcontrols_read_forward_band(self, dss):
        expected = 2
        actual = dss.regcontrols.forward_band
        assert actual == expected

    def test_regcontrols_write_forward_band(self, dss):
        expected = 1
        dss.regcontrols.forward_band = expected
        actual = dss.regcontrols.forward_band
        assert actual == expected

    def test_regcontrols_read_forward_vreg(self, dss):
        expected = 122
        actual = dss.regcontrols.forward_vreg
        assert actual == expected

    def test_regcontrols_write_forward_vreg(self, dss):
        expected = 111
        dss.regcontrols.forward_vreg = expected
        actual = dss.regcontrols.forward_vreg
        assert actual == expected

    def test_regcontrols_read_reverse_band(self, dss):
        expected = 3
        actual = dss.regcontrols.reverse_band
        assert actual == expected

    def test_regcontrols_write_reverse_band(self, dss):
        expected = 12
        dss.regcontrols.reverse_band = expected
        actual = dss.regcontrols.reverse_band
        assert actual == expected

    def test_regcontrols_read_reverse_vreg(self, dss):
        expected = 120
        actual = dss.regcontrols.reverse_vreg
        assert actual == expected

    def test_regcontrols_write_reverse_vreg(self, dss):
        expected = 110
        dss.regcontrols.reverse_vreg = expected
        actual = dss.regcontrols.reverse_vreg
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_regcontrols_read_name(self, dss):
        expected = 'reg1'
        actual = dss.regcontrols.name
        assert actual == expected

    def test_regcontrols_write_name(self, dss):
        expected = 'reg2'
        dss.regcontrols.name = expected
        actual = dss.regcontrols.name
        assert actual == expected

    def test_regcontrols_read_monitored_bus(self, dss):
        expected = ''
        actual = dss.regcontrols.monitored_bus
        assert actual == expected

    def test_regcontrols_write_monitored_bus(self, dss):
        expected = '672'
        dss.regcontrols.monitored_bus = expected
        actual = dss.regcontrols.monitored_bus
        assert actual == expected

    def test_regcontrols_read_transformer(self, dss):
        expected = 'reg1'
        actual = dss.regcontrols.transformer
        assert actual == expected

    def test_regcontrols_write_transformer(self, dss):
        expected = 'reg2'
        dss.regcontrols.transformer = expected
        actual = dss.regcontrols.transformer
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_regcontrols_all_names(self, dss):
        expected = ['reg1', 'reg2', 'reg3']
        actual = dss.regcontrols.names
        assert actual == expected
