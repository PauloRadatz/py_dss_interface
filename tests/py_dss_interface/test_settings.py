# -*- coding: utf-8 -*-
# @Time     : 09/07/2021 02:30 AM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_settings.py
# @Software : VSCode

import os
import pytest


class TestSettings13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.solution_solve()

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_settings_read_allow_duplicates(self):
        expected = 0
        actual = self.dss.settings_read_allow_duplicates()
        assert expected == actual

    def test_settings_write_allow_duplicates(self):
        expected = 1
        self.dss.settings_write_allow_duplicates(expected)
        actual = self.dss.settings_read_allow_duplicates()
        assert expected == actual

    def test_settings_read_zone_lock(self):
        expected = 0
        actual = self.dss.settings_read_zone_lock()
        assert expected == actual

    def test_settings_write_zone_lock(self):
        expected = 1
        self.dss.settings_write_zone_lock(expected)
        actual = self.dss.settings_read_zone_lock()
        assert expected == actual

    def test_settings_read_ckt_model(self):
        expected = 1
        actual = self.dss.settings_read_ckt_model()
        assert expected == actual

    def test_settings_write_ckt_model(self):
        expected = 2
        self.dss.settings_write_ckt_model(expected)
        actual = self.dss.settings_read_ckt_model()
        assert expected == actual

    def test_settings_read_trapezoidal(self):
        expected = 0
        actual = self.dss.settings_read_trapezoidal()
        assert expected == actual

    def test_settings_write_trapezoidal(self):
        expected = 1
        self.dss.settings_write_trapezoidal(expected)
        actual = self.dss.settings_read_trapezoidal()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_settings_allocation_factors(self):
        # TODO: It works, but shows a OpenDSS error message (Allocation Factor must be greater than zero.)
        expected = 0
        actual = self.dss.settings_allocation_factors()
        assert expected == actual

    def test_settings_read_norm_vmin_pu(self):
        expected = 0.95
        actual = self.dss.settings_read_norm_vmin_pu()
        assert expected == actual

    def test_settings_write_norm_vmin_pu(self):
        expected = 0.92
        self.dss.settings_write_norm_vmin_pu(expected)
        actual = self.dss.settings_read_norm_vmin_pu()
        assert expected == actual

    def test_settings_read_norm_vmax_pu(self):
        expected = 1.05
        actual = self.dss.settings_read_norm_vmax_pu()
        assert expected == actual

    def test_settings_write_norm_vmax_pu(self):
        expected = 1.15
        self.dss.settings_write_norm_vmax_pu(expected)
        actual = self.dss.settings_read_norm_vmax_pu()
        assert expected == actual

    def test_settings_read_emerg_vmin_pu(self):
        expected = 0.9
        actual = self.dss.settings_read_emerg_vmin_pu()
        assert expected == actual

    def test_settings_write_emerg_vmin_pu(self):
        expected = 0.8
        self.dss.settings_write_emerg_vmin_pu(expected)
        actual = self.dss.settings_read_emerg_vmin_pu()
        assert expected == actual

    def test_settings_read_emerg_vmax_pu(self):
        expected = 1.08
        actual = self.dss.settings_read_emerg_vmax_pu()
        assert expected == actual

    def test_settings_write_emerg_vmax_pu(self):
        expected = 1.2
        self.dss.settings_write_emerg_vmax_pu(expected)
        actual = self.dss.settings_read_emerg_vmax_pu()
        assert expected == actual

    def test_settings_read_ue_weight(self):
        expected = 1
        actual = self.dss.settings_read_ue_weight()
        assert expected == actual

    def test_settings_write_ue_weight(self):
        expected = 2
        self.dss.settings_write_ue_weight(expected)
        actual = self.dss.settings_read_ue_weight()
        assert expected == actual

    def test_settings_read_loss_weight(self):
        expected = 1
        actual = self.dss.settings_read_loss_weight()
        assert expected == actual

    def test_settings_write_loss_weight(self):
        expected = 1
        self.dss.settings_write_loss_weight(expected)
        actual = self.dss.settings_read_loss_weight()
        assert expected == actual

    def test_settings_read_price_signal(self):
        expected = 25
        actual = self.dss.settings_read_price_signal()
        assert expected == actual

    def test_settings_write_price_signal(self):
        expected = 1
        self.dss.settings_write_price_signal(expected)
        actual = self.dss.settings_read_price_signal()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_settings_read_auto_bus_list(self):
        expected = ''
        actual = self.dss.settings_read_auto_bus_list()
        assert expected == actual

    def test_settings_write_auto_bus_list(self):
        expected = 'test'
        self.dss.settings_write_auto_bus_list(expected)
        actual = self.dss.settings_read_auto_bus_list()
        assert expected == actual

    def test_settings_read_price_curve(self):
        expected = ''
        actual = self.dss.settings_read_price_curve()
        assert expected == actual

    def test_settings_write_price_curve(self):
        expected = "default"
        self.dss.settings_write_price_curve(expected)
        actual = self.dss.settings_read_price_curve()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_settings_read_ue_regs(self):
        expected = [10]
        actual = self.dss.settings_read_ue_regs()
        assert expected == actual

    def test_settings_write_ue_regs(self):
        expected = [13]
        # TODO: To works must convert the list to string
        self.dss.settings_write_ue_regs(str(expected))
        actual = self.dss.settings_read_ue_regs()
        assert expected == actual

    def test_settings_read_loss_regs(self):
        expected = [13]
        actual = self.dss.settings_read_loss_regs()
        assert expected == actual

    def test_settings_write_loss_regs(self):
        expected = [10]
        # TODO: To works must convert the list to string
        self.dss.settings_write_loss_regs(str(expected))
        actual = self.dss.settings_read_loss_regs()
        assert expected == actual

    def test_settings_read_voltage_bases(self):
        expected = [115, 4.16, .48]
        actual = self.dss.settings_read_voltage_bases()
        assert expected == actual

    def test_settings_write_voltage_bases(self):
        expected = [112, 4.13, 0.38]
        # TODO: To works must convert the list to string
        self.dss.settings_write_voltage_bases(str(expected))
        actual = self.dss.settings_read_voltage_bases()
        assert expected == actual
