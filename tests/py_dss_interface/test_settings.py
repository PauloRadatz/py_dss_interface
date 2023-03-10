# -*- coding: utf-8 -*-
# @Time     : 09/07/2021 02:30 AM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_settings.py
# @Software : VSCode

import pytest


class TestSettings13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.solution.solve()

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_settings_read_allow_duplicates(self, dss):
        expected = 0
        actual = dss.settings.allow_duplicates
        assert actual == expected

    def test_settings_write_allow_duplicates(self, dss):
        expected = 1
        dss.settings.allow_duplicates = expected
        actual = dss.settings.allow_duplicates
        assert actual == expected

    def test_settings_read_zone_lock(self, dss):
        expected = 0
        actual = dss.settings.zone_lock
        assert actual == expected

    def test_settings_write_zone_lock(self, dss):
        expected = 1
        dss.settings.zone_lock = expected
        actual = dss.settings.zone_lock
        assert actual == expected

    def test_settings_read_ckt_model(self, dss):
        expected = 1
        actual = dss.settings.ckt_model
        assert actual == expected

    def test_settings_write_ckt_model(self, dss):
        expected = 2
        dss.settings.ckt_model = expected
        actual = dss.settings.ckt_model
        assert actual == expected

    def test_settings_read_trapezoidal(self, dss):
        expected = 0
        actual = dss.settings.trapezoidal
        assert actual == expected

    def test_settings_write_trapezoidal(self, dss):
        expected = 1
        dss.settings.trapezoidal = expected
        actual = dss.settings.trapezoidal
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    # def test_settings_allocation_factors(self, dss):
    #     # TODO: It works, but shows a OpenDSS error message (Allocation Factor must be greater than zero.)
    #     expected = 0
    #     actual = dss.settings.allocation_factors()
    #     assert actual == expected

    def test_settings_read_norm_vmin_pu(self, dss):
        expected = 0.95
        actual = dss.settings.norm_vmin_pu
        assert actual == expected

    def test_settings_write_norm_vmin_pu(self, dss):
        expected = 0.92
        dss.settings.norm_vmin_pu = expected
        actual = dss.settings.norm_vmin_pu
        assert actual == expected

    def test_settings_read_norm_vmax_pu(self, dss):
        expected = 1.05
        actual = dss.settings.norm_vmax_pu
        assert actual == expected

    def test_settings_write_norm_vmax_pu(self, dss):
        expected = 1.15
        dss.settings.norm_vmax_pu = expected
        actual = dss.settings.norm_vmax_pu
        assert actual == expected

    def test_settings_read_emerg_vmin_pu(self, dss):
        expected = 0.9
        actual = dss.settings.emerg_vmin_pu
        assert actual == expected

    def test_settings_write_emerg_vmin_pu(self, dss):
        expected = 0.8
        dss.settings.emerg_vmin_pu = expected
        actual = dss.settings.emerg_vmin_pu
        assert actual == expected

    def test_settings_read_emerg_vmax_pu(self, dss):
        expected = 1.08
        actual = dss.settings.emerg_vmax_pu
        assert actual == expected

    def test_settings_write_emerg_vmax_pu(self, dss):
        expected = 1.2
        dss.settings.emerg_vmax_pu = expected
        actual = dss.settings.emerg_vmax_pu
        assert actual == expected

    def test_settings_read_ue_weight(self, dss):
        expected = 1
        actual = dss.settings.ue_weight
        assert actual == expected

    def test_settings_write_ue_weight(self, dss):
        expected = 2
        dss.settings.ue_weight = expected
        actual = dss.settings.ue_weight
        assert actual == expected

    def test_settings_read_loss_weight(self, dss):
        expected = 1
        actual = dss.settings.loss_weight
        assert actual == expected

    def test_settings_write_loss_weight(self, dss):
        expected = 1
        dss.settings.loss_weight = expected
        actual = dss.settings.loss_weight
        assert actual == expected

    def test_settings_read_price_signal(self, dss):
        expected = 25
        actual = dss.settings.price_signal
        assert actual == expected

    def test_settings_write_price_signal(self, dss):
        expected = 1
        dss.settings.price_signal = expected
        actual = dss.settings.price_signal
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_settings_read_auto_bus_list(self, dss):
        expected = ''
        actual = dss.settings.auto_bus_list
        assert actual == expected

    def test_settings_write_auto_bus_list(self, dss):
        expected = 'test'
        dss.settings.auto_bus_list = expected
        actual = dss.settings.auto_bus_list
        assert actual == expected

    def test_settings_read_price_curve(self, dss):
        expected = ''
        actual = dss.settings.price_curve
        assert actual == expected

    def test_settings_write_price_curve(self, dss):
        expected = "default"
        dss.settings.price_curve = expected
        actual = dss.settings.price_curve
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_settings_read_ue_regs(self, dss):
        expected = [10]
        actual = dss.settings.ue_regs
        assert actual == expected

    def test_settings_write_ue_regs(self, dss):
        expected = [13]
        # TODO: To works must convert the list to string
        # dss.settings.ue_regs = expected
        # actual = dss.settings.ue_regs
        # assert actual == expected

    def test_settings_read_loss_regs(self, dss):
        expected = [13]
        actual = dss.settings.loss_regs
        assert actual == expected

    def test_settings_write_loss_regs(self, dss):
        expected = [10]
        # TODO: To works must convert the list to string
        # dss.settings.loss_regs = expected
        # actual = dss.settings.loss_regs
        # assert actual == expected

    def test_settings_read_voltage_bases(self, dss):
        expected = [115, 4.16, .48]
        actual = dss.settings.voltage_bases
        assert actual == expected

    def test_settings_write_voltage_bases(self, dss):
        expected = [112, 4.13, 0.38]
        # TODO: To works must convert the list to string - not anymore
        # dss.settings.voltage_bases = expected
        # actual = dss.settings.voltage_bases
        # assert actual == expected
