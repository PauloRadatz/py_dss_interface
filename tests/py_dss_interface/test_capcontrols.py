# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 08:27 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_capcontrols.py
# @Software: PyCharm


import pytest


class TestCapControls13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("New Capacitor.CAPBank2A  Bus1=670.1  kv=2.4  kvar=300  phases=1 conn=wye")
        dss.text("New Capacitor.CAPBank2B  Bus1=670.2  kv=2.4  kvar=300  phases=1 conn=wye")
        dss.text("New Capacitor.CAPBank2C  Bus1=670.3  kv=2.4  kvar=300  phases=1 conn=wye")
        dss.text(
            "New CapControl.CAPBank2A_Ctrl Capacitor=CAPBank2A element=line.670671 "
            "terminal=1 type=kvar ptratio=1 ctratio=1 ONsetting=150 OFFsetting=-225 "
            "VoltOverride=Y Vmin=7110 Vmax=7740 Delay=100 Delayoff=100")
        dss.text(
            "New CapControl.CAPBank2B_Ctrl Capacitor=CAPBank2B element=line.670671 "
            "terminal=1 type=kvar ptratio=1 ctratio=1 ONsetting=150 OFFsetting=-225 "
            "VoltOverride=Y Vmin=7110 Vmax=7740 Delay=101 Delayoff=101")
        dss.text(
            "New CapControl.CAPBank2C_Ctrl Capacitor=CAPBank2C element=line.670671 "
            "terminal=1 type=kvar ptratio=1 ctratio=1 ONsetting=150 OFFsetting=-225 "
            "VoltOverride=Y Vmin=7110 Vmax=7740 Delay=102 Delayoff=102")
        dss.solution_solve()

        dss.write_name("CAPBank2C_Ctrl")

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_capcontrols_first(self, dss):
        expected = 1
        actual = dss.first()
        assert actual == expected

    def test_capcontrols_next(self, dss):
        expected = 2
        dss.first()
        actual = dss.next()
        assert actual == expected

    def test_capcontrols_count(self, dss):
        expected = 3
        actual = dss.count()
        assert actual == expected

    def test_capcontrols_read_mode(self, dss):
        expected = 2
        actual = dss.read_mode()
        assert actual == expected

    def test_capcontrols_write_mode(self, dss):
        expected = 3
        dss.write_mode(expected)
        actual = dss.read_mode()
        assert actual == expected

    def test_capcontrols_read_monitored_term(self, dss):
        expected = 1
        actual = dss.read_monitored_term()
        assert actual == expected

    def test_capcontrols_write_monitored_term(self, dss):
        expected = 1
        dss.write_monitored_term(dss, expected)
        actual = dss.read_monitored_term()
        assert actual == expected

    def test_capcontrols_read_use_volt_override(self, dss):
        actual = dss.read_use_volt_override()
        assert actual is None

    def test_capcontrols_write_use_volt_override(self, dss):
        dss.write_use_volt_override(dss, 10)
        actual = dss.read_use_volt_override()
        assert actual is None

    # ===================================================================
    # String methods
    # ===================================================================
    def test_capcontrols_read_name(self, dss):
        expected = 'CAPBank2C_Ctrl'.lower()
        actual = dss.read_name()
        assert actual == expected

    def test_capcontrols_write_name(self, dss):
        expected = 'CAPBank2B_Ctrl'.lower()
        dss.write_name(expected)
        actual = dss.read_name()
        assert actual == expected

    def test_capcontrols_read_capacitor(self, dss):
        expected = 'CAPBank2C'.lower()
        actual = dss.capacitor_controlled()
        assert actual == expected

    def test_capcontrols_write_capacitor(self, dss):
        expected = 'CAPBank2B'.lower()
        dss.write_capacitor(expected)
        actual = dss.capacitor_controlled()
        assert actual == expected

    def test_capcontrols_read_monitored_obj(self, dss):
        expected = 'line.670671'
        actual = dss.monitored_object()
        assert actual == expected

    def test_capcontrols_write_monitored_obj(self, dss):
        expected = 'line.671680'
        dss.monitored_obj_write(expected)
        actual = dss.monitored_object()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_capcontrols_read_ct_ratio(self, dss):
        expected = 1.0
        actual = dss.ct_ratio_read()
        assert actual == expected

    def test_capcontrols_write_ct_ratio(self, dss):
        expected = 1.0
        dss.ct_ratio_write(expected)
        actual = dss.ct_ratio_read()
        assert actual == expected

    def test_capcontrols_read_pt_ratio(self, dss):
        expected = 1.0
        actual = dss.pt_ratio_read()
        assert actual == expected

    def test_capcontrols_write_pt_ratio(self, dss):
        expected = 1.0
        dss.pt_ratio_write(expected)
        actual = dss.pt_ratio_read()
        assert actual == expected

    def test_capcontrols_read_on_setting(self, dss):
        expected = 150.0
        actual = dss.on_setting_read()
        assert actual == expected

    def test_capcontrols_write_on_setting(self, dss):
        expected = 150.0
        dss.on_setting_write(expected)
        actual = dss.on_setting_read()
        assert actual == expected

    def test_capcontrols_read_off_setting(self, dss):
        expected = -225
        actual = dss.off_setting_read()
        assert actual == expected

    def test_capcontrols_write_off_setting(self, dss):
        expected = 150.0
        dss.off_setting_write(expected)
        actual = dss.off_setting_read()
        assert actual == expected

    def test_capcontrols_read_vmax(self, dss):
        expected = 7740
        actual = dss.vmax_read()
        assert actual == expected

    def test_capcontrols_write_vmax(self, dss):
        expected = 8000
        dss.vmax_write(expected)
        actual = dss.vmax_read()
        assert actual == expected

    def test_capcontrols_read_vmin(self, dss):
        expected = 7110
        actual = dss.vmin_read()
        assert actual == expected

    def test_capcontrols_write_vmin(self, dss):
        expected = 7000
        dss.vmin_write(expected)
        actual = dss.vmin_read()
        assert actual == expected

    def test_capcontrols_read_delay(self, dss):
        expected = 102
        actual = dss.delay_read()
        assert actual == expected

    def test_capcontrols_write_delay(self, dss):
        expected = 105
        dss.delay_write(expected)
        actual = dss.delay_read()
        assert actual == expected

    def test_capcontrols_read_delay_off(self, dss):
        expected = 102
        actual = dss.delay_off_read()
        assert actual == expected

    def test_capcontrols_write_delay_off(self, dss):
        expected = 105
        dss.delay_off_write(expected)
        actual = dss.delay_off_read()
        assert actual == expected

    def test_capcontrols_read_dead_time(self, dss):
        expected = 300
        actual = dss.dead_time_read()
        assert actual == expected

    def test_capcontrols_write_dead_time(self, dss):
        expected = 10
        dss.dead_time_write(expected)
        actual = dss.dead_time_read()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_capcontrols_all_names(self, dss):
        expected = [x.lower() for x in ['CAPBank2A_Ctrl', 'CAPBank2B_Ctrl', 'CAPBank2C_Ctrl']]
        actual = dss.all_names()
        assert actual == expected
