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
        dss.solution.solve()

        dss.capcontrols.name = "CAPBank2C_Ctrl"

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_first(self, dss):
        expected = 1
        actual = dss.capcontrols.first()
        assert actual == expected

    def test_next(self, dss):
        expected = 2
        dss.capcontrols.first()
        actual = dss.capcontrols.next()
        assert actual == expected

    def test_count(self, dss):
        expected = 3
        actual = dss.capcontrols.count
        assert actual == expected

    def test_read_mode(self, dss):
        expected = 2
        actual = dss.capcontrols.mode
        assert actual == expected

    def test_write_mode(self, dss):
        expected = 3
        dss.capcontrols.mode = expected
        actual = dss.capcontrols.mode
        assert actual == expected

    def test_read_monitored_term(self, dss):
        expected = 1
        actual = dss.capcontrols.monitored_term
        assert actual == expected

    def test_write_monitored_term(self, dss):
        expected = 1
        dss.capcontrols.monitored_term = dss, expected
        actual = dss.capcontrols.monitored_term
        assert actual == expected

    def test_read_use_volt_override(self, dss):
        actual = dss.capcontrols.use_volt_override
        assert actual is None

    def test_write_use_volt_override(self, dss):
        dss.capcontrols.use_volt_override = dss, 10
        actual = dss.capcontrols.use_volt_override
        assert actual is None

    # ===================================================================
    # String methods
    # ===================================================================
    def test_read_name(self, dss):
        expected = 'CAPBank2C_Ctrl'.lower()
        actual = dss.capcontrols.name
        assert actual == expected

    def test_write_name(self, dss):
        expected = 'CAPBank2B_Ctrl'.lower()
        dss.capcontrols.name = expected
        actual = dss.capcontrols.name
        assert actual == expected

    def test_read_capacitor(self, dss):
        expected = 'CAPBank2C'.lower()
        actual = dss.capcontrols.controlled_capacitor
        assert actual == expected

    def test_write_capacitor(self, dss):
        expected = 'CAPBank2B'.lower()
        dss.capcontrols.controlled_capacitor = expected
        actual = dss.capcontrols.controlled_capacitor
        assert actual == expected

    def test_read_monitored_obj(self, dss):
        expected = 'line.670671'
        actual = dss.capcontrols.monitored_object
        assert actual == expected

    def test_write_monitored_obj(self, dss):
        expected = 'line.671680'
        dss.capcontrols.monitored_object = expected
        actual = dss.capcontrols.monitored_object
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_read_ct_ratio(self, dss):
        expected = 1.0
        actual = dss.capcontrols.ct_ratio
        assert actual == expected

    def test_write_ct_ratio(self, dss):
        expected = 1.0
        dss.capcontrols.ct_ratio = expected
        actual = dss.capcontrols.ct_ratio
        assert actual == expected

    def test_read_pt_ratio(self, dss):
        expected = 1.0
        actual = dss.capcontrols.pt_ratio
        assert actual == expected

    def test_write_pt_ratio(self, dss):
        expected = 1.0
        dss.capcontrols.pt_ratio = expected
        actual = dss.capcontrols.pt_ratio
        assert actual == expected

    def test_read_on_setting(self, dss):
        expected = 150.0
        actual = dss.capcontrols.on_setting
        assert actual == expected

    def test_write_on_setting(self, dss):
        expected = 150.0
        dss.capcontrols.on_setting = expected
        actual = dss.capcontrols.on_setting
        assert actual == expected

    def test_read_off_setting(self, dss):
        expected = -225
        actual = dss.capcontrols.off_setting
        assert actual == expected

    def test_write_off_setting(self, dss):
        expected = 150.0
        dss.capcontrols.off_setting = expected
        actual = dss.capcontrols.off_setting
        assert actual == expected

    def test_read_vmax(self, dss):
        expected = 7740
        actual = dss.capcontrols.vmax
        assert actual == expected

    def test_write_vmax(self, dss):
        expected = 8000
        dss.capcontrols.vmax = expected
        actual = dss.capcontrols.vmax
        assert actual == expected

    def test_read_vmin(self, dss):
        expected = 7110
        actual = dss.capcontrols.vmin
        assert actual == expected

    def test_write_vmin(self, dss):
        expected = 7000
        dss.capcontrols.vmin = expected
        actual = dss.capcontrols.vmin
        assert actual == expected

    def test_read_delay(self, dss):
        expected = 102
        actual = dss.capcontrols.delay
        assert actual == expected

    def test_write_delay(self, dss):
        expected = 105
        dss.capcontrols.delay = expected
        actual = dss.capcontrols.delay
        assert actual == expected

    def test_read_delay_off(self, dss):
        expected = 102
        actual = dss.capcontrols.delay_off
        assert actual == expected

    def test_write_delay_off(self, dss):
        expected = 105
        dss.capcontrols.delay_off = expected
        actual = dss.capcontrols.delay_off
        assert actual == expected

    def test_read_dead_time(self, dss):
        expected = 300
        actual = dss.capcontrols.dead_time
        assert actual == expected

    def test_write_dead_time(self, dss):
        expected = 10
        dss.capcontrols.dead_time = expected
        actual = dss.capcontrols.dead_time
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_all_names(self, dss):
        expected = [x.lower() for x in ['CAPBank2A_Ctrl', 'CAPBank2B_Ctrl', 'CAPBank2C_Ctrl']]
        actual = dss.capcontrols.names
        assert actual == expected
