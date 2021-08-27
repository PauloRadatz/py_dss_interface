# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 08:27 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_capcontrols.py
# @Software: PyCharm


import pytest


class TestCapControls13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("New Capacitor.CAPBank2A  Bus1=670.1  kv=2.4  kvar=300  phases=1 conn=wye")
        self.dss.text("New Capacitor.CAPBank2B  Bus1=670.2  kv=2.4  kvar=300  phases=1 conn=wye")
        self.dss.text("New Capacitor.CAPBank2C  Bus1=670.3  kv=2.4  kvar=300  phases=1 conn=wye")
        self.dss.text(
            "New CapControl.CAPBank2A_Ctrl Capacitor=CAPBank2A element=line.670671 "
            "terminal=1 type=kvar ptratio=1 ctratio=1 ONsetting=150 OFFsetting=-225 "
            "VoltOverride=Y Vmin=7110 Vmax=7740 Delay=100 Delayoff=100")
        self.dss.text(
            "New CapControl.CAPBank2B_Ctrl Capacitor=CAPBank2B element=line.670671 "
            "terminal=1 type=kvar ptratio=1 ctratio=1 ONsetting=150 OFFsetting=-225 "
            "VoltOverride=Y Vmin=7110 Vmax=7740 Delay=101 Delayoff=101")
        self.dss.text(
            "New CapControl.CAPBank2C_Ctrl Capacitor=CAPBank2C element=line.670671 "
            "terminal=1 type=kvar ptratio=1 ctratio=1 ONsetting=150 OFFsetting=-225 "
            "VoltOverride=Y Vmin=7110 Vmax=7740 Delay=102 Delayoff=102")
        self.dss.solution_solve()

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_capcontrols_first(self):
        expected = 1
        actual = self.dss.capcontrols_first()
        assert expected == actual

    def test_capcontrols_next(self):
        expected = 2
        self.dss.capcontrols_first()
        actual = self.dss.capcontrols_next()
        assert expected == actual

    def test_capcontrols_count(self):
        expected = 3
        actual = self.dss.capcontrols_count()
        assert expected == actual

    def test_capcontrols_read_mode(self):
        expected = 2
        actual = self.dss.capcontrols_read_mode()
        assert expected == actual

    def test_capcontrols_write_mode(self):
        expected = 3
        self.dss.capcontrols_write_mode(expected)
        actual = self.dss.capcontrols_read_mode()
        assert expected == actual

    def test_capcontrols_read_monitored_term(self):
        expected = 1
        actual = self.dss.capcontrols_read_monitored_term()
        assert expected == actual

    def test_capcontrols_write_monitored_term(self):
        expected = 1
        self.dss.capcontrols_write_monitored_term(self.dss, expected)
        actual = self.dss.capcontrols_read_monitored_term()
        assert expected == actual

    def test_capcontrols_read_use_volt_override(self):
        expected = None
        actual = self.dss.capcontrols_read_use_volt_override()
        assert expected == actual

    def test_capcontrols_write_use_volt_override(self):
        expected = None
        self.dss.capcontrols_write_use_volt_override(self.dss, 10)
        actual = self.dss.capcontrols_read_use_volt_override()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_capcontrols_read_name(self):
        expected = 'CAPBank2C_Ctrl'.lower()
        actual = self.dss.capcontrols_read_name()
        assert expected == actual

    def test_capcontrols_write_name(self):
        expected = 'CAPBank2B_Ctrl'.lower()
        self.dss.capcontrols_write_name(expected)
        actual = self.dss.capcontrols_read_name()
        assert expected == actual

    def test_capcontrols_read_capacitor(self):
        expected = 'CAPBank2C'.lower()
        actual = self.dss.capcontrols_read_capacitor()
        assert expected == actual

    def test_capcontrols_write_capacitor(self):
        expected = 'CAPBank2B'.lower()
        self.dss.capcontrols_write_capacitor(expected)
        actual = self.dss.capcontrols_read_capacitor()
        assert expected == actual

    def test_capcontrols_read_monitored_obj(self):
        expected = 'line.670671'
        actual = self.dss.capcontrols_read_monitored_obj()
        assert expected == actual

    def test_capcontrols_write_monitored_obj(self):
        expected = 'line.671680'
        self.dss.capcontrols_write_monitored_obj(expected)
        actual = self.dss.capcontrols_read_monitored_obj()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_capcontrols_read_ct_ratio(self):
        expected = 1.0
        actual = self.dss.capcontrols_read_ct_ratio()
        assert expected == actual

    def test_capcontrols_write_ct_ratio(self):
        expected = 1.0
        self.dss.capcontrols_write_ct_ratio(expected)
        actual = self.dss.capcontrols_read_ct_ratio()
        assert expected == actual

    def test_capcontrols_read_pt_ratio(self):
        expected = 1.0
        actual = self.dss.capcontrols_read_pt_ratio()
        assert expected == actual

    def test_capcontrols_write_pt_ratio(self):
        expected = 1.0
        self.dss.capcontrols_write_pt_ratio(expected)
        actual = self.dss.capcontrols_read_pt_ratio()
        assert expected == actual

    def test_capcontrols_read_on_setting(self):
        expected = 150.0
        actual = self.dss.capcontrols_read_on_setting()
        assert expected == actual

    def test_capcontrols_write_on_setting(self):
        expected = 150.0
        self.dss.capcontrols_write_on_setting(expected)
        actual = self.dss.capcontrols_read_on_setting()
        assert expected == actual

    def test_capcontrols_read_off_setting(self):
        expected = -225
        actual = self.dss.capcontrols_read_off_setting()
        assert expected == actual

    def test_capcontrols_write_off_setting(self):
        expected = 150.0
        self.dss.capcontrols_write_off_setting(expected)
        actual = self.dss.capcontrols_read_off_setting()
        assert expected == actual

    def test_capcontrols_read_vmax(self):
        expected = 7740
        actual = self.dss.capcontrols_read_vmax()
        assert expected == actual

    def test_capcontrols_write_vmax(self):
        expected = 8000
        self.dss.capcontrols_write_vmax(expected)
        actual = self.dss.capcontrols_read_vmax()
        assert expected == actual

    def test_capcontrols_read_vmin(self):
        expected = 7110
        actual = self.dss.capcontrols_read_vmin()
        assert expected == actual

    def test_capcontrols_write_vmin(self):
        expected = 7000
        self.dss.capcontrols_write_vmin(expected)
        actual = self.dss.capcontrols_read_vmin()
        assert expected == actual

    def test_capcontrols_read_delay(self):
        expected = 102
        actual = self.dss.capcontrols_read_delay()
        assert expected == actual

    def test_capcontrols_write_delay(self):
        expected = 105
        self.dss.capcontrols_write_delay(expected)
        actual = self.dss.capcontrols_read_delay()
        assert expected == actual

    def test_capcontrols_read_delay_off(self):
        expected = 102
        actual = self.dss.capcontrols_read_delay_off()
        assert expected == actual

    def test_capcontrols_write_delay_off(self):
        expected = 105
        self.dss.capcontrols_write_delay_off(expected)
        actual = self.dss.capcontrols_read_delay_off()
        assert expected == actual

    def test_capcontrols_read_dead_time(self):
        expected = -1
        actual = self.dss.capcontrols_read_dead_time()
        assert expected == actual

    # TODO Paulo: returning -1
    # def test_capcontrols_write_dead_time(self):
    #     expected = 10
    #     self.dss.capcontrols_write_dead_time(expected)
    #     actual = self.dss.capcontrols_read_dead_time()
    #     assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_capcontrols_all_names(self):
        expected = [x.lower() for x in ['CAPBank2A_Ctrl', 'CAPBank2B_Ctrl', 'CAPBank2C_Ctrl']]
        actual = self.dss.capcontrols_all_names()
        assert expected == actual
