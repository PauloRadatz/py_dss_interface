# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 10:05 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_storages.py
# @Software: PyCharm

import pytest


class TestBus13Storages:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("New Storage.St phases=3 bus1=SourceBus kv=13.8 pf=0.98 kWrated=100 %reserve=20 kva=110 effcurve=Eff "
                 "kWhrated=1000 %stored=70 %idlingkW=10 "
                 "%effcharge=90 %effdischarge=90 state=charging dispmode=default model=1")
        return dss

    def test_first(self, dss):
        expected = 1
        actual = dss.storages.first()
        assert actual == expected

        dss.storages.name = "st"

        # expected = 'st'
        # actual = dss.storages.name
        # assert actual == expected

    def test_next(self, dss):
        expected = 1
        actual = dss.storages.first()
        assert actual == expected

        expected = 0
        actual = dss.storages.next()
        assert actual == expected

    def test_count(self, dss):
        expected = 1
        actual = dss.storages.count
        assert actual == expected

    def test_read_idx(self, dss):
        expected = 1
        actual = dss.storages.idx
        assert actual == expected

    def test_write_idx(self, dss):
        expected = 1
        dss.storages.idx = expected
        actual = dss.storages.idx
        assert actual == expected

    def test_read_state(self, dss):
        expected = -1
        actual = dss.storages.state
        assert actual == expected

    def test_write_state(self, dss):
        expected = 1
        dss.storages.state = expected
        actual = dss.storages.state
        assert actual == expected

    def test_read_control_mode(self, dss):
        expected = 0
        actual = dss.storages.control_mode
        assert actual == expected

    def test_write_control_mode(self, dss):
        expected = 1
        dss.storages.control_mode = expected
        actual = dss.storages.control_mode
        assert actual == expected

    def test_read_safe_mode(self, dss):
        expected = 0
        actual = dss.storages.safe_mode
        assert actual == expected

    def test_read_var_follow_inverter(self, dss):
        expected = 0
        actual = dss.storages.var_follow_inverter
        assert actual == expected

    def test_write_var_follow_inverter(self, dss):
        expected = 1
        dss.storages.var_follow_inverter = expected
        actual = dss.storages.var_follow_inverter
        assert actual == expected

    def test_names(self, dss):
        expected = ["st"]
        actual = dss.storages.names
        assert actual == expected

    def test_register_names(self, dss):
        expected = ['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Hours', 'Price($)']
        actual = dss.storages.register_names
        assert actual == expected

    def test_register_values(self, dss):
        expected = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = dss.storages.register_values
        assert actual == expected

    def test_read_pu_soc(self, dss):
        expected = 0.7
        actual = dss.storages.pu_soc
        assert actual == expected

    def test_write_pu_soc(self, dss):
        expected = 1
        dss.storages.pu_soc = expected
        actual = dss.storages.pu_soc
        assert actual == expected

    def test_read_amp_limit(self, dss):
        expected = -1
        actual = dss.storages.amp_limit
        assert actual == expected

    def test_write_amp_limit(self, dss):
        expected = 1
        dss.storages.amp_limit = expected
        actual = dss.storages.amp_limit
        assert actual == expected

    def test_read_amp_limit_gain(self, dss):
        expected = 0.8
        actual = dss.storages.amp_limit_gain
        assert actual == expected

    def test_write_amp_limit_gain(self, dss):
        expected = 1
        dss.storages.amp_limit_gain = expected
        actual = dss.storages.amp_limit_gain
        assert actual == expected

    def test_read_charge_trigger(self, dss):
        expected = 0
        actual = dss.storages.charge_trigger
        assert actual == expected

    def test_write_charge_trigger(self, dss):
        expected = 1
        dss.storages.charge_trigger = expected
        actual = dss.storages.charge_trigger
        assert actual == expected

    def test_read_discharge_trigger(self, dss):
        expected = 0
        actual = dss.storages.discharge_trigger
        assert actual == expected

    def test_write_discharge_trigger(self, dss):
        expected = 1
        dss.storages.discharge_trigger = expected
        actual = dss.storages.discharge_trigger
        assert actual == expected

    def test_read_eff_charge(self, dss):
        expected = 90
        actual = dss.storages.eff_charge
        assert actual == expected

    def test_write_eff_charge(self, dss):
        expected = 1
        dss.storages.eff_charge = expected
        actual = dss.storages.eff_charge
        assert actual == expected

    def test_read_eff_discharge(self, dss):
        expected = 90
        actual = dss.storages.eff_discharge
        assert actual == expected

    def test_write_eff_discharge(self, dss):
        expected = 1
        dss.storages.eff_discharge = expected
        actual = dss.storages.eff_discharge
        assert actual == expected

    def test_read_kp(self, dss):
        expected = 0.01
        actual = dss.storages.kp
        assert actual == expected

    def test_write_kp(self, dss):
        expected = 1
        dss.storages.kp = expected
        actual = dss.storages.kp
        assert actual == expected

    def test_read_kv(self, dss):
        expected = 13.8
        actual = dss.storages.kv
        assert actual == expected

    def test_write_kv(self, dss):
        expected = 1
        dss.storages.kv = expected
        actual = dss.storages.kv
        assert actual == expected

    def test_read_kva(self, dss):
        expected = 110
        actual = dss.storages.kva
        assert actual == expected

    def test_write_kva(self, dss):
        expected = 1
        dss.storages.kva = expected
        actual = dss.storages.kva
        assert actual == expected

    def test_read_kvar(self, dss):
        expected = 0
        actual = dss.storages.kvar
        assert actual == expected

    def test_write_kvar(self, dss):
        expected = 1
        dss.storages.kvar = expected
        actual = dss.storages.kvar
        assert actual == expected

    def test_read_kvdc(self, dss):
        expected = 8
        actual = dss.storages.kvdc
        assert actual == expected

    def test_write_kvdc(self, dss):
        expected = 1
        dss.storages.kvdc = expected
        actual = dss.storages.kvdc
        assert actual == expected

    def test_read_kw(self, dss):
        expected = -100
        actual = dss.storages.kw
        assert actual == expected

    def test_write_kw(self, dss):
        expected = 1
        dss.storages.kw = expected
        actual = dss.storages.kw
        assert actual == expected

    def test_read_kwh_rated(self, dss):
        expected = 1000
        actual = dss.storages.kwh_rated
        assert actual == expected

    def test_write_kwh_rated(self, dss):
        expected = 1
        dss.storages.kwh_rated = expected
        actual = dss.storages.kwh_rated
        assert actual == expected

    def test_read_kw_rated(self, dss):
        expected = 100
        actual = dss.storages.kw_rated
        assert actual == expected

    def test_write_kw_rated(self, dss):
        expected = 1
        dss.storages.kw_rated = expected
        actual = dss.storages.kw_rated
        assert actual == expected

    def test_read_limit_current(self, dss):
        expected = 0
        actual = dss.storages.limit_current
        assert actual == expected

    def test_write_limit_current(self, dss):
        expected = 1
        dss.storages.limit_current = expected
        actual = dss.storages.limit_current
        assert actual == expected

    def test_read_pf(self, dss):
        expected = 0.98
        actual = dss.storages.pf
        assert actual == expected

    def test_write_pf(self, dss):
        expected = 1
        dss.storages.pf = expected
        actual = dss.storages.pf
        assert actual == expected

    def test_read_pi_tol(self, dss):
        expected = 0
        actual = dss.storages.pi_tol
        assert actual == expected

    def test_write_pi_tol(self, dss):
        expected = 1
        dss.storages.pi_tol = expected
        actual = dss.storages.pi_tol
        assert actual == expected

    def test_read_safe_voltage(self, dss):
        expected = 80
        actual = dss.storages.safe_voltage
        assert actual == expected

    def test_write_safe_voltage(self, dss):
        expected = 1
        dss.storages.safe_voltage = expected
        actual = dss.storages.safe_voltage
        assert actual == expected

    def test_read_time_charge_trig(self, dss):
        expected = 2
        actual = dss.storages.time_charge_trig
        assert actual == expected

    def test_write_time_charge_trig(self, dss):
        expected = 1
        dss.storages.time_charge_trig = expected
        actual = dss.storages.time_charge_trig
        assert actual == expected

    def test_read_name(self, dss):
        expected = "st"
        actual = dss.storages.name
        assert actual == expected

    def test_write_name(self, dss):
        expected = "st"
        dss.storages.name = expected
        actual = dss.storages.name
        assert actual == expected

