# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 07:06 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_sensors.py
# @Software : VSCode

import pytest


class TestSensors13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("New Sensor.Sensor1 "
                 "element=Load.671 "
                 "terminal=1 "
                 "kVBase=4.16 "
                 "conn=delta ")
        dss.solution.solve()
        dss.sensors.name = "Sensor1"

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_sensors_count(self, dss):
        expected = 1
        actual = dss.sensors._count
        assert actual == expected

    def test_sensors_first(self, dss):
        expected = 1
        actual = dss.sensors.first()
        assert actual == expected

    def test_sensors_next(self, dss):
        dss.text("New Sensor.Sensor2 "
                 "element=Load.611 "
                 "terminal=1 "
                 "kVBase=4.16 "
                 "conn=wye ")
        expected = 2
        dss.sensors.first()
        actual = dss.sensors.next()
        assert actual == expected

    def test_sensors_read_is_delta(self, dss):
        expected = 1
        actual = dss.sensors.is_delta
        assert actual == expected

    def test_sensors_write_is_delta(self, dss):
        expected = 0
        dss.sensors.is_delta = expected
        actual = dss.sensors.is_delta
        assert actual == expected

    def test_sensors_read_reverse_delta(self, dss):
        expected = 0
        actual = dss.sensors.reverse_delta
        assert actual == expected

    def test_sensors_write_reverse_delta(self, dss):
        expected = 0
        dss.sensors.reverse_delta = expected
        actual = dss.sensors.reverse_delta
        assert actual == expected

    def test_sensors_read_metered_terminal(self, dss):
        expected = 1
        actual = dss.sensors.metered_terminal
        assert actual == expected

    def test_sensors_write_metered_terminal(self, dss):
        expected = 1
        dss.sensors.metered_terminal = expected
        actual = dss.sensors.metered_terminal
        assert actual == expected

    def test_sensors_reset(self, dss):
        expected = 0
        actual = dss.sensors.reset()
        assert actual == expected

    def test_sensors_reset_all(self, dss):
        expected = 0
        actual = dss.sensors.reset_all()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_sensors_read_pct_error(self, dss):
        expected = 1
        actual = dss.sensors.pct_error
        assert actual == expected

    def test_sensors_write_pct_error(self, dss):
        expected = 1.0
        dss.sensors.pct_error = expected
        actual = dss.sensors.pct_error
        assert actual == expected

    def test_sensors_read_weight(self, dss):
        expected = 1.0
        actual = dss.sensors.weight
        assert actual == expected

    def test_sensors_write_weight(self, dss):
        expected = 1.0
        dss.sensors._weight = expected
        actual = dss.sensors.weight
        assert actual == expected

    def test_sensors_read_kv_base(self, dss):
        expected = 4.16
        actual = dss.sensors.kv_base
        assert actual == expected

    def test_sensors_write_kv_base(self, dss):
        expected = 0.48
        dss.sensors.kv_base = expected
        actual = dss.sensors.kv_base
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_sensors_read_name(self, dss):
        expected = "sensor1"
        actual = dss.sensors.name
        assert actual == expected

    def test_sensors_write_name(self, dss):
        dss.text("New Sensor.Sensor2 "
                 "element=Load.611 "
                 "terminal=1 "
                 "kVBase=4.16 "
                 "conn=wye ")
        expected = "sensor2"
        dss.sensors.name = expected
        actual = dss.sensors.name
        assert actual == expected

    def test_sensors_read_metered_element(self, dss):
        expected = "load.671"
        actual = dss.sensors.metered_element
        assert actual == expected

    def test_sensors_write_metered_element(self, dss):
        expected = "load.611"
        dss.sensors.metered_element = expected
        actual = dss.sensors.metered_element
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_sensors_all_names(self, dss):
        dss.text("New Sensor.Sensor2 "
                 "element=Load.611 "
                 "terminal=1 "
                 "kVBase=4.16 "
                 "conn=wye ")
        expected = ["sensor1", "sensor2"]
        actual = dss.sensors.names
        assert actual == expected

    def test_sensors_read_currents(self, dss):
        expected = [0.0, 0.0, 0.0]
        actual = dss.sensors.currents
        assert actual == expected

    def test_sensors_write_currents(self, dss):
        expected = [1, 1, 1]
        dss.sensors.currents = expected
        actual = dss.sensors.currents
        assert actual == expected

    def test_sensors_read_kvars(self, dss):
        expected = [0.0, 0.0, 0.0]
        actual = dss.sensors.kvars
        assert actual == expected

    def test_sensors_write_kvars(self, dss):
        expected = [0.0, 0.0, 0.0]
        dss.sensors.kvars = expected
        actual = dss.sensors.kvars
        assert actual == expected

    def test_sensors_read_kws(self, dss):
        expected = [0.0, 0.0, 0.0]
        actual = dss.sensors.kws
        assert actual == expected

    def test_sensors_write_kws(self, dss):
        expected = [10, 10, 10]
        dss.sensors.kws = expected
        actual = dss.sensors.kws
        assert actual == expected
