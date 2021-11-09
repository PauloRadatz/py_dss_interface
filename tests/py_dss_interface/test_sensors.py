# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 07:06 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_sensors.py
# @Software : VSCode

import pytest


class TestSensors13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("New Sensor.Sensor1 "
                      "element=Load.671 "
                      "terminal=1 "
                      "kVBase=4.16 "
                      "conn=delta ")
        self.dss.text("New Sensor.Sensor2 "
                      "element=Load.611 "
                      "terminal=1 "
                      "kVBase=4.16 "
                      "conn=wye ")
        self.dss.solution_solve()
        self.dss.sensors_write_name("Sensor1")

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_sensors_count(self):
        expected = 2
        actual = self.dss.sensors_count()
        assert actual == expected

    def test_sensors_first(self):
        expected = 1
        actual = self.dss.sensors_first()
        assert actual == expected

    def test_sensors_next(self):
        expected = 2
        actual = self.dss.sensors_next()
        assert actual == expected

    def test_sensors_read_is_delta(self):
        expected = 1
        actual = self.dss.sensors_read_is_delta()
        assert actual == expected

    def test_sensors_write_is_delta(self):
        expected = 0
        self.dss.sensors_write_is_delta(expected)
        actual = self.dss.sensors_read_is_delta()
        assert actual == expected

    def test_sensors_read_reverse_delta(self):
        expected = 0
        actual = self.dss.sensors_read_reverse_delta()
        assert actual == expected

    def test_sensors_write_reverse_delta(self):
        expected = 0
        self.dss.sensors_write_reverse_delta(expected)
        actual = self.dss.sensors_read_reverse_delta()
        assert actual == expected

    def test_sensors_read_metered_terminal(self):
        expected = 1
        actual = self.dss.sensors_read_metered_terminal()
        assert actual == expected

    def test_sensors_write_metered_terminal(self):
        expected = 1
        self.dss.sensors_write_metered_terminal(expected)
        actual = self.dss.sensors_read_metered_terminal()
        assert actual == expected

    def test_sensors_reset(self):
        expected = 0
        actual = self.dss.sensors_reset()
        assert actual == expected

    def test_sensors_reset_all(self):
        expected = 0
        actual = self.dss.sensors_reset_all()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_sensors_read_pct_error(self):
        expected = 1
        actual = self.dss.sensors_read_pct_error()
        assert actual == expected

    def test_sensors_write_pct_error(self):
        expected = 1.0
        self.dss.sensors_write_pct_error(expected)
        actual = self.dss.sensors_read_pct_error()
        assert actual == expected

    def test_sensors_read_weight(self):
        expected = 1.0
        actual = self.dss.sensors_read_weight()
        assert actual == expected

    def test_sensors_write_weight(self):
        expected = 1.0
        self.dss.sensors_write_weight(expected)
        actual = self.dss.sensors_read_weight()
        assert actual == expected

    def test_sensors_read_kv_base(self):
        expected = 4.16
        actual = self.dss.sensors_read_kv_base()
        assert actual == expected

    def test_sensors_write_kv_base(self):
        expected = 0.48
        self.dss.sensors_write_kv_base(expected)
        actual = self.dss.sensors_read_kv_base()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_sensors_read_name(self):
        expected = "sensor1"
        actual = self.dss.sensors_read_name()
        assert actual == expected

    def test_sensors_write_name(self):
        expected = "sensor2"
        self.dss.sensors_write_name(expected)
        actual = self.dss.sensors_read_name()
        assert actual == expected

    def test_sensors_read_metered_element(self):
        expected = "load.671"
        actual = self.dss.sensors_read_metered_element()
        assert actual == expected

    def test_sensors_write_metered_element(self):
        expected = "load.611"
        self.dss.sensors_write_metered_element(expected)
        actual = self.dss.sensors_read_metered_element()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_sensors_all_names(self):
        expected = ["sensor1", "sensor2"]
        actual = self.dss.sensors_all_names()
        assert actual == expected

    def test_sensors_read_currents(self):
        expected = [0.0, 0.0, 0.0]
        actual = self.dss.sensors_read_currents()
        assert actual == expected

    def test_sensors_write_currents(self):
        expected = [1, 1, 1]
        self.dss.sensors_write_currents(str(expected))
        actual = self.dss.sensors_read_currents()
        assert actual == expected

    def test_sensors_read_kvars(self):
        expected = [0.0, 0.0, 0.0]
        actual = self.dss.sensors_read_kvars()
        assert actual == expected

    def test_sensors_write_kvars(self):
        expected = [0.0, 0.0, 0.0]
        self.dss.sensors_write_kvars(str(expected))
        actual = self.dss.sensors_read_kvars()
        assert actual == expected

    def test_sensors_read_kws(self):
        expected = [0.0, 0.0, 0.0]
        actual = self.dss.sensors_read_kws()
        assert actual == expected

    def test_sensors_write_kws(self):
        expected = [10, 10, 10]
        self.dss.sensors_write_kws(str(expected))
        actual = self.dss.sensors_read_kws()
        assert actual == expected
