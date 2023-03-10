# -*- coding: utf-8 -*-
# @Time    : 6/25/2021 2:28 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_capacitors.py
# @Software: PyCharm


import pytest


class TestBus13Capacitors:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.circuit.set_active_element("capacitor.cap1")
        return dss

    def test_read_num_steps(self, dss):
        expected = 1
        actual = dss.capacitors.num_steps
        assert actual == expected

    def test_write_num_steps(self, dss):
        dss.capacitors.num_steps = 5
        expected = 5
        actual = dss.capacitors.num_steps
        assert actual == expected

    def test_available_steps(self, dss):
        expected = 0
        actual = dss.capacitors.available_steps
        assert actual == expected

        dss.capacitors.num_steps = 5
        dss.capacitors.subtract_step()
        dss.capacitors.subtract_step()
        expected = 2
        actual = dss.capacitors.available_steps
        assert actual == expected

    def test_read_is_delta(self, dss):
        expected = 0
        actual = dss.capacitors.is_delta
        assert actual == expected

    def test_write_is_delta(self, dss):
        dss.capacitors.is_delta = 1
        expected = 1
        actual = dss.capacitors.is_delta
        assert actual == expected

    def test_count(self, dss):
        expected = 2
        actual = dss.capacitors.count
        assert actual == expected

    def test_first(self, dss):
        expected = 1
        actual = dss.capacitors.first()
        assert actual == expected

        expected = 'cap1'
        actual = dss.capacitors.name
        assert actual == expected

    def test_next(self, dss):
        expected = 1
        actual = dss.capacitors.first()
        assert actual == expected

        expected = 2
        actual = dss.capacitors.next()
        assert actual == expected

        expected = 'cap2'
        actual = dss.capacitors.name
        assert actual == expected

    def test_add_step(self, dss):
        dss.capacitors.num_steps = 5
        expected = 0
        actual = dss.capacitors.add_step()
        assert actual == expected

        dss.capacitors.subtract_step()
        dss.capacitors.subtract_step()
        expected = 2
        actual = dss.capacitors.available_steps
        assert actual == expected

        expected = 1
        dss.capacitors.add_step()
        actual = dss.capacitors.available_steps
        assert actual == expected

    def test_subtract_step(self, dss):
        dss.capacitors.num_steps = 5
        expected = 1
        actual = dss.capacitors.subtract_step()
        assert actual == expected

    def test_open(self, dss):
        expected = 0
        actual = dss.capacitors.open_all_steps()
        assert actual == expected

    def test_close(self, dss):
        expected = 0
        actual = dss.capacitors.close_all_steps()
        assert actual == expected

    def test_read_kv(self, dss):
        expected = 2.4
        actual = dss.capacitors.kv
        assert actual == expected

    def test_write_kv(self, dss):
        expected = 2.6
        dss.capacitors.kv = expected
        actual = dss.capacitors.kv
        assert actual == expected

    def test_read_kvar(self, dss):
        expected = 100.0
        actual = dss.capacitors.kvar
        assert actual == expected

    def test_write_kvar(self, dss):
        expected = 50.0
        dss.capacitors.kvar = expected
        actual = dss.capacitors.kvar
        assert actual == expected

    def test_read_name(self, dss):
        dss.capacitors.first()
        expected = 'cap1'
        actual = dss.capacitors.name
        assert actual == expected

    def test_write_name(self, dss):
        dss.capacitors.name = 'cap1'
        expected = 'cap1'
        actual = dss.capacitors.name
        assert actual == expected

    def test_all_names(self, dss):
        expected = ['cap1', 'cap2']
        actual = dss.capacitors.names
        assert actual == expected

    def test_read_states(self, dss):
        dss.capacitors.num_steps = 5
        expected = [1, 1, 1, 0, 0]
        dss.capacitors.add_step()
        dss.capacitors.add_step()
        dss.capacitors.add_step()
        dss.capacitors.subtract_step()
        dss.capacitors.subtract_step()
        actual = dss.capacitors.states
        assert actual == expected

    def test_write_states(self, dss):
        dss.capacitors.num_steps = 5
        expected = [0, 1, 0, 1, 1]
        dss.capacitors.states = expected
        actual = dss.capacitors.states
        assert actual == expected
