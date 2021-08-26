# -*- coding: utf-8 -*-
# @Time    : 6/25/2021 2:28 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_capacitors.py
# @Software: PyCharm


import pytest


class TestBus13Capacitors:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.circuit_set_active_bus('692')

    def test_capacitors_read_num_steps(self):
        expected = 1
        actual = self.dss.capacitors_read_num_steps()
        assert actual == expected

    def test_capacitors_write_num_steps(self):
        self.dss.capacitors_write_num_steps(5)
        expected = 5
        actual = self.dss.capacitors_read_num_steps()
        assert actual == expected

    def test_capacitors_available_steps(self):
        expected = 0
        actual = self.dss.capacitors_available_steps()
        assert actual == expected

        self.dss.capacitors_write_num_steps(5)
        self.dss.capacitors_subtract_step()
        self.dss.capacitors_subtract_step()
        expected = 2
        actual = self.dss.capacitors_available_steps()
        assert actual == expected

    def test_capacitors_read_is_delta(self):
        expected = 0
        actual = self.dss.capacitors_read_is_delta()
        assert actual == expected

    def test_capacitors_write_is_delta(self):
        self.dss.capacitors_write_is_delta()
        expected = 1
        actual = self.dss.capacitors_read_is_delta()
        assert actual == expected

    def test_capacitors_count(self):
        expected = 2
        actual = self.dss.capacitors_count()
        assert actual == expected

    def test_capacitors_first(self):
        expected = 1
        actual = self.dss.capacitors_first()
        assert actual == expected

        expected = 'cap1'
        actual = self.dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_next(self):
        expected = 1
        actual = self.dss.capacitors_first()
        assert actual == expected

        expected = 2
        actual = self.dss.capacitors_next()
        assert actual == expected

        expected = 'cap2'
        actual = self.dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_add_step(self):
        self.dss.capacitors_write_num_steps(5)
        expected = 0
        actual = self.dss.capacitors_add_step()
        assert actual == expected

        self.dss.capacitors_subtract_step()
        self.dss.capacitors_subtract_step()
        expected = 2
        actual = self.dss.capacitors_available_steps()
        assert actual == expected

    def test_capacitors_subtract_step(self):
        self.dss.capacitors_write_num_steps(5)
        expected = 1
        actual = self.dss.capacitors_subtract_step()
        assert actual == expected

    def test_capacitors_open(self):
        expected = 0
        actual = self.dss.capacitors_open()
        assert actual == expected

    def test_capacitors_close(self):
        expected = 0
        actual = self.dss.capacitors_close()
        assert actual == expected

    def test_capacitors_read_kv(self):
        expected = 2.4
        actual = self.dss.capacitors_read_kv()
        assert actual == expected

    def test_capacitors_write_kv(self):
        expected = 2.6
        self.dss.capacitors_write_kv(expected)
        actual = self.dss.capacitors_read_kv()
        assert actual == expected

    def test_capacitors_read_kvar(self):
        expected = 100.0
        actual = self.dss.capacitors_read_kvar()
        assert actual == expected

    def test_capacitors_write_kvar(self):
        expected = 50.0 # TODO it only works when we set 50.0. We should correct the type ourselves
        self.dss.capacitors_write_kvar(expected)
        actual = self.dss.capacitors_read_kvar()
        assert actual == expected

    def test_capacitors_read_name(self):
        self.dss.capacitors_first()
        expected = 'cap1'
        actual = self.dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_write_name(self):
        self.dss.capacitors_write_name('cap1')
        expected = 'cap1'
        actual = self.dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_all_names(self):
        expected = ['cap1', 'cap2']
        actual = self.dss.capacitors_all_names()
        assert actual == expected

    def test_capacitors_read_states(self):
        self.dss.capacitors_write_num_steps(5)
        expected = [1, 1, 1, 0, 0]
        self.dss.capacitors_add_step()
        self.dss.capacitors_add_step()
        self.dss.capacitors_add_step()
        self.dss.capacitors_subtract_step()
        self.dss.capacitors_subtract_step()
        actual = self.dss.capacitors_read_states()
        assert actual == expected

    def test_capacitors_write_states(self):
        self.dss.capacitors_write_num_steps(5)
        expected = [0, 1, 0, 1, 1]
        self.dss.capacitors_write_states(self.dss, expected)
        actual = self.dss.capacitors_read_states()
        assert actual == expected
