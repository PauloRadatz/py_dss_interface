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
        dss.circuit_set_active_element("capacitor.cap1")
        return dss

    def test_capacitors_read_num_steps(self, dss):
        expected = 1
        actual = dss.capacitors_read_num_steps()
        assert actual == expected

    def test_capacitors_write_num_steps(self, dss):
        dss.capacitors_write_num_steps(5)
        expected = 5
        actual = dss.capacitors_read_num_steps()
        assert actual == expected

    def test_capacitors_available_steps(self, dss):
        expected = 0
        actual = dss.capacitors_available_steps()
        assert actual == expected

        dss.capacitors_write_num_steps(5)
        dss.capacitors_subtract_step()
        dss.capacitors_subtract_step()
        expected = 2
        actual = dss.capacitors_available_steps()
        assert actual == expected

    def test_capacitors_read_is_delta(self, dss):
        expected = 0
        actual = dss.capacitors_read_is_delta()
        assert actual == expected

    def test_capacitors_write_is_delta(self, dss):
        dss.capacitors_write_is_delta()
        expected = 1
        actual = dss.capacitors_read_is_delta()
        assert actual == expected

    def test_capacitors_count(self, dss):
        expected = 2
        actual = dss.capacitors_count()
        assert actual == expected

    def test_capacitors_first(self, dss):
        expected = 1
        actual = dss.capacitors_first()
        assert actual == expected

        expected = 'cap1'
        actual = dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_next(self, dss):
        expected = 1
        actual = dss.capacitors_first()
        assert actual == expected

        expected = 2
        actual = dss.capacitors_next()
        assert actual == expected

        expected = 'cap2'
        actual = dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_add_step(self, dss):
        dss.capacitors_write_num_steps(5)
        expected = 0
        actual = dss.capacitors_add_step()
        assert actual == expected

        dss.capacitors_subtract_step()
        dss.capacitors_subtract_step()
        expected = 2
        actual = dss.capacitors_available_steps()
        assert actual == expected

    def test_capacitors_subtract_step(self, dss):
        dss.capacitors_write_num_steps(5)
        expected = 1
        actual = dss.capacitors_subtract_step()
        assert actual == expected

    def test_capacitors_open(self, dss):
        expected = 0
        actual = dss.capacitors_open()
        assert actual == expected

    def test_capacitors_close(self, dss):
        expected = 0
        actual = dss.capacitors_close()
        assert actual == expected

    def test_capacitors_read_kv(self, dss):
        expected = 2.4
        actual = dss.capacitors_read_kv()
        assert actual == expected

    def test_capacitors_write_kv(self, dss):
        expected = 2.6
        dss.capacitors_write_kv(expected)
        actual = dss.capacitors_read_kv()
        assert actual == expected

    def test_capacitors_read_kvar(self, dss):
        expected = 100.0
        actual = dss.capacitors_read_kvar()
        assert actual == expected

    def test_capacitors_write_kvar(self, dss):
        expected = 50.0
        dss.capacitors_write_kvar(expected)
        actual = dss.capacitors_read_kvar()
        assert actual == expected

    def test_capacitors_read_name(self, dss):
        dss.capacitors_first()
        expected = 'cap1'
        actual = dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_write_name(self, dss):
        dss.capacitors_write_name('cap1')
        expected = 'cap1'
        actual = dss.capacitors_read_name()
        assert actual == expected

    def test_capacitors_all_names(self, dss):
        expected = ['cap1', 'cap2']
        actual = dss.capacitors_all_names()
        assert actual == expected

    # def test_capacitors_read_states(self, dss):
    #     dss.capacitors_write_num_steps(5)
    #     expected = [1, 1, 1, 0, 0]
    #     dss.capacitors_add_step()
    #     dss.capacitors_add_step()
    #     dss.capacitors_add_step()
    #     dss.capacitors_subtract_step()
    #     dss.capacitors_subtract_step()
    #     actual = dss.capacitors_read_states()
    #     assert actual == expected

    def test_capacitors_write_states(self, dss):
        dss.capacitors_write_num_steps(5)
        expected = [0, 1, 0, 1, 1]
        dss.capacitors_write_states(dss, expected)
        actual = dss.capacitors_read_states()
        assert actual == expected
