# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 06:40 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_pdelements.py
# @Software : VSCode

import pytest


class TestSensors13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.solution_solve()
        self.dss.pdelements_write_name("Line.671692")

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_pdelements_count(self):
        expected = 19
        actual = self.dss.pdelements_count()
        assert expected == actual

    def test_pdelements_first(self):
        expected = 1
        actual = self.dss.pdelements_first()
        assert expected == actual

    def test_pdelements_next(self):
        expected = 1
        self.dss.pdelements_first()
        actual = self.dss.pdelements_next()
        assert expected == actual

    def test_pdelements_is_shunt(self):
        expected = 1
        self.dss.pdelements_write_name('capacitor.cap1')
        actual = self.dss.pdelements_is_shunt()
        assert expected == actual

    def test_pdelements_num_customers(self):
        expected = 1
        actual = self.dss.pdelements_num_customers()
        assert expected == actual

    def test_pdelements_total_customers(self):
        expected = 4
        actual = self.dss.pdelements_total_customers()
        assert expected == actual

    def test_pdelements_parent_pd_element(self):
        expected = 3
        actual = self.dss.pdelements_parent_pd_element()
        assert expected == actual

    def test_pdelements_from_terminal(self):
        expected = 1
        actual = self.dss.pdelements_from_terminal()
        assert expected == actual

    def test_pdelements_section_id(self):
        expected = 0
        actual = self.dss.pdelements_section_id()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_pdelements_read_fault_rate(self):
        expected = 0.1
        actual = self.dss.pdelements_read_fault_rate()
        assert expected == actual

    def test_pdelements_write_fault_rate(self):
        expected = 0.5
        self.dss.pdelements_write_fault_rate(expected)
        actual = self.dss.pdelements_read_fault_rate()
        assert expected == actual

    def test_pdelements_read_pct_permanent(self):
        expected = 20.0
        actual = self.dss.pdelements_read_pct_permanent()
        assert expected == actual

    def test_pdelements_write_pct_permanent(self):
        expected = 30
        self.dss.pdelements_write_pct_permanent(expected)
        actual = self.dss.pdelements_read_pct_permanent()
        assert expected == actual

    def test_pdelements_lambda(self):
        expected = 0
        actual = self.dss.pdelements_lambda()
        assert expected == actual

    def test_pdelements_accumulated_l(self):
        expected = 0
        actual = self.dss.pdelements_accumulated_l()
        assert expected == actual

    def test_pdelements_repair_time(self):
        expected = 3
        actual = self.dss.pdelements_repair_time()
        assert expected == actual

    def test_pdelements_total_miles(self):
        expected = 0
        actual = self.dss.pdelements_total_miles()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_pdelements_read_name(self):
        expected = 'Line.671692'
        actual = self.dss.pdelements_read_name()
        assert expected == actual

    def test_pdelements_write_name(self):
        expected = 'Capacitor.cap2'
        self.dss.pdelements_write_name(expected)
        actual = self.dss.pdelements_read_name()
        assert expected == actual