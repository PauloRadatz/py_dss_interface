# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 06:40 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_pdelements.py
# @Software : VSCode

import pytest


class TestSensors13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.solution.solve()
        dss.pdelements.name = "Line.671692"

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_pdelements_count(self, dss):
        expected = 19
        actual = dss.pdelements.count
        assert actual == expected

    def test_pdelements_first(self, dss):
        expected = 1
        actual = dss.pdelements.first()
        assert actual == expected

    def test_pdelements_next(self, dss):
        expected = 1
        dss.pdelements.first()
        actual = dss.pdelements.next()
        assert actual == expected

    def test_pdelements_is_shunt(self, dss):
        expected = 1
        dss.pdelements.name = 'capacitor.cap1'
        actual = dss.pdelements.is_shunt
        assert actual == expected

    def test_pdelements_num_customers(self, dss):
        expected = 1
        actual = dss.pdelements.num_customers
        assert actual == expected

    def test_pdelements_total_customers(self, dss):
        expected = 4
        actual = dss.pdelements.total_customers
        assert actual == expected

    def test_pdelements_parent_pd_element(self, dss):
        expected = 3
        actual = dss.pdelements.parent_pd_element
        assert actual == expected

    def test_pdelements_from_terminal(self, dss):
        expected = 1
        actual = dss.pdelements.from_terminal
        assert actual == expected

    def test_pdelements_section_id(self, dss):
        expected = 0
        actual = dss.pdelements.section_id
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_pdelements_read_fault_rate(self, dss):
        expected = 0.1
        actual = dss.pdelements.fault_rate
        assert actual == expected

    def test_pdelements_write_fault_rate(self, dss):
        expected = 0.5
        dss.pdelements.fault_rate = expected
        actual = dss.pdelements.fault_rate
        assert actual == expected

    def test_pdelements_read_pct_permanent(self, dss):
        expected = 20.0
        actual = dss.pdelements.pct_permanent
        assert actual == expected

    def test_pdelements_write_pct_permanent(self, dss):
        expected = 30
        dss.pdelements.pct_permanent = expected
        actual = dss.pdelements.pct_permanent
        assert actual == expected

    def test_pdelements_lambda(self, dss):
        expected = 0
        actual = dss.pdelements.branch_failure_rate
        assert actual == expected

    def test_pdelements_accumulated_l(self, dss):
        expected = 0
        actual = dss.pdelements.accumulated_failure_rate
        assert actual == expected

    def test_pdelements_repair_time(self, dss):
        expected = 3
        actual = dss.pdelements.repair_time
        assert actual == expected

    def test_pdelements_total_miles(self, dss):
        expected = 0
        actual = dss.pdelements.total_miles
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_pdelements_read_name(self, dss):
        expected = 'Line.671692'
        actual = dss.pdelements.name
        assert actual == expected

    def test_pdelements_write_name(self, dss):
        expected = 'Capacitor.cap2'
        dss.pdelements.name = expected
        actual = dss.pdelements.name
        assert actual == expected
