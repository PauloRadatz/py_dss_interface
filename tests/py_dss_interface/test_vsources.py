# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:30 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_vsources.py
# @Software : VSCode

import pytest


class TestVSources13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text('New "Vsource.Fonte1" basekv=2.4 pu=1.0001 phases=3 bus1=611.3 angle=30 MVAsc3=1000 MVAsc1=2000')
        self.dss.solution_solve()
        self.dss.vsources_write_name('fonte1')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_vsources_count(self):
        expected = 2
        actual = self.dss.vsources_count()
        assert actual == expected

    def test_vsources_first(self):
        expected = 1
        actual = self.dss.vsources_first()
        assert actual == expected

    def test_vsources_next(self):
        expected = 2
        self.dss.vsources_first()
        actual = self.dss.vsources_next()
        assert actual == expected

    def test_vsources_read_phases(self):
        expected = 3
        actual = self.dss.vsources_read_phases()
        assert actual == expected

    def test_vsources_write_phases(self):
        expected = 2
        self.dss.vsources_write_phases(expected)
        actual = self.dss.vsources_read_phases()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_vsources_read_base_kv(self):
        expected = 2.4
        actual = self.dss.vsources_read_base_kv()
        assert actual == expected

    def test_vsources_write_base_kv(self):
        expected = 13.8
        self.dss.vsources_write_base_kv(expected)
        actual = self.dss.vsources_read_base_kv()

    def test_vsources_read_pu(self):
        expected = 1.0001
        actual = self.dss.vsources_read_pu()
        assert actual == expected

    def test_vsources_write_pu(self):
        expected = 1.1
        self.dss.vsources_write_pu(expected)
        actual = self.dss.vsources_read_pu()
        assert actual == expected

    def test_vsources_read_angle_deg(self):
        expected = 30
        actual = self.dss.vsources_read_angle_deg()
        assert actual == expected

    def test_vsources_write_angle_deg(self):
        expected = 45
        self.dss.vsources_write_angle_deg(expected)
        actual = self.dss.vsources_read_angle_deg()
        assert actual == expected

    def test_vsources_read_frequency(self):
        expected = 60
        actual = self.dss.vsources_read_frequency()
        assert actual == expected

    def test_vsources_write_frequency(self):
        expected = 50
        self.dss.vsources_write_frequency(expected)
        actual = self.dss.vsources_read_frequency()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_vsources_read_name(self):
        expected = 'fonte1'
        actual = self.dss.vsources_read_name()
        assert actual == expected

    def test_vsources_write_name(self):
        expected = 'source'
        self.dss.vsources_write_name(expected)
        actual = self.dss.vsources_read_name()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_vsources_all_names(self):
        expected = ['source', 'fonte1']
        actual = self.dss.vsources_all_names()
        assert actual == expected
