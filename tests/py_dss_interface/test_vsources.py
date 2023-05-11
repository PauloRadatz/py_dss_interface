# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:30 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_vsources.py
# @Software : VSCode

import pytest


class TestVSources13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text('New Vsource.Fonte1 basekv=2.4 pu=1.0001 phases=3 bus1=611.3 angle=30 MVAsc3=1000 MVAsc1=2000')
        dss.solution.solve()
        dss.vsources.name = 'fonte1'

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_vsources_count(self, dss):
        expected = 2
        actual = dss.vsources.count
        assert actual == expected

    def test_vsources_first(self, dss):
        expected = 1
        actual = dss.vsources.first()
        assert actual == expected

    def test_vsources_next(self, dss):
        expected = 2
        dss.vsources.first()
        actual = dss.vsources.next()
        assert actual == expected

    def test_vsources_read_phases(self, dss):
        expected = 3
        actual = dss.vsources.phases
        assert actual == expected

    def test_vsources_write_phases(self, dss):
        expected = 2
        dss.vsources.phases = expected
        actual = dss.vsources.phases
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_vsources_read_base_kv(self, dss):
        expected = 2.4
        actual = dss.vsources.base_kv
        assert actual == expected

    def test_vsources_write_base_kv(self, dss):
        expected = 13.8
        dss.vsources.base_kv = expected
        actual = dss.vsources.base_kv
        assert actual == expected

    def test_vsources_read_pu(self, dss):
        expected = 1.0001
        actual = dss.vsources.pu
        assert actual == expected

    def test_vsources_write_pu(self, dss):
        expected = 1.1
        dss.vsources.pu = expected
        actual = dss.vsources.pu
        assert actual == expected

    def test_vsources_read_angle_deg(self, dss):
        expected = 30
        actual = dss.vsources.angle_deg
        assert actual == expected

    def test_vsources_write_angle_deg(self, dss):
        expected = 45
        dss.vsources.angle_deg = expected
        actual = dss.vsources.angle_deg
        assert actual == expected

    def test_vsources_read_frequency(self, dss):
        expected = 60
        actual = dss.vsources.frequency
        assert actual == expected

    def test_vsources_write_frequency(self, dss):
        expected = 50
        dss.vsources.frequency = expected
        actual = dss.vsources.frequency
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_vsources_read_name(self, dss):
        expected = 'fonte1'
        actual = dss.vsources.name
        assert actual == expected

    def test_vsources_write_name(self, dss):
        expected = 'source'
        dss.vsources.name = expected
        actual = dss.vsources.name
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_vsources_all_names(self, dss):
        expected = ['source', 'fonte1']
        actual = dss.vsources.names
        assert actual == expected
