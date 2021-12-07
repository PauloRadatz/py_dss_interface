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
        dss.solution_solve()
        dss.vsources_write_name('fonte1')

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_vsources_count(self, dss):
        expected = 2
        actual = dss.vsources_count()
        assert actual == expected

    def test_vsources_first(self, dss):
        expected = 1
        actual = dss.vsources_first()
        assert actual == expected

    def test_vsources_next(self, dss):
        expected = 2
        dss.vsources_first()
        actual = dss.vsources_next()
        assert actual == expected

    def test_vsources_read_phases(self, dss):
        expected = 3
        actual = dss.vsources_read_phases()
        assert actual == expected

    def test_vsources_write_phases(self, dss):
        expected = 2
        dss.vsources_write_phases(expected)
        actual = dss.vsources_read_phases()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_vsources_read_base_kv(self, dss):
        expected = 2.4
        actual = dss.vsources_read_base_kv()
        assert actual == expected

    def test_vsources_write_base_kv(self, dss):
        expected = 13.8
        dss.vsources_write_base_kv(expected)
        actual = dss.vsources_read_base_kv()

    def test_vsources_read_pu(self, dss):
        expected = 1.0001
        actual = dss.vsources_read_pu()
        assert actual == expected

    def test_vsources_write_pu(self, dss):
        expected = 1.1
        dss.vsources_write_pu(expected)
        actual = dss.vsources_read_pu()
        assert actual == expected

    def test_vsources_read_angle_deg(self, dss):
        expected = 30
        actual = dss.vsources_read_angle_deg()
        assert actual == expected

    def test_vsources_write_angle_deg(self, dss):
        expected = 45
        dss.vsources_write_angle_deg(expected)
        actual = dss.vsources_read_angle_deg()
        assert actual == expected

    def test_vsources_read_frequency(self, dss):
        expected = 60
        actual = dss.vsources_read_frequency()
        assert actual == expected

    def test_vsources_write_frequency(self, dss):
        expected = 50
        dss.vsources_write_frequency(expected)
        actual = dss.vsources_read_frequency()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_vsources_read_name(self, dss):
        expected = 'fonte1'
        actual = dss.vsources_read_name()
        assert actual == expected

    def test_vsources_write_name(self, dss):
        expected = 'source'
        dss.vsources_write_name(expected)
        actual = dss.vsources_read_name()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_vsources_all_names(self, dss):
        expected = ['source', 'fonte1']
        actual = dss.vsources_all_names()
        assert actual == expected
