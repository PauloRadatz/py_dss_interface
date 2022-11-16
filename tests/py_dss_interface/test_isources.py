# -*- coding: utf-8 -*-
# @Time    : 6/25/2021 4:11 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_isources.py
# @Software: PyCharm

import pytest


class TestBus13ISources:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("new isource.MyISource phases=3 amps=100 bus1=692 angle=30")

        return dss

    def test_isources_count(self, dss):
        expected = 1
        actual = dss.isource.count
        assert actual == expected

    def test_isources_first(self, dss):
        expected = 1
        actual = dss.isource.first()
        assert actual == expected

        expected = 'myisource'
        actual = dss.isource.name
        assert actual == expected

    def test_isources_next(self, dss):
        dss.isource.name = 'myisource'
        expected = 0
        actual = dss.isource.next()
        assert actual == expected

        expected = 'myisource'
        actual = dss.isource.name
        assert actual == expected

    def test_isources_read_amps(self, dss):
        expected = 100.0
        actual = dss.isource.amps
        assert actual == expected

    def test_isources_write_amps(self, dss):
        expected = 0.0
        dss.isource.amps = expected
        actual = dss.isource.amps
        assert actual == expected

    def test_isources_read_angle_deg(self, dss):
        expected = 30.0
        actual = dss.isource.angle_deg
        assert actual == expected

    def test_isources_write_angle_deg(self, dss):
        expected = 0.0
        dss.isource.angle_deg = expected
        actual = dss.isource.angle_deg
        assert actual == expected

    def test_isources_read_frequency(self, dss):
        expected = 60.0
        actual = dss.isource.frequency
        assert actual == expected

    def test_isources_write_frequency(self, dss):
        expected = 0.0
        dss.isource.frequency = expected
        actual = dss.isource.frequency
        assert actual == expected

