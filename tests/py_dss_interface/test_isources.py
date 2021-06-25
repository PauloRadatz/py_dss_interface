# -*- coding: utf-8 -*-
# @Time    : 6/25/2021 4:11 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_isources.py
# @Software: PyCharm

import pytest


class TestBus13ISources:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("new isource.MyISource phases=3 amps=100 bus1=692 angle=30")

    def test_isources_count(self):
        expected = 1
        actual = self.dss.isources_count()
        assert actual == expected

    def test_isources_first(self):
        expected = 1
        actual = self.dss.isources_first()
        assert actual == expected

        expected = 'myisource'
        actual = self.dss.isources_read_name()
        assert actual == expected

    def test_isources_next(self):
        self.dss.isources_write_name('myisource')
        expected = 0
        actual = self.dss.isources_next()
        assert actual == expected

        expected = 'myisource'
        actual = self.dss.isources_read_name()
        assert actual == expected

    def test_isources_read_amps(self):
        expected = 100.0
        actual = self.dss.isources_read_amps()
        assert actual == expected

    def test_isources_write_amps(self):
        expected = 0.0
        self.dss.isources_write_amps(expected)
        actual = self.dss.isources_read_amps()
        assert actual == expected

    def test_isources_read_angle_deg(self):
        expected = 30.0
        actual = self.dss.isources_read_angle_deg()
        assert actual == expected

    def test_isources_write_angle_deg(self):
        expected = 0.0
        self.dss.isources_write_angle_deg(expected)
        actual = self.dss.isources_read_angle_deg()
        assert actual == expected

    def test_isources_read_frequency(self):
        expected = 60.0
        actual = self.dss.isources_read_frequency()
        assert actual == expected

    def test_isources_write_frequency(self):
        expected = 0.0
        self.dss.isources_write_frequency(expected)
        actual = self.dss.isources_read_frequency()
        assert actual == expected


