# -*- coding: utf-8 -*-
# @Time    : 6/27/2021 4:36 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_dsselement.py
# @Software: PyCharm

import pytest
import platform


class TestBus13DSSElement:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.circuit_set_active_element('Line.671692')

    def test_dsselement_num_properties(self):
        expected = 38
        actual = self.dss.dsselement_num_properties()
        assert actual == expected

    def test_dsselement_name(self):
        expected = "Line.671692"
        actual = self.dss.dsselement_name()
        assert actual == expected

    def test_dsselement_all_property_names(self):
        expected = ['bus1', 'bus2', 'linecode', 'length', 'phases', 'r1', 'x1', 'r0', 'x0', 'C1', 'C0', 'rmatrix',
                    'xmatrix', 'cmatrix', 'Switch', 'Rg', 'Xg', 'rho', 'geometry', 'units', 'spacing', 'wires',
                    'EarthModel', 'cncables', 'tscables', 'B1', 'B0', 'Seasons', 'Ratings', 'LineType', 'normamps',
                    'emergamps', 'faultrate', 'pctperm', 'repair', 'basefreq', 'enabled', 'like']
        actual = self.dss.dsselement_all_property_names()
        assert actual == expected
