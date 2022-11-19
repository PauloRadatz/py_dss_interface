# -*- coding: utf-8 -*-
# @Time    : 6/27/2021 4:36 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_dsselement.py
# @Software: PyCharm

import pytest


class TestBus13DSSElement:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.circuit.set_active_element('Line.671692')

        return dss

    def test_dsselement_num_properties(self, dss):
        expected = 38
        actual = dss.dsselement.num_properties
        assert actual == expected

    def test_dsselement_name(self, dss):
        expected = "671692"
        actual = dss.lines.name
        assert actual == expected

    def test_dsselement_all_property_names(self, dss):
        expected = ['bus1', 'bus2', 'linecode', 'length', 'phases', 'r1', 'x1', 'r0', 'x0', 'C1', 'C0', 'rmatrix',
                    'xmatrix', 'cmatrix', 'Switch', 'Rg', 'Xg', 'rho', 'geometry', 'units', 'spacing', 'wires',
                    'EarthModel', 'cncables', 'tscables', 'B1', 'B0', 'Seasons', 'Ratings', 'LineType', 'normamps',
                    'emergamps', 'faultrate', 'pctperm', 'repair', 'basefreq', 'enabled', 'like']
        actual = dss.dsselement.property_names
        assert actual == expected
