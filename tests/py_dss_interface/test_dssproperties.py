# -*- coding: utf-8 -*-
# @Time    : 6/27/2021 4:48 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_dssproperties.py
# @Software: PyCharm

import pytest
import platform


class TestBus13DSSProperties:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.circuit_set_active_element('Line.671692')

    def test_dssproperties_name(self):
        expected = "bus1"
        actual = self.dss.dssproperties_name("1")
        assert actual == expected

        expected = "linecode"
        actual = self.dss.dssproperties_name("3")
        assert actual == expected

    def test_dssproperties_description(self):
        expected = 'Length of line. Default is 1.0. If units do not match the impedance data, specify "units" property. '
        actual = self.dss.dssproperties_description("4")
        assert actual == expected

    def test_dssproperties_read_value(self):
        expected = "671"
        actual = self.dss.dssproperties_read_value("1")
        assert actual == expected

    def test_dssproperties_write_value(self):
        #todo
        pass


