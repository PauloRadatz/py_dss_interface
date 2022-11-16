# -*- coding: utf-8 -*-
# @Time    : 6/27/2021 4:48 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_dssproperties.py
# @Software: PyCharm

import pytest


class TestBus13DSSProperties:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.circuit.set_active_element('Line.671692')

        return dss

    def test_dssproperties_name(self, dss):
        expected = "bus1"
        actual = dss.dssproperties.name_active_property("1")
        assert actual == expected

        expected = "linecode"
        actual = dss.dssproperties.name_active_property("3")
        assert actual == expected

    def test_dssproperties_description(self, dss):
        expected = 'Length of line. Default is 1.0. If units do not match the impedance data, specify "units" ' \
                   'property. '
        actual = dss.dssproperties.description_active_property("4")
        assert actual == expected

    def test_dssproperties_read_value(self, dss):
        expected = "671"
        actual = dss.dssproperties.value_read("1")
        assert actual == expected

    def test_dssproperties_write_value(self, dss):
        # todo
        pass
