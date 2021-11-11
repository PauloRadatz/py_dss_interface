# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 09:31 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_dssexecutive.py
# @Software: PyCharm


import pytest


class TestDSSExecutive13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_executive_num_commands(self, dss):
        expected = 129
        actual = dss.executive_num_commands()
        assert actual == expected

    def test_executive_num_options(self, dss):
        expected = 134
        actual = dss.executive_num_options()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_executive_command(self, dss):
        expected = "New"
        actual = dss.executive_command("1")
        assert actual == expected

    def test_executive_option(self, dss):
        expected = "type"
        actual = dss.executive_option("1")
        assert actual == expected

    def test_executive_command_help(self, dss):
        expected = "Continuation of editing on the active object."
        actual = dss.executive_command_help("3")
        assert actual == expected

    def test_executive_option_help(self, dss):
        expected = "Sets the active DSS class type.  Same as Class=..."
        actual = dss.executive_option_help("1")
        assert actual == expected

    def test_executive_option_value(self, dss):
        expected = "Line"
        actual = dss.executive_option_value("1")
        assert actual == expected
