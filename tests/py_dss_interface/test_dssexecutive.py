# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 09:31 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_dssexecutive.py
# @Software: PyCharm


import pytest


class TestDSSExecutive13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_executive_num_commands(self):
        expected = 129
        actual = self.dss.executive_num_commands()
        assert actual == expected

    def test_executive_num_options(self):
        expected = 133
        actual = self.dss.executive_num_options()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_executive_command(self):
        expected = "New"
        actual = self.dss.executive_command("1")
        assert actual == expected

    def test_executive_option(self):
        expected = "type"
        actual = self.dss.executive_option("1")
        assert actual == expected

    def test_executive_command_help(self):
        expected = "Continuation of editing on the active object."
        actual = self.dss.executive_command_help("3")
        assert actual == expected

    def test_executive_option_help(self):
        expected = "Sets the active DSS class type.  Same as Class=..."
        actual = self.dss.executive_option_help("1")
        assert actual == expected

    def test_executive_option_value(self):
        expected = "Line"
        actual = self.dss.executive_option_value("1")
        assert actual == expected
