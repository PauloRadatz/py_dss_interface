# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 10:55 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_parser.py
# @Software: PyCharm


import pytest
import platform

# TODO: Error in all tests


class TestDSSParser13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_parser_int_value(self):
        # expected = 1
        # actual = self.dss.parser_int_value()
        # assert expected == actual
        ...

    def test_parser_reset_delimeters(self):
        # expected = 1
        # actual = self.dss.parser_reset_delimeters()
        # assert expected == actual
        ...

    def test_parser_read_auto_increment(self):
        # expected = 0
        # actual = self.dss.parser_read_auto_increment()
        # assert expected == actual
        ...

    def test_parser_write_auto_increment(self):
        # expected = 1
        # self.dss.parser_write_auto_increment(expected)
        # actual = self.dss.parser_read_auto_increment()
        # assert expected == actual
        ...

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_parser_dbl_value(self):
        # expected = 1
        # actual = self.dss.parser_dbl_value()
        # assert expected == actual
        ...

    # ===================================================================
    # String methods
    # ===================================================================
    def test_parser_read_cmd_string(self):
        # expected = ""
        # actual = self.dss.parser_read_cmd_string()
        # assert expected == actual
        ...

    def test_parser_write_cmd_string(self):
        # expected = "? datapath"
        # self.dss.parser_write_cmd_string(expected)
        # actual = self.dss.parser_read_cmd_string()
        # assert expected == actual
        ...

    def test_parser_next_param(self):
        # expected = "0"
        # actual = self.dss.parser_next_param()
        # assert expected == actual
        ...

    def test_parser_str_value(self):
        # expected = "1"
        # actual = self.dss.parser_str_value()
        # assert expected == actual
        ...

    def test_parser_read_white_space(self):
        # expected = ""
        # actual = self.dss.parser_read_white_space()
        # assert expected == actual
        ...

    def test_parser_write_white_space(self):
        # expected = "a"
        # self.dss.parser_write_white_space(expected)
        # actual = self.dss.parser_read_white_space()
        # assert expected == actual
        ...

    def test_parser_read_begin_quote(self):
        # expected = ""
        # actual = self.dss.parser_read_begin_quote()
        # assert expected == actual
        ...

    def test_parser_write_begin_quote(self):
        # expected = "a"
        # self.dss.parser_write_begin_quote(expected)
        # actual = self.dss.parser_read_begin_quote()
        # assert expected == actual
        ...

    def test_parser_read_end_quote(self):
        # expected = ""
        # actual = self.dss.parser_read_end_quote()
        # assert expected == actual
        ...

    def test_parser_write_end_quote(self):
        # expected = "a"
        # self.dss.parser_write_end_quote(expected)
        # actual = self.dss.parser_read_end_quote()
        # assert expected == actual
        ...

    def test_parser_read_delimiters(self):
        # expected = ","
        # actual = self.dss.parser_read_delimiters()
        # assert expected == actual
        ...

    def test_parser_write_delimiters(self):
        # expected = ";"
        # self.dss.parser_write_delimiters(expected)
        # actual = self.dss.parser_read_delimiters()
        # assert expected == actual
        ...

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_parser_vector(self):
        # expected = [1, 2, 3]
        # actual = self.dss.parser_vector()
        # assert expected == actual
        ...

    def test_parser_matrix(self):
        # expected = [[1, 2, 3], [4, 5, 6]]
        # actual = self.dss.parser_matrix()
        # assert expected == actual
        ...

    def test_parser_sym_matrix(self):
        # expected = [[1, 2, 3], [4, 5, 6]]
        # actual = self.dss.parser_sym_matrix()
        # assert expected == actual
        ...
