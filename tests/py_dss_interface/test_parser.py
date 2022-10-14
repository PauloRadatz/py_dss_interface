# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 10:55 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_parser.py
# @Software: PyCharm


import pytest


# TODO: Error in all tests


class TestDSSParser13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        return solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_parser_int_value(self, dss):
        # expected = 1
        # actual = dss.parser_int_value()
        # assert actual == expected
        ...

    def test_parser_reset_delimeters(self, dss):
        # expected = 1
        # actual = dss.parser_reset_delimeters()
        # assert actual == expected
        ...

    def test_parser_read_auto_increment(self, dss):
        # expected = 0
        # actual = dss.parser_read_auto_increment()
        # assert actual == expected
        ...

    def test_parser_write_auto_increment(self, dss):
        # expected = 1
        # dss.parser_write_auto_increment(expected)
        # actual = dss.parser_read_auto_increment()
        # assert actual == expected
        ...

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_parser_dbl_value(self, dss):
        # expected = 1
        # actual = dss.parser_dbl_value()
        # assert actual == expected
        ...

    # ===================================================================
    # String methods
    # ===================================================================
    def test_parser_read_cmd_string(self, dss):
        # expected = ""
        # actual = dss.parser_read_cmd_string()
        # assert actual == expected
        ...

    def test_parser_write_cmd_string(self, dss):
        # expected = "? datapath"
        # dss.parser_write_cmd_string(expected)
        # actual = dss.parser_read_cmd_string()
        # assert actual == expected
        ...

    def test_parser_next_param(self, dss):
        # expected = "0"
        # actual = dss.parser_next_param()
        # assert actual == expected
        ...

    def test_parser_str_value(self, dss):
        # expected = "1"
        # actual = dss.parser_str_value()
        # assert actual == expected
        ...

    def test_parser_read_white_space(self, dss):
        # expected = ""
        # actual = dss.parser_read_white_space()
        # assert actual == expected
        ...

    def test_parser_write_white_space(self, dss):
        # expected = "a"
        # dss.parser_write_white_space(expected)
        # actual = dss.parser_read_white_space()
        # assert actual == expected
        ...

    def test_parser_read_begin_quote(self, dss):
        # expected = ""
        # actual = dss.parser_read_begin_quote()
        # assert actual == expected
        ...

    def test_parser_write_begin_quote(self, dss):
        # expected = "a"
        # dss.parser_write_begin_quote(expected)
        # actual = dss.parser_read_begin_quote()
        # assert actual == expected
        ...

    def test_parser_read_end_quote(self, dss):
        # expected = ""
        # actual = dss.parser_read_end_quote()
        # assert actual == expected
        ...

    def test_parser_write_end_quote(self, dss):
        # expected = "a"
        # dss.parser_write_end_quote(expected)
        # actual = dss.parser_read_end_quote()
        # assert actual == expected
        ...

    def test_parser_read_delimiters(self, dss):
        # expected = ","
        # actual = dss.parser_read_delimiters()
        # assert actual == expected
        ...

    def test_parser_write_delimiters(self, dss):
        # expected = ";"
        # dss.parser_write_delimiters(expected)
        # actual = dss.parser_read_delimiters()
        # assert actual == expected
        ...

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_parser_vector(self, dss):
        # expected = [1, 2, 3]
        # actual = dss.parser_vector()
        # assert actual == expected
        ...

    def test_parser_matrix(self, dss):
        # expected = [[1, 2, 3], [4, 5, 6]]
        # actual = dss.parser_matrix()
        # assert actual == expected
        ...

    def test_parser_sym_matrix(self, dss):
        # expected = [[1, 2, 3], [4, 5, 6]]
        # actual = dss.parser_sym_matrix()
        # assert actual == expected
        ...
