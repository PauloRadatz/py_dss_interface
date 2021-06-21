# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# TODO: Paulo - https://github.com/PauloRadatz/py_dss_interface/issues/18
# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
# print(f'dss.parser_int_value(): {dss.parser_int_value()}')
# print(f'dss.parser_reset_delimeters(): {dss.parser_reset_delimeters()}')
# print(f'dss.parser_read_auto_increment(): {dss.parser_read_auto_increment()}')
# print(f'dss.parser_write_auto_increment(): {dss.parser_write_auto_increment(1)}')

# String methods
print(45 * '=' + ' String Methods' + 45 * '=')
# print(f'dss.parser_read_cmd_string(): {dss.parser_read_cmd_string()}')
# print(f'dss.parser_write_cmd_string(): {dss.parser_write_cmd_string(1)}')
# print(f'dss.parser_next_param(): {dss.parser_next_param()}')
# print(f'dss.parser_str_value(): {dss.parser_str_value()}')
# print(f'dss.parser_read_white_space(): {dss.parser_read_white_space()}')
# print(f'dss.parser_write_white_space(): {dss.parser_write_white_space(1)}')
# print(f'dss.parser_read_begin_quote(): {dss.parser_read_begin_quote()}')
# print(f'dss.parser_write_begin_quote(): {dss.parser_write_begin_quote(1)}')
# print(f'dss.parser_read_end_quote(): {dss.parser_read_end_quote()}')
# print(f'dss.parser_write_end_quote(): {dss.parser_write_end_quote(1)}')
# print(f'dss.parser_write_end_quote(): {dss.parser_write_end_quote()}')
# print(f'dss.parser_read_delimiters(): {dss.parser_read_delimiters()}')
# print(f'dss.parser_write_delimiters(): {dss.parser_write_delimiters(1)}')

# Float methods
print(45 * '=' + ' Float Methods' + 45 * '=')
# print(f'dss.parser_dbl_value(): {dss.parser_dbl_value()}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
# print(f'dss.parser_vector(): {dss.parser_vector()}')
# print(f'dss.parser_matrix(): {dss.parser_matrix()}')
# print(f'dss.parser_sym_matrix(): {dss.parser_sym_matrix()}')
#
