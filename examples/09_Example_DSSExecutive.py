# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 12/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'executive_num_commands: {dss.num_commands()}')
print(f'executive_num_options: {dss.num_options()}')

# String methods
print(45 * '=' + ' String Methods ' + 45 * '=')
print(f'executive_command: {dss.command("1")}')
print(f'executive_option: {dss.option("1")}')
print(f'executive_command_help: {dss.command_help("1")}')
print(f'executive_option_help: {dss.option_help("1")}')
print(f'executive_option_value: {dss.option_value("1")}')
