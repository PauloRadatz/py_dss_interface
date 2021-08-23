# -*- coding: utf-8 -*-
# @Time    : 8/17/2021 10:15 AM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_linecodes.py
# @Software: PyCharm

import pytest
import platform

class TestLineCodes13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.linecodes_write_name('1')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_linecodes_count(self):
        expected = 36
        actual = self.dss.linecodes_count()
        assert expected == actual

    def test_linecodes_first(self):
        expected = 1
        actual = self.dss.linecodes_first()
        assert expected == actual

    def test_linecodes_next(self):
        expected = 2
        actual = self.dss.linecodes_next()
        assert expected == actual

    def test_linecodes_read_units(self):
        expected = 0
        actual = self.dss.linecodes_read_units()
        assert expected == actual

    def test_linecodes_write_units(self):
        expected = 5
        self.dss.linecodes_write_units(expected)
        actual = self.dss.linecodes_read_units()
        assert expected == actual

    def test_linecodes_read_phases(self):
        expected = 3
        actual = self.dss.linecodes_read_phases()
        assert expected == actual

    def test_linecodes_write_phases(self):
        expected = 2
        self.dss.linecodes_write_phases(expected)
        actual = self.dss.linecodes_read_phases()
        assert expected == actual

    def test_linecodes_is_z1z0(self):
        expected = 0
        actual = self.dss.linecodes_is_z1z0()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_linecodes_read_name(self):
        expected = '1'
        actual = self.dss.linecodes_read_name()
        assert expected == actual

    def test_linecodes_write_name(self):
        expected = '2'
        self.dss.linecodes_write_name(expected)
        actual = self.dss.linecodes_read_name()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_linecodes_read_r1(self):
        expected = 0.058
        actual = self.dss.linecodes_read_r1()
        assert expected == actual

    def test_linecodes_write_r1(self):
        expected = 0.1
        self.dss.linecodes_write_r1(expected)
        actual = self.dss.linecodes_read_r1()
        assert expected == actual

    def test_linecodes_read_x1(self):
        expected = 0.1206
        actual = self.dss.linecodes_read_x1()
        assert expected == actual

    def test_linecodes_write_x1(self):
        expected = 0.1
        self.dss.linecodes_write_x1(expected)
        actual = self.dss.linecodes_read_x1()
        assert expected == actual

    def test_linecodes_read_c1(self):
        expected = 3.4
        actual = self.dss.linecodes_read_c1()
        assert expected == actual

    def test_linecodes_write_c1(self):
        # TODO: Paulo, check the input value to write c1
        expected = 1.0
        self.dss.linecodes_write_c1(expected)
        actual = self.dss.linecodes_read_c1()
        assert expected == actual

    def test_linecodes_read_r0(self):
        expected = 0.1784
        actual = self.dss.linecodes_read_r0()
        assert expected == actual

    def test_linecodes_write_r0(self):
        expected = 0.1
        self.dss.linecodes_write_r0(expected)
        actual = self.dss.linecodes_read_r0()
        assert expected == actual

    def test_linecodes_read_x0(self):
        expected = 0.4047
        actual = self.dss.linecodes_read_x0()
        assert expected == actual

    def test_linecodes_write_x0(self):
        expected = 0.1
        self.dss.linecodes_write_x0(expected)
        actual = self.dss.linecodes_read_x0()
        assert expected == actual

    def test_linecodes_read_c0(self):
        expected = 1.6
        actual = self.dss.linecodes_read_c0()
        assert expected == actual

    def test_linecodes_write_c0(self):
        # TODO: Paulo, check the input value to write c0
        expected = 1.0
        self.dss.linecodes_write_c0(expected)
        actual = self.dss.linecodes_read_c0()
        assert expected == actual

    def test_linecodes_read_norm_amps(self):
        expected = 400
        actual = self.dss.linecodes_read_norm_amps()
        assert expected == actual

    def test_linecodes_write_norm_amps(self):
        expected = 300
        self.dss.linecodes_write_norm_amps(expected)
        actual = self.dss.linecodes_read_norm_amps()
        assert expected == actual

    def test_linecodes_read_emerg_amps(self):
        expected = 600
        actual = self.dss.linecodes_read_emerg_amps()
        assert expected == actual

    def test_linecodes_write_emerg_amps(self):
        expected = 300
        self.dss.linecodes_write_emerg_amps(expected)
        actual = self.dss.linecodes_read_emerg_amps()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_linecodes_read_rmatrix(self):
        expected = [0.086666667, 0.029545455, 0.02907197, 0.029545455, 0.088371212, 0.029924242, 0.02907197, 0.029924242, 0.08740530]
        expected = [truncate(x, 6) for x in expected]
        actual = self.dss.linecodes_read_rmatrix()
        actual = [truncate(x, 6) for x in actual]
        assert expected == actual

    def test_linecodes_write_rmatrix(self):
        expected_list = [0.791721, 0.31, 0.781649, 0.28345, 0.32, 0.791721]
        expected = format_matrix(expected_list)
        expected_str = format_matrix_str(expected)

        self.dss.linecodes_write_rmatrix(expected_str)
        actual = self.dss.linecodes_read_rmatrix()

        actual   = [truncate(x, 6) for x in actual]
        expected = [truncate(x, 6) for x in expected]

        assert expected == actual

    def test_linecodes_read_xmatrix(self):
        expected = [0.20416667, 0.09501894, 0.07289773, 0.09501894, 0.19852273, 0.08022727, 0.07289773, 0.08022727, 0.20172349]
        expected = [truncate(x, 6) for x in expected]

        actual = self.dss.linecodes_read_xmatrix()
        actual = [truncate(x, 6) for x in actual]

        assert expected == actual

    def test_linecodes_write_xmatrix(self):
        expected_list = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
        expected = format_matrix(expected_list)
        expected_str = format_matrix_str(expected)

        self.dss.linecodes_write_xmatrix(expected_str)
        actual = self.dss.linecodes_read_xmatrix()

        actual   = [truncate(x, 9) for x in actual]
        expected = [truncate(x, 9) for x in expected]

        assert expected == actual

    def test_linecodes_read_cmatrix(self):
        expected = [2.85171007, -0.92029379, -0.35075557, -0.92029379, 3.00463186, -0.58501125, -0.35075557, -0.58501125, 2.71134756]

        actual = self.dss.linecodes_read_cmatrix()
        actual   = [truncate(x, 6) for x in actual]
        expected = [truncate(x, 6) for x in expected]

        assert expected == actual

    def test_linecodes_write_cmatrix(self):
        expected_list = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
        expected = format_matrix(expected_list)
        expected_str = format_matrix_str(expected)
        self.dss.linecodes_write_cmatrix(expected_str)
        actual = self.dss.linecodes_read_cmatrix()

        actual   = [truncate(x, 9) for x in actual]
        expected = [truncate(x, 9) for x in expected]

        assert expected == actual

    def test_linecodes_all_names(self):
        expected = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '300', '301', '302', '303', '304',
                    '400', '601', '602', '603', '604', '605', '606', '607', '721', '722', '723', '724', 'mtx601',
                    'mtx602', 'mtx603', 'mtx604', 'mtx605', 'mtx606', 'mtx607']
        actual = self.dss.linecodes_all_names()
        assert expected == actual


def truncate(num,n):
    # Return a truncated version of a floating point number
    temp = str(num)
    for x in range(len(temp)):
        if temp[x] == '.':
            try:
                return float(temp[:x+n+1])
            except:
                return float(temp)
    return float(temp)

def format_matrix(expected_list):
    # Return a full matrix from expected list from lower triangle
    matrix = list()
    for i in range(0, 3):
        if i == 0:
            matrix.append(expected_list[0])
            matrix.append(expected_list[1])
            matrix.append(expected_list[3])
        elif i == 1:
            matrix.append(expected_list[1])
            matrix.append(expected_list[2])
            matrix.append(expected_list[4])
        else:
            matrix.append(expected_list[3])
            matrix.append(expected_list[4])
            matrix.append(expected_list[5])
    return matrix

def format_matrix_str(expected_list):
    # Return the matrix converted to string with delimiters used in OpenDSS
    matrix_str = '['
    for i, val in enumerate(expected_list):
        matrix_str += str(val) + " "
        if i in [2, 5]:
            matrix_str += ' | '
    return matrix_str[:-1] + ']'
