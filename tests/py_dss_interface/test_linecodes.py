# -*- coding: utf-8 -*-
# @Time    : 8/17/2021 10:15 AM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_linecodes.py
# @Software: PyCharm

import pytest


class TestLineCodes13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.linecodes.name = '1'

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_linecodes_count(self, dss):
        expected = 36
        actual = dss.linecodes.count
        assert actual == expected

    def test_linecodes_first(self, dss):
        expected = 1
        actual = dss.linecodes.first()
        assert actual == expected

    def test_linecodes_next(self, dss):
        expected = 2
        actual = dss.linecodes.next()
        assert actual == expected

    def test_linecodes_read_units(self, dss):
        expected = 0
        actual = dss.linecodes.units
        assert actual == expected

    def test_linecodes_write_units(self, dss):
        expected = 5
        dss.linecodes.units = expected
        actual = dss.linecodes.units
        assert actual == expected

    def test_linecodes_read_phases(self, dss):
        expected = 3
        actual = dss.linecodes.phases
        assert actual == expected

    def test_linecodes_write_phases(self, dss):
        expected = 2
        dss.linecodes.phases = expected
        actual = dss.linecodes.phases
        assert actual == expected

    def test_linecodes_is_z1z0(self, dss):
        expected = 0
        actual = dss.linecodes.is_z1z0
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_linecodes_read_name(self, dss):
        expected = '1'
        actual = dss.linecodes.name
        assert actual == expected

    def test_linecodes_write_name(self, dss):
        expected = '2'
        dss.linecodes.name = expected
        actual = dss.linecodes.name
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_linecodes_read_r1(self, dss):
        expected = 0.058
        actual = dss.linecodes.r1
        assert actual == expected

    def test_linecodes_write_r1(self, dss):
        expected = 0.1
        dss.linecodes.r1 = expected
        actual = dss.linecodes.r1
        assert actual == expected

    def test_linecodes_read_x1(self, dss):
        expected = 0.1206
        actual = dss.linecodes.x1
        assert actual == expected

    def test_linecodes_write_x1(self, dss):
        expected = 0.1
        dss.linecodes.x1 = expected
        actual = dss.linecodes.x1
        assert actual == expected

    def test_linecodes_read_c1(self, dss):
        expected = 3.4
        actual = dss.linecodes.c1
        assert actual == expected

    def test_linecodes_write_c1(self, dss):
        expected = 1.0
        dss.linecodes.c1 = expected
        actual = dss.linecodes.c1
        assert actual == expected

    def test_linecodes_read_r0(self, dss):
        expected = 0.1784
        actual = dss.linecodes.r0
        assert actual == expected

    def test_linecodes_write_r0(self, dss):
        expected = 0.1
        dss.linecodes.r0 = expected
        actual = dss.linecodes.r0
        assert actual == expected

    def test_linecodes_read_x0(self, dss):
        expected = 0.4047
        actual = dss.linecodes.x0
        assert actual == expected

    def test_linecodes_write_x0(self, dss):
        expected = 0.1
        dss.linecodes.x0 = expected
        actual = dss.linecodes.x0
        assert actual == expected

    def test_linecodes_read_c0(self, dss):
        expected = 1.6
        actual = dss.linecodes.c0
        assert actual == expected

    def test_linecodes_write_c0(self, dss):
        expected = 2.0
        dss.linecodes.c0 = expected
        actual = dss.linecodes.c0
        assert actual == expected

    def test_linecodes_read_norm_amps(self, dss):
        expected = 400
        actual = dss.linecodes.norm_amps
        assert actual == expected

    def test_linecodes_write_norm_amps(self, dss):
        expected = 300
        dss.linecodes.norm_amps = expected
        actual = dss.linecodes.norm_amps
        assert actual == expected

    def test_linecodes_read_emerg_amps(self, dss):
        expected = 600
        actual = dss.linecodes.emerg_amps
        assert actual == expected

    def test_linecodes_write_emerg_amps(self, dss):
        expected = 300
        dss.linecodes.emerg_amps = expected
        actual = dss.linecodes.emerg_amps
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_linecodes_read_rmatrix(self, dss):
        expected = [0.086666667, 0.029545455, 0.02907197, 0.029545455, 0.088371212, 0.029924242, 0.02907197,
                    0.029924242, 0.08740530]
        expected = [truncate(x, 6) for x in expected]
        actual = dss.linecodes.rmatrix
        actual = [truncate(x, 6) for x in actual]
        assert actual == expected

    def test_linecodes_write_rmatrix(self, dss):
        expected = [0.086, 0.029, 0.02, 0.029, 0.088, 0.029, 0.02, 0.029, 0.08]

        dss.linecodes.rmatrix = expected
        actual = dss.linecodes.rmatrix

        assert actual == expected

    def test_linecodes_read_xmatrix(self, dss):
        expected = [0.20416667, 0.09501894, 0.07289773, 0.09501894, 0.19852273, 0.08022727, 0.07289773, 0.08022727, 0.20172349]
        expected = [truncate(x, 6) for x in expected]

        actual = dss.linecodes.xmatrix
        actual = [truncate(x, 6) for x in actual]

        assert actual == expected

    def test_linecodes_write_xmatrix(self, dss):
        expected = [0.086, 0.029, 0.02, 0.029, 0.088, 0.029, 0.02, 0.029, 0.08]

        dss.linecodes.xmatrix = expected
        actual = dss.linecodes.xmatrix

        assert actual == expected

    def test_linecodes_read_cmatrix(self, dss):
        expected = [2.85171007, -0.92029379, -0.35075557, -0.92029379, 3.00463186, -0.58501125, -0.35075557, -0.58501125, 2.71134756]

        actual = dss.linecodes.cmatrix
        actual = [truncate(x, 6) for x in actual]
        expected = [truncate(x, 6) for x in expected]

        assert actual == expected

    def test_linecodes_write_cmatrix(self, dss):
        expected = [0.0861100, 0.0291100, 0.021100, 0.0291100, 0.0881100, 0.0291100, 0.021100, 0.0291100, 0.081100]

        dss.linecodes.cmatrix = expected
        actual = dss.linecodes.cmatrix

        actual = [truncate(x, 3) for x in actual]
        expected = [truncate(x, 3) for x in expected]

        assert actual == expected

    def test_linecodes_all_names(self, dss):
        expected = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '300', '301', '302', '303', '304',
                    '400', '601', '602', '603', '604', '605', '606', '607', '721', '722', '723', '724', 'mtx601',
                    'mtx602', 'mtx603', 'mtx604', 'mtx605', 'mtx606', 'mtx607']
        actual = dss.linecodes.names
        assert actual == expected


# # Todo move those guys to another place.
def truncate(num, n):
    # Return a truncated version of a floating point number
    temp = str(num)
    for x in range(len(temp)):
        if temp[x] == '.':
            try:
                return float(temp[:x + n + 1])
            except Exception as e:
                print(e)
                return float(temp)
    return float(temp)


def format_matrix(expected_list):
    # Return a full matrix from expected list from lower triangle
    matrix = []
    for i in range(3):
        if i == 0:
            matrix.extend((expected_list[0], expected_list[1], expected_list[3]))
        elif i == 1:
            matrix.extend((expected_list[1], expected_list[2], expected_list[4]))
        else:
            matrix.extend((expected_list[3], expected_list[4], expected_list[5]))
    return matrix


def format_matrix_str(expected_list):
    # Return the matrix converted to string with delimiters used in OpenDSS
    matrix_str = '['
    for i, val in enumerate(expected_list):
        matrix_str += f"{str(val)} "
        if i in [2, 5]:
            matrix_str += ' | '
    return f'{matrix_str[:-1]}]'
