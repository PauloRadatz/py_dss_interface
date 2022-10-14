# -*- coding: utf-8 -*-
# @Time    : 11/3/2021 4:39 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : utils.py
# @Software: PyCharm

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
