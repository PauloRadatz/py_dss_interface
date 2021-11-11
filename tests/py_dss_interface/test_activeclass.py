# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 08:27 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_activeclass.py
# @Software: PyCharm


import pytest


class TestActiveClass13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_active_class_first(self, dss):
        expected = 1
        actual = dss.active_class_first()
        assert actual == expected

    def test_active_class_next(self, dss):
        expected = 2
        dss.active_class_first()
        actual = dss.active_class_next()
        assert actual == expected

    def test_active_class_num_elements(self, dss):
        expected = 12
        actual = dss.active_class_num_elements()
        assert actual == expected

    def test_active_class_count(self, dss):
        expected = 12
        actual = dss.active_class_count()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================

    def test_active_class_get_class_name(self, dss):
        expected = 'Line'
        actual = dss.active_class_get_class_name()
        assert actual == expected

    def test_active_class_get_name(self, dss):
        expected = '671692'
        actual = dss.active_class_get_name()
        assert actual == expected

    def test_active_class_write_name(self, dss):
        expected = '645646'
        actual = dss.active_class_write_name(expected)
        actual = dss.active_class_get_name()
        assert actual == expected

    def test_active_class_parent_class_name(self, dss):
        expected = 'TPDClass'
        actual = dss.active_class_parent_class_name()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_active_class_all_names(self, dss):
        expected = ['650632', '632670', '670671', '671680', '632633', '632645', '645646', '692675', '671684', '684611',
                    '684652', '671692']
        actual = dss.active_class_all_names()
        assert actual == expected
