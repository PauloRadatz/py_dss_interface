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
        return solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_first(self, dss):
        expected = 1
        actual = dss.active_class.first()
        assert actual == expected

    def test_next(self, dss):
        expected = 2
        dss.active_class.first()
        actual = dss.active_class.next()
        assert actual == expected

    def test_num_elements(self, dss):
        expected = 12
        actual = dss.active_class.num_elements
        assert actual == expected

    def test_count(self, dss):
        expected = 12
        actual = dss.active_class.count
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================

    def test_get_class_name(self, dss):
        expected = 'Line'
        actual = dss.active_class.class_name
        assert actual == expected

    def test_get_name(self, dss):
        expected = '671692'
        actual = dss.active_class.name
        assert actual == expected

    def test_write_name(self, dss):
        expected = '645646'
        dss.active_class.name = expected
        actual = dss.active_class.name
        assert actual == expected

    def test_parent_class_name(self, dss):
        expected = 'TPDClass'
        actual = dss.active_class.parent_class_name
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_all_names(self, dss):
        expected = ['650632', '632670', '670671', '671680', '632633', '632645', '645646', '692675', '671684', '684611',
                    '684652', '671692']
        actual = dss.active_class.names
        assert actual == expected
