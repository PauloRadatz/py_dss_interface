# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 08:27 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_activeclass.py
# @Software: PyCharm


import pytest


class TestActiveClass13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_active_class_first(self):
        expected = 1
        actual = self.dss.active_class_first()
        assert expected == actual

    def test_active_class_next(self):
        expected = 2
        self.dss.active_class_first()
        actual = self.dss.active_class_next()
        assert expected == actual

    def test_active_class_num_elements(self):
        expected = 12
        actual = self.dss.active_class_num_elements()
        assert expected == actual

    def test_active_class_count(self):
        expected = 12
        actual = self.dss.active_class_count()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================

    def test_active_class_get_class_name(self):
        expected = 'Line'
        actual = self.dss.active_class_get_class_name()
        assert expected == actual

    def test_active_class_get_name(self):
        expected = '671692'
        actual = self.dss.active_class_get_name()
        assert expected == actual

    def test_active_class_write_name(self):
        expected = '645646'
        actual = self.dss.active_class_write_name(expected)
        actual = self.dss.active_class_get_name()
        assert expected == actual

    def test_active_class_parent_class_name(self):
        expected = 'TPDClass'
        actual = self.dss.active_class_parent_class_name()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_active_class_all_names(self):
        expected = ['650632', '632670', '670671', '671680', '632633', '632645', '645646', '692675', '671684', '684611',
                    '684652', '671692']
        actual = self.dss.active_class_all_names()
        assert expected == actual
