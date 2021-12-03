# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 06:56 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_swtcontrols.py
# @Software : VSCode

import pytest


class TestSwtControls13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("new swtcontrol.1  SwitchedObj=line.650632  SwitchedTerm=1 Action=c")
        dss.text("new swtcontrol.2  SwitchedObj=line.692675  SwitchedTerm=1 Action=o")
        dss.solution_solve()
        dss.swtcontrols_write_name('1')

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_swtcontrols_first(self, dss):
        expected = 1
        actual = dss.swtcontrols_first()
        assert actual == expected

    def test_swtcontrols_next(self, dss):
        expected = 2
        actual = dss.swtcontrols_next()
        assert actual == expected

    def test_swtcontrols_read_action(self, dss):
        expected = 2
        actual = dss.swtcontrols_read_action()
        assert actual == expected

    def test_swtcontrols_write_action(self, dss):
        expected = 2
        dss.swtcontrols_write_action(expected)
        actual = dss.swtcontrols_read_action()
        assert actual == expected

    def test_swtcontrols_read_is_locked(self, dss):
        expected = 1
        actual = dss.swtcontrols_read_is_locked()
        assert actual == expected

    def test_swtcontrols_write_is_locked(self, dss):
        expected = 1
        dss.swtcontrols_write_is_locked(expected)
        actual = dss.swtcontrols_read_is_locked()
        assert actual == expected

    def test_swtcontrols_read_switched_term(self, dss):
        expected = 1
        actual = dss.swtcontrols_read_switched_term()
        assert actual == expected

    def test_swtcontrols_write_switched_term(self, dss):
        expected = 0
        dss.swtcontrols_write_switched_term(expected)
        actual = dss.swtcontrols_read_switched_term()
        assert actual == expected

    def test_swtcontrols_count(self, dss):
        expected = 2
        actual = dss.swtcontrols_count()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_swtcontrols_read_delay(self, dss):
        expected = 120
        actual = dss.swtcontrols_read_delay()
        assert actual == expected

    def test_swtcontrols_write_delay(self, dss):
        expected = 160.0
        dss.swtcontrols_write_delay(expected)
        actual = dss.swtcontrols_read_delay()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_swtcontrols_read_name(self, dss):
        expected = '1'
        actual = dss.swtcontrols_read_name()
        assert actual == expected

    def test_swtcontrols_write_name(self, dss):
        expected = '2'
        dss.swtcontrols_write_name(expected)
        actual = dss.swtcontrols_read_name()
        assert actual == expected

    def test_swtcontrols_read_switched_obj(self, dss):
        expected = 'line.650632'
        actual = dss.swtcontrols_read_switched_obj()
        assert actual == expected

    def test_swtcontrols_write_switched_obj(self, dss):
        expected = 'line.692675'
        dss.swtcontrols_write_switched_obj(expected)
        actual = dss.swtcontrols_read_switched_obj()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_swtcontrols_all_names(self, dss):
        expected = ['1', '2']
        actual = dss.swtcontrols_all_names()
