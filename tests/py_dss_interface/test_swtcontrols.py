# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 06:56 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_swtcontrols.py
# @Software : VSCode

import pytest


class TestSWTControls13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("new swtcontrol.1   SwitchedObj=line.650632  SwitchedTerm=1 Action=c")
        self.dss.text("new swtcontrol.2   SwitchedObj=line.692675  SwitchedTerm=1 Action=o")
        self.dss.solution_solve()
        self.dss.swtcontrols_write_name('1')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_swtcontrols_first(self):
        expected = 1
        actual = self.dss.swtcontrols_first()
        assert expected == actual

    def test_swtcontrols_next(self):
        expected = 2
        actual = self.dss.swtcontrols_next()
        assert expected == actual

    def test_swtcontrols_read_action(self):
        expected = 2
        actual = self.dss.swtcontrols_read_action()
        assert expected == actual

    def test_swtcontrols_write_action(self):
        expected = 2
        self.dss.swtcontrols_write_action(expected)
        actual = self.dss.swtcontrols_read_action()
        assert expected == actual

    def test_swtcontrols_read_is_locked(self):
        expected = 1
        actual = self.dss.swtcontrols_read_is_locked()
        assert expected == actual

    def test_swtcontrols_write_is_locked(self):
        expected = 1
        self.dss.swtcontrols_write_is_locked(expected)
        actual = self.dss.swtcontrols_read_is_locked()
        assert expected == actual

    def test_swtcontrols_read_switched_term(self):
        expected = 1
        actual = self.dss.swtcontrols_read_switched_term()
        assert expected == actual

    def test_swtcontrols_write_switched_term(self):
        expected = 0
        self.dss.swtcontrols_write_switched_term(expected)
        actual = self.dss.swtcontrols_read_switched_term()
        assert expected == actual

    def test_swtcontrols_count(self):
        expected = 2
        actual = self.dss.swtcontrols_count()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_swtcontrols_read_delay(self):
        expected = 120
        actual = self.dss.swtcontrols_read_delay()
        assert expected == actual

    def test_swtcontrols_write_delay(self):
        expected = 160.0
        self.dss.swtcontrols_write_delay(expected)
        actual = self.dss.swtcontrols_read_delay()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_swtcontrols_read_name(self):
        expected = '1'
        actual = self.dss.swtcontrols_read_name()
        assert expected == actual

    def test_swtcontrols_write_name(self):
        expected = '2'
        self.dss.swtcontrols_write_name(expected)
        actual = self.dss.swtcontrols_read_name()
        assert expected == actual

    def test_swtcontrols_read_switched_obj(self):
        expected = 'line.650632'
        actual = self.dss.swtcontrols_read_switched_obj()
        assert expected == actual

    def test_swtcontrols_write_switched_obj(self):
        expected = 'line.692675'
        self.dss.swtcontrols_write_switched_obj(expected)
        actual = self.dss.swtcontrols_read_switched_obj()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_swtcontrols_all_names(self):
        expected = ['1', '2']
        actual = self.dss.swtcontrols_all_names()
