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
        dss.solution.solve()
        dss.swtcontrols.name = '1'

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_swtcontrols_first(self, dss):
        expected = 1
        actual = dss.swtcontrols.first()
        assert actual == expected

    def test_swtcontrols_next(self, dss):
        expected = 2
        actual = dss.swtcontrols.next()
        assert actual == expected

    def test_swtcontrols_open(self, dss):
        dss.swtcontrols.open()
        expected = ["open", "open", "open"]
        actual = dss.swtcontrols.state
        assert actual == expected

    def test_swtcontrols_close(self, dss):
        dss.swtcontrols.close()
        expected = ["closed", "closed", "closed"]
        actual = dss.swtcontrols.state
        assert actual == expected


    # TODO Does not work for C++
    def test_swtcontrols_read_is_locked(self, dss):
        dss.text("new swtcontrol.1  SwitchedObj=line.650632  SwitchedTerm=1 Action=c lock=Yes")
        dss.text("new swtcontrol.2  SwitchedObj=line.692675  SwitchedTerm=1 Action=o")
        dss.solution.solve()
        dss.swtcontrols.name = '1'
        expected = 1
        actual = dss.swtcontrols.is_locked
        assert actual == expected

    def test_swtcontrols_write_is_locked(self, dss):
        expected = 1
        dss.swtcontrols.is_locked = expected
        actual = dss.swtcontrols.is_locked
        assert actual == expected

    def test_swtcontrols_read_switched_term(self, dss):
        expected = 1
        actual = dss.swtcontrols.switched_term
        assert actual == expected

    def test_swtcontrols_write_switched_term(self, dss):
        dss.text("new swtcontrol.1  SwitchedObj=line.650632  SwitchedTerm=1 Action=c")
        dss.text("new swtcontrol.2  SwitchedObj=line.692675  SwitchedTerm=1 Action=o")
        dss.solution.solve()
        dss.swtcontrols.name = '1'
        expected = 0
        dss.swtcontrols.switched_term = expected
        actual = dss.swtcontrols.switched_term
        assert actual == expected

    def test_swtcontrols_count(self, dss):
        expected = 2
        actual = dss.swtcontrols.count
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================

    # ===================================================================
    # String methods
    # ===================================================================
    def test_swtcontrols_read_name(self, dss):
        expected = '1'
        actual = dss.swtcontrols.name
        assert actual == expected

    def test_swtcontrols_write_name(self, dss):
        expected = '2'
        dss.swtcontrols.name = expected
        actual = dss.swtcontrols.name
        assert actual == expected

    def test_swtcontrols_read_switched_obj(self, dss):
        expected = 'line.650632'
        actual = dss.swtcontrols.switched_obj
        assert actual == expected

    def test_swtcontrols_write_switched_obj(self, dss):
        expected = 'line.692675'
        dss.swtcontrols.switched_obj = expected
        actual = dss.swtcontrols.switched_obj
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_swtcontrols_names(self, dss):
        expected = ['1', '2']
        actual = dss.swtcontrols.names
        assert actual == expected

    def test_swtcontrols_read_state(self, dss):
        expected = ["closed", "closed", "closed"]
        actual = dss.swtcontrols.state
        assert expected == actual

    def test_swtcontrols_write_state(self, dss):
        expected = ["open", "open", "open"]
        dss.swtcontrols.state = expected
        actual = dss.swtcontrols.state
        assert expected == actual

    def test_swtcontrols_write_state_(self, dss):
        expected = ["open", "closed", "open"]
        dss.swtcontrols.state = expected
        actual = dss.swtcontrols.state
        assert expected == actual

    def test_swtcontrols_read_normal_state(self, dss):
        expected = ["closed", "closed", "closed"]
        actual = dss.swtcontrols.normal_state
        assert expected == actual

    def test_swtcontrols_write_normal_state(self, dss):
        expected = ["open", "open", "open"]
        dss.swtcontrols.normal_state = expected
        actual = dss.swtcontrols.normal_state
        assert expected == actual

    def test_swtcontrols_reset(self, dss):
        dss.swtcontrols.state = ["open", "open", "open"]
        expected = ["closed", "closed", "closed"]
        dss.swtcontrols.reset()
        actual = dss.swtcontrols.state
        assert actual == expected
