# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 06:31 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_relays.py
# @Software : VSCode

import pytest


class TestRelays13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text(r"New Relay.test1 MonitoredObj=Line.650632 "
                      r"                MonitoredTerm=1 "
                      r"                Type=Current "
                      r"                PhaseTrip=800 "
                      r"                GroundTrip=250")
        self.dss.text(r"New Relay.test2 MonitoredObj=Line.632645 "
                      r"                MonitoredTerm=1 "
                      r"                Type=Current "
                      r"                PhaseTrip=800 "
                      r"                GroundTrip=250")
        self.dss.solution_solve()
        self.dss.relays_write_name("test1")

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_relays_count(self):
        expected = 2
        actual = self.dss.relays_count()
        assert expected == actual

    def test_relays_first(self):
        expected = 1
        actual = self.dss.relays_first()
        assert expected == actual

    def test_relays_next(self):
        expected = 2
        actual = self.dss.relays_next()
        assert expected == actual

    def test_relays_read_monitored_term(self):
        expected = 1
        actual = self.dss.relays_read_monitored_term()
        assert expected == actual

    def test_relays_write_monitored_term(self):
        expected = 2
        self.dss.relays_write_monitored_term(expected)
        actual = self.dss.relays_read_monitored_term()
        assert expected == actual

    def test_relays_read_switched_term(self):
        exptected = 1
        actual = self.dss.relays_read_switched_term()
        assert exptected == actual

    def test_relays_write_switched_term(self):
        expected = 2
        self.dss.relays_write_switched_term(expected)
        actual = self.dss.relays_read_switched_term()
        assert expected == actual

    def test_relays_read_idx(self):
        expected = 1
        actual = self.dss.relays_read_idx()
        assert expected == actual

    def test_relays_write_idx(self):
        expected = 2
        self.dss.relays_write_idx(expected)
        actual = self.dss.relays_read_idx()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_relays_read_name(self):
        expected = "test1"
        actual = self.dss.relays_read_name()
        assert expected == actual

    def test_relays_write_name(self):
        expected = "test2"
        self.dss.relays_write_name(expected)
        actual = self.dss.relays_read_name()
        assert expected == actual

    def test_relays_read_monitored_obj(self):
        expected = "line.650632"
        actual = self.dss.relays_read_monitored_obj()
        assert expected == actual

    def test_relays_write_monitored_obj(self):
        expected = "line.632645"
        self.dss.relays_write_monitored_obj(expected)
        actual = self.dss.relays_read_monitored_obj()
        assert expected == actual

    def test_relays_read_switched_obj(self):
        expected = "line.650632"
        actual = self.dss.relays_read_switched_obj()
        assert expected == actual

    def test_relays_write_switched_obj(self):
        expected = "line.632645"
        self.dss.relays_write_switched_obj(expected)
        actual = self.dss.relays_read_switched_obj()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_relays_all_names(self):
        expected = ["test1", "test2"]
        actual = self.dss.relays_all_names()
        assert expected == actual
