# -*- coding: utf-8 -*-
# @Time     : 09/13/2021 06:31 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_relays.py
# @Software : VSCode

import pytest


class TestRelays13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text(r"New Relay.test1 MonitoredObj=Line.650632 "
                 r"                MonitoredTerm=1 "
                 r"                Type=Current "
                 r"                PhaseTrip=800 "
                 r"                GroundTrip=250")
        dss.text(r"New Relay.test2 MonitoredObj=Line.632645 "
                 r"                MonitoredTerm=1 "
                 r"                Type=Current "
                 r"                PhaseTrip=800 "
                 r"                GroundTrip=250")
        dss.solution.solve()
        dss.relays.name = "test1"

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_relays_count(self, dss):
        expected = 2
        actual = dss.relays.count
        assert actual == expected

    def test_relays_first(self, dss):
        expected = 1
        actual = dss.relays.first()
        assert actual == expected

    def test_relays_next(self, dss):
        expected = 2
        actual = dss.relays.next()
        assert actual == expected

    def test_relays_read_monitored_term(self, dss):
        expected = 1
        actual = dss.relays.monitored_term
        assert actual == expected

    def test_relays_write_monitored_term(self, dss):
        expected = 2
        dss.relays.monitored_term = expected
        actual = dss.relays.monitored_term
        assert actual == expected

    def test_relays_read_switched_term(self, dss):
        exptected = 1
        actual = dss.relays.switched_term
        assert exptected == actual

    def test_relays_write_switched_term(self, dss):
        expected = 2
        dss.relays.switched_term = expected
        actual = dss.relays.switched_term
        assert actual == expected

    def test_relays_read_idx(self, dss):
        expected = 1
        actual = dss.relays.idx
        assert actual == expected

    def test_relays_write_idx(self, dss):
        expected = 2
        dss.relays.idx = expected
        actual = dss.relays.idx
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_relays_read_name(self, dss):
        expected = "test1"
        actual = dss.relays.name
        assert actual == expected

    def test_relays_write_name(self, dss):
        expected = "test2"
        dss.relays.name = expected
        actual = dss.relays.name
        assert actual == expected

    def test_relays_read_monitored_obj(self, dss):
        expected = "line.650632"
        actual = dss.relays.monitored_obj
        assert actual == expected

    def test_relays_write_monitored_obj(self, dss):
        expected = "line.632645"
        dss.relays.monitored_obj = expected
        actual = dss.relays.monitored_obj
        assert actual == expected

    def test_relays_read_switched_obj(self, dss):
        expected = "line.650632"
        actual = dss.relays.switched_obj
        assert actual == expected

    def test_relays_write_switched_obj(self, dss):
        expected = "line.632645"
        dss.relays.switched_obj = expected
        actual = dss.relays.switched_obj
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_relays_all_names(self, dss):
        expected = ["test1", "test2"]
        actual = dss.relays.names
        assert actual == expected
