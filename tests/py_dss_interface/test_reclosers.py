# -*- coding: utf-8 -*-
# @Time     : 09/07/2021 02:16 AM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_reclosers.py
# @Software : VSCode

import pytest


class TestReclosers13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text(r"New 'Recloser.cb1' MonitoredObj=Line.650632 "
                      r"MonitoredTerm=1  "
                      r"NumFast=4       "
                      r"PhaseFast=Ext_Inv "
                      r"PhaseDelayed=Ext_Inv "
                      r"PhaseTrip=800 "
                      r"TDPhFast=1 "
                      r"TDPhDelayed=1 "
                      r"PhaseInst=2400 "
                      r"GroundFast=Ext_Inv "
                      r"GroundDelayed=Ext_Inv "
                      r"GroundTrip=800 "
                      r"TDGrFast=1 "
                      r"TDGrDelayed=1 "
                      r"GroundInst=1200 "
                      r"Shots=4 "
                      r"RecloseIntervals=(0.5, 2, 2, )")
        self.dss.solution_solve()
        self.dss.reclosers_write_name('cb1')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_reclosers_count(self):
        self.dss.text(r"New 'Recloser.cb2' MonitoredObj=Line.684611 "
                      r"MonitoredTerm=2 "
                      r"NumFast=4 "
                      r"PhaseFast=Ext_Inv "
                      r"PhaseDelayed=Ext_Inv "
                      r"PhaseTrip=800 "
                      r"TDPhFast=1 "
                      r"TDPhDelayed=1 "
                      r"PhaseInst=2400 "
                      r"GroundFast=Ext_Inv "
                      r"GroundDelayed=Ext_Inv "
                      r"GroundTrip=800 "
                      r"TDGrFast=1 "
                      r"TDGrDelayed=1 "
                      r"GroundInst=1200 "
                      r"Shots=4 "
                      r"RecloseIntervals=(0.5, 2, 2, )")
        expected = 2
        actual = self.dss.reclosers_count()
        assert actual == expected

    def test_reclosers_first(self):
        expected = 1
        actual = self.dss.reclosers_first()
        assert actual == expected

    def test_reclosers_next(self):
        self.dss.text(r"New 'Recloser.cb2' MonitoredObj=Line.684611 "
                      r"MonitoredTerm=2 "
                      r"NumFast=4 "
                      r"PhaseFast=Ext_Inv "
                      r"PhaseDelayed=Ext_Inv "
                      r"PhaseTrip=800 "
                      r"TDPhFast=1 "
                      r"TDPhDelayed=1 "
                      r"PhaseInst=2400 "
                      r"GroundFast=Ext_Inv "
                      r"GroundDelayed=Ext_Inv "
                      r"GroundTrip=800 "
                      r"TDGrFast=1 "
                      r"TDGrDelayed=1 "
                      r"GroundInst=1200 "
                      r"Shots=4 "
                      r"RecloseIntervals=(0.5, 2, 2, )")
        expected = 2
        self.dss.reclosers_first()
        actual = self.dss.reclosers_next()
        assert actual == expected

    def test_reclosers_read_monitored_term(self):
        expected = 1
        actual = self.dss.reclosers_read_monitored_term()
        assert actual == expected

    def test_reclosers_write_monitored_term(self):
        expected = 2
        self.dss.reclosers_write_monitored_term(expected)
        actual = self.dss.reclosers_read_monitored_term()
        assert actual == expected

    def test_reclosers_read_switched_term(self):
        expected = 1
        actual = self.dss.reclosers_read_switched_term()
        assert actual == expected

    def test_reclosers_write_switched_term(self):
        expected = 2
        self.dss.reclosers_write_switched_term(expected)
        actual = self.dss.reclosers_read_switched_term()
        assert actual == expected

    def test_reclosers_read_num_fast(self):
        expected = 4
        actual = self.dss.reclosers_read_num_fast()
        assert actual == expected

    def test_reclosers_write_num_fast(self):
        expected = 1
        self.dss.reclosers_write_num_fast(expected)
        actual = self.dss.reclosers_read_num_fast()
        assert actual == expected

    def test_reclosers_read_shots(self):
        expected = 4
        actual = self.dss.reclosers_read_shots()
        assert actual == expected

    def test_reclosers_write_shots(self):
        expected = 3
        self.dss.reclosers_write_shots(expected)
        actual = self.dss.reclosers_read_shots()
        assert actual == expected

    def test_reclosers_open(self):
        expected = 0
        actual = self.dss.reclosers_open()
        assert actual == expected

    def test_reclosers_close(self):
        expected = 0
        actual = self.dss.reclosers_close()
        assert actual == expected

    def test_reclosers_read_idx(self):
        expected = 1
        actual = self.dss.reclosers_read_idx()
        assert actual == expected

    def test_reclosers_write_idx(self):
        self.dss.text(r"New 'Recloser.cb2' MonitoredObj=Line.684611 "
                      r"MonitoredTerm=2 "
                      r"NumFast=4 "
                      r"PhaseFast=Ext_Inv "
                      r"PhaseDelayed=Ext_Inv "
                      r"PhaseTrip=800 "
                      r"TDPhFast=1 "
                      r"TDPhDelayed=1 "
                      r"PhaseInst=2400 "
                      r"GroundFast=Ext_Inv "
                      r"GroundDelayed=Ext_Inv "
                      r"GroundTrip=800 "
                      r"TDGrFast=1 "
                      r"TDGrDelayed=1 "
                      r"GroundInst=1200 "
                      r"Shots=4 "
                      r"RecloseIntervals=(0.5, 2, 2, )")
        expected = 2
        self.dss.reclosers_write_idx(expected)
        actual = self.dss.reclosers_read_idx()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_reclosers_read_phase_trip(self):
        expected = 800
        actual = self.dss.reclosers_read_phase_trip()
        assert actual == expected

    def test_reclosers_write_phase_trip(self):
        expected = 700
        self.dss.reclosers_write_phase_trip(expected)
        actual = self.dss.reclosers_read_phase_trip()
        assert actual == expected

    def test_reclosers_read_phase_inst(self):
        expected = 2400
        actual = self.dss.reclosers_read_phase_inst()
        assert actual == expected

    def test_reclosers_write_phase_inst(self):
        expected = 1200
        self.dss.reclosers_write_phase_inst(expected)
        actual = self.dss.reclosers_read_phase_inst()
        assert actual == expected

    def test_reclosers_read_ground_trip(self):
        expected = 800
        actual = self.dss.reclosers_read_ground_trip()
        assert actual == expected

    def test_reclosers_write_ground_trip(self):
        expected = 700
        self.dss.reclosers_write_ground_trip(expected)
        actual = self.dss.reclosers_read_ground_trip()
        assert actual == expected

    def test_reclosers_read_ground_inst(self):
        expected = 1200
        actual = self.dss.reclosers_read_ground_inst()
        assert actual == expected

    def test_reclosers_write_ground_inst(self):
        expected = 1900
        self.dss.reclosers_write_ground_inst(expected)
        actual = self.dss.reclosers_read_ground_inst()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_reclosers_read_name(self):
        expected = 'cb1'
        actual = self.dss.reclosers_read_name()
        assert actual == expected

    def test_reclosers_write_name(self):
        self.dss.text(r"New 'Recloser.cb2' MonitoredObj=Line.684611 "
                      r"MonitoredTerm=2 "
                      r"NumFast=4 "
                      r"PhaseFast=Ext_Inv "
                      r"PhaseDelayed=Ext_Inv "
                      r"PhaseTrip=800 "
                      r"TDPhFast=1 "
                      r"TDPhDelayed=1 "
                      r"PhaseInst=2400 "
                      r"GroundFast=Ext_Inv "
                      r"GroundDelayed=Ext_Inv "
                      r"GroundTrip=800 "
                      r"TDGrFast=1 "
                      r"TDGrDelayed=1 "
                      r"GroundInst=1200 "
                      r"Shots=4 "
                      r"RecloseIntervals=(0.5, 2, 2, )")
        expected = 'cb2'
        self.dss.reclosers_write_name(expected)
        actual = self.dss.reclosers_read_name()
        assert actual == expected

    def test_reclosers_read_monitored_obj(self):
        expected = 'line.650632'
        actual = self.dss.reclosers_read_monitored_obj()
        assert actual == expected

    def test_reclosers_write_monitored_obj(self):
        expected = 'line.684652'
        self.dss.reclosers_write_monitored_obj(expected)
        actual = self.dss.reclosers_read_monitored_obj()
        assert actual == expected

    def test_reclosers_read_switched_obj(self):
        expected = 'line.650632'
        actual = self.dss.reclosers_read_switched_obj()
        assert actual == expected

    def test_reclosers_write_switched_obj(self):
        expected = 'line.684652'
        self.dss.reclosers_write_switched_obj(expected)
        actual = self.dss.reclosers_read_switched_obj()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_reclosers_all_names(self):
        self.dss.text(r"New 'Recloser.cb2' MonitoredObj=Line.684611 "
                      r"MonitoredTerm=2 "
                      r"NumFast=4 "
                      r"PhaseFast=Ext_Inv "
                      r"PhaseDelayed=Ext_Inv "
                      r"PhaseTrip=800 "
                      r"TDPhFast=1 "
                      r"TDPhDelayed=1 "
                      r"PhaseInst=2400 "
                      r"GroundFast=Ext_Inv "
                      r"GroundDelayed=Ext_Inv "
                      r"GroundTrip=800 "
                      r"TDGrFast=1 "
                      r"TDGrDelayed=1 "
                      r"GroundInst=1200 "
                      r"Shots=4 "
                      r"RecloseIntervals=(0.5, 2, 2, )")
        expected = ['cb1', 'cb2']
        actual = self.dss.reclosers_all_names()
        assert actual == expected

    def test_reclosers_reclose_intervals(self):
        expected = [0.5, 2, 2]
        actual = self.dss.reclosers_reclose_intervals()
        assert actual == expected
