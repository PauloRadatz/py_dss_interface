# -*- coding: utf-8 -*-
# @Time    : 8/22/2021 07:43 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_fuses.py
# @Software: PyCharm


import pytest
import platform

class TestFuses13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("New TCC_Curve.tlink  npts=7 C_array=[ 2 2.1 3 4 6 22 50] T_array=[ 300 100 10.1 4 1.4 0.1 0.02]")
        self.dss.text("New TCC_Curve.tlink2 npts=7 C_array=[ 2 2.1 3 4 6 22 50] T_array=[ 300 100 10.1 4 1.4 0.1 0.02]")
        self.dss.text("New Fuse.Fuse1   LINE.684652   1 fusecurve=tlink   Ratedcurrent=10")
        self.dss.text("New Fuse.Fuse2   LINE.684611   1 fusecurve=tlink2  Ratedcurrent=15")
        self.dss.fuses_write_name("Fuse1")

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_fuses_first(self):
        expected = 1
        actual = self.dss.fuses_first()
        assert expected == actual

    def test_fuses_next(self):
        expected = 2
        actual = self.dss.fuses_next()
        assert expected == actual

    def test_fuses_count(self):
        expected = 2
        actual = self.dss.fuses_count()
        assert expected == actual

    def test_fuses_open(self):
        expected = 0
        actual = self.dss.fuses_open()
        assert expected == actual

    def test_fuses_close(self):
        expected = ['closed']
        self.dss.fuses_close()
        actual = self.dss.fuses_read_state()
        assert expected == actual

    def test_fuses_num_phases(self):
        expected = 1
        actual = self.dss.fuses_num_phases()
        assert expected == actual

    def test_fuses_read_monitored_term(self):
        expected = 1
        actual = self.dss.fuses_read_monitored_term()
        assert expected == actual

    def test_fuses_write_monitored_term(self):
        expected = 2
        self.dss.fuses_write_monitored_term(expected)
        actual = self.dss.fuses_read_monitored_term()
        assert expected == actual

    def test_fuses_read_switched_term(self):
        expected = 1
        actual = self.dss.fuses_read_switched_term()
        assert expected == actual

    # TODO: Not writing value, always return 0
    def test_fuses_write_switched_term(self):
        expected = 2
        self.dss.fuses_write_switched_term(expected)
        actual = self.dss.fuses_read_switched_term()
        assert expected == actual

    def test_fuses_read_idx(self):
        expected = 1
        actual = self.dss.fuses_read_idx()
        assert expected == actual

    # TODO: not writing idx, always return 1
    def test_fuses_write_idx(self):
        expected = 2
        self.dss.fuses_write_idx(expected)
        actual = self.dss.fuses_read_idx()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_fuses_read_name(self):
        expected = "fuse1"
        actual = self.dss.fuses_read_name()
        assert expected.lower() == actual.lower()

    def test_fuses_write_name(self):
        expected = "fuse2"
        self.dss.fuses_write_name(expected)
        actual = self.dss.fuses_read_name()
        assert expected.lower() == actual.lower()

    def test_fuses_read_switched_obj(self):
        expected = 'line.684652'
        actual = self.dss.fuses_read_switched_obj()
        assert expected == actual

    def test_fuses_write_switched_obj(self):
        expected = 'line.684611'
        self.dss.fuses_write_switched_obj(expected)
        actual = self.dss.fuses_read_switched_obj()
        assert expected == actual

    def test_fuses_read_tcc_curve(self):
        expected = 'tlink'
        actual = self.dss.fuses_read_tcc_curve()
        assert expected == actual

    def test_fuses_write_tcc_curve(self):
        expected = 'tlink2'
        self.dss.fuses_write_tcc_curve(expected)
        actual = self.dss.fuses_read_tcc_curve()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_fuses_read_rated_current(self):
        expected = 10
        actual = self.dss.fuses_read_rated_current()
        assert expected == actual

    def test_fuses_write_rated_current(self):
        expected = 12
        self.dss.fuses_write_rated_current(expected)
        actual = self.dss.fuses_read_rated_current()
        assert expected == actual

    def test_fuses_read_delay(self):
        expected = 0
        actual = self.dss.fuses_read_delay()
        assert expected == actual

    # TODO: Not writing value for delay, always return -1
    def test_fuses_write_delay(self):
        expected = 2
        self.dss.fuses_write_delay(expected)
        actual = self.dss.fuses_read_delay()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_fuses_all_names(self):
        expected = ["fuse1", "fuse2"]
        actual = self.dss.fuses_all_names()
        assert expected == actual

