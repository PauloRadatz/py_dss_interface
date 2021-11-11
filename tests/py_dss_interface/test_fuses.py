# -*- coding: utf-8 -*-
# @Time    : 8/22/2021 07:43 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_fuses.py
# @Software: PyCharm


import pytest


class TestFuses13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("New TCC_Curve.tlink  npts=7 C_array=[ 2 2.1 3 4 6 22 50] T_array=[ 300 100 10.1 4 1.4 0.1 0.02]")
        dss.text("New TCC_Curve.tlink2 npts=7 C_array=[ 2 2.1 3 4 6 22 50] T_array=[ 300 100 10.1 4 1.4 0.1 0.02]")
        dss.text("New Fuse.Fuse1   LINE.684652   1 fusecurve=tlink   Ratedcurrent=10")
        dss.text("New Fuse.Fuse2   LINE.684611   1 fusecurve=tlink2  Ratedcurrent=15")
        dss.fuses_write_name("Fuse1")

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_fuses_first(self, dss):
        expected = 1
        actual = dss.fuses_first()
        assert actual == expected

    def test_fuses_next(self, dss):
        expected = 2
        actual = dss.fuses_next()
        assert actual == expected

    def test_fuses_count(self, dss):
        expected = 2
        actual = dss.fuses_count()
        assert actual == expected

    def test_fuses_open(self, dss):
        expected = 0
        actual = dss.fuses_open()
        assert actual == expected

    def test_fuses_close(self, dss):
        expected = ['closed']
        dss.fuses_close()
        actual = dss.fuses_read_state()
        assert actual == expected

    def test_fuses_num_phases(self, dss):
        expected = 1
        actual = dss.fuses_num_phases()
        assert actual == expected

    def test_fuses_read_monitored_term(self, dss):
        expected = 1
        actual = dss.fuses_read_monitored_term()
        assert actual == expected

    def test_fuses_write_monitored_term(self, dss):
        expected = 2
        dss.fuses_write_monitored_term(expected)
        actual = dss.fuses_read_monitored_term()
        assert actual == expected

    def test_fuses_read_switched_term(self, dss):
        expected = 1
        actual = dss.fuses_read_switched_term()
        assert actual == expected

    def test_fuses_write_switched_term(self, dss):
        expected = 2
        dss.fuses_write_switched_term(expected)
        actual = dss.fuses_read_switched_term()
        assert actual == expected

    def test_fuses_read_idx(self, dss):
        expected = 1
        actual = dss.fuses_read_idx()
        assert actual == expected

    def test_fuses_write_idx(self, dss):
        expected = 2
        dss.fuses_write_idx(expected)
        actual = dss.fuses_read_idx()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_fuses_read_name(self, dss):
        expected = "fuse1"
        actual = dss.fuses_read_name()
        assert expected.lower() == actual.lower()

    def test_fuses_write_name(self, dss):
        expected = "fuse2"
        dss.fuses_write_name(expected)
        actual = dss.fuses_read_name()
        assert expected.lower() == actual.lower()

    def test_fuses_read_switched_obj(self, dss):
        expected = 'line.684652'
        actual = dss.fuses_read_switched_obj()
        assert actual == expected

    def test_fuses_write_switched_obj(self, dss):
        expected = 'line.684611'
        dss.fuses_write_switched_obj(expected)
        actual = dss.fuses_read_switched_obj()
        assert actual == expected

    def test_fuses_read_tcc_curve(self, dss):
        expected = 'tlink'
        actual = dss.fuses_read_tcc_curve()
        assert actual == expected

    def test_fuses_write_tcc_curve(self, dss):
        expected = 'tlink2'
        dss.fuses_write_tcc_curve(expected)
        actual = dss.fuses_read_tcc_curve()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_fuses_read_rated_current(self, dss):
        expected = 10
        actual = dss.fuses_read_rated_current()
        assert actual == expected

    def test_fuses_write_rated_current(self, dss):
        expected = 12
        dss.fuses_write_rated_current(expected)
        actual = dss.fuses_read_rated_current()
        assert actual == expected

    def test_fuses_read_delay(self, dss):
        expected = 0
        actual = dss.fuses_read_delay()
        assert actual == expected

    def test_fuses_write_delay(self, dss):
        expected = 2
        dss.fuses_write_delay(expected)
        actual = dss.fuses_read_delay()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_fuses_all_names(self, dss):
        expected = ["fuse1", "fuse2"]
        actual = dss.fuses_all_names()
        assert actual == expected
