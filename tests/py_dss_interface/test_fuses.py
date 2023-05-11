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
        dss.fuses.name = "Fuse1"

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_fuses_first(self, dss):
        expected = 1
        actual = dss.fuses.first()
        assert actual == expected

    def test_fuses_next(self, dss):
        expected = 2
        actual = dss.fuses.next()
        assert actual == expected

    def test_fuses_count(self, dss):
        expected = 2
        actual = dss.fuses.count
        assert actual == expected

    def test_fuses_open(self, dss):
        expected = 0
        actual = dss.fuses.open
        assert actual == expected

    def test_fuses_close(self, dss):
        expected = ['closed']
        dss.fuses.close()
        actual = dss.fuses.state
        assert actual == expected

    def test_fuses_num_phases(self, dss):
        expected = 1
        actual = dss.fuses.num_phases
        assert actual == expected

    def test_fuses_read_monitored_term(self, dss):
        expected = 1
        actual = dss.fuses.monitored_term
        assert actual == expected

    def test_fuses_write_monitored_term(self, dss):
        expected = 2
        dss.fuses.monitored_term = expected
        actual = dss.fuses.monitored_term
        assert actual == expected

    def test_fuses_read_switched_term(self, dss):
        expected = 1
        actual = dss.fuses.switched_term
        assert actual == expected

    def test_fuses_write_switched_term(self, dss):
        expected = 2
        dss.fuses.switched_term = expected
        actual = dss.fuses.switched_term
        assert actual == expected

    def test_fuses_read_idx(self, dss):
        expected = 1
        actual = dss.fuses.idx
        assert actual == expected

    def test_fuses_write_idx(self, dss):
        expected = 2
        dss.fuses.idx = expected
        actual = dss.fuses.idx
        assert actual == expected

    def test_fuses_is_blown(self, dss):
        expected = 0
        actual = dss.fuses.is_blown
        assert actual == expected

    def test_fuses_reset_delay(self, dss):
        expected = 0
        actual = dss.fuses.reset()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_fuses_read_name(self, dss):
        expected = "fuse1"
        actual = dss.fuses.name
        assert expected.lower() == actual.lower()

    def test_fuses_write_name(self, dss):
        expected = "fuse2"
        dss.fuses.name = expected
        actual = dss.fuses.name
        assert expected.lower() == actual.lower()

    def test_fuses_read_switched_obj(self, dss):
        expected = 'line.684652'
        actual = dss.fuses.switched_obj
        assert actual == expected

    def test_fuses_write_switched_obj(self, dss):
        expected = 'line.684611'
        dss.fuses.switched_obj = expected
        actual = dss.fuses.switched_obj
        assert actual == expected

    def test_fuses_read_tcc_curve(self, dss):
        expected = 'tlink'
        actual = dss.fuses.tcc_curve
        assert actual == expected

    def test_fuses_write_tcc_curve(self, dss):
        expected = 'tlink2'
        dss.fuses.tcc_curve = expected
        actual = dss.fuses.tcc_curve
        assert actual == expected

    def test_fuses_read_monitored_obj(self, dss):
        expected = 'line.684652'
        actual = dss.fuses.monitored_obj
        assert actual == expected

    def test_fuses_write_monitored_obj(self, dss):
        expected = 'line.684111'
        dss.fuses.monitored_obj = expected
        actual = dss.fuses.monitored_obj
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_fuses_read_rated_current(self, dss):
        expected = 10
        actual = dss.fuses.rated_current
        assert actual == expected

    def test_fuses_write_rated_current(self, dss):
        expected = 12
        dss.fuses.rated_current = expected
        actual = dss.fuses.rated_current
        assert actual == expected

    def test_fuses_read_delay(self, dss):
        expected = 0
        actual = dss.fuses.delay
        assert actual == expected

    def test_fuses_write_delay(self, dss):
        expected = 2
        dss.fuses.delay = expected
        actual = dss.fuses.delay
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_fuses_all_names(self, dss):
        expected = ["fuse1", "fuse2"]
        actual = dss.fuses.names
        assert actual == expected

    def test_fuses_read_state(self, dss):
        expected = ['closed']
        actual = dss.fuses.state
        assert actual == expected

    # # TODO: Correct this test
    # def test_fuses_write_state(self, dss):
    #     expected = ["closed"]
    #     dss.fuses.state = expected
    #     actual = dss.fuses.state
    #     # assert actual == expected

    def test_fuses_read_normal(self, dss):
        expected = ['closed']
        actual = dss.fuses.normal
        assert actual == expected

    def test_fuses_write_normal(self, dss):
        # TODO
        expected = [0, 0, 0]
        # dss.fuses.normal = expected
        # actual = dss.fuses.normal
        # assert actual == expected

