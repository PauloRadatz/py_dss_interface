# -*- coding: utf-8 -*-
# @Time    : 7/31/2021 16:54 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_lines.py
# @Software: PyCharm

import pytest
import platform

class TestLines13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.lines_write_name('650632')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_lines_first(self):
        expected = 1
        actual = self.dss.lines_first()
        assert expected == actual

    def test_lines_next(self):
        expected = 2
        actual = self.dss.lines_next()
        assert expected == actual

    def test_lines_read_phases(self):
        expected = 3
        actual = self.dss.lines_read_phases()
        assert expected == actual

    def test_lines_write_phases(self):
        expected = 2
        self.dss.lines_write_phases(2)
        actual = self.dss.lines_read_phases()
        assert expected == actual

    def test_lines_num_cust(self):
        expected = 0
        actual = self.dss.lines_num_cust()
        assert expected == actual

    def test_lines_parent(self):
        expected = 0
        actual = self.dss.lines_parent()
        assert expected == actual

    def test_lines_count(self):
        expected = 12
        actual = self.dss.lines_count()
        assert expected == actual

    def test_lines_read_units(self):
        expected = 5
        actual = self.dss.lines_read_units()
        assert expected == actual

    def test_lines_write_units(self):
        expected = 3
        self.dss.lines_write_units(expected)
        actual = self.dss.lines_read_units()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_lines_read_name(self):
        expected = '650632'
        actual = self.dss.lines_read_name()
        assert expected == actual

    def test_lines_write_name(self):
        expected = '632670'
        self.dss.lines_write_name(expected)
        actual = self.dss.lines_read_name()
        assert expected == actual

    def test_lines_read_bus1(self):
        expected = 'rg60.1.2.3'
        actual = self.dss.lines_read_bus1()
        assert expected == actual

    def test_lines_write_bus1(self):
        expected = '670.1'
        self.dss.lines_write_bus1(expected)
        actual = self.dss.lines_read_bus1()
        assert expected == actual

    def test_lines_read_bus2(self):
        expected = '632.1.2.3'
        actual = self.dss.lines_read_bus2()
        assert expected == actual

    def test_lines_write_bus2(self):
        expected = '670.1'
        self.dss.lines_write_bus2(expected)
        actual = self.dss.lines_read_bus2()
        assert expected == actual

    def test_lines_read_linecode(self):
        expected = 'mtx601'
        actual = self.dss.lines_read_linecode()
        assert expected == actual

    def test_lines_write_linecode(self):
        expected = '723'
        self.dss.lines_write_linecode(expected)
        actual = self.dss.lines_read_linecode()
        assert expected == actual

    def test_lines_read_geometry(self):
        expected = ''
        actual = self.dss.lines_read_geometry()
        assert expected == actual

    def test_lines_write_geometry(self):

        self.dss.text("New WireData.1/0_ACSR Rac=0.646847 Runits=km GMRac=0.13589  GMRUnits=cm Radius=0.50546 Radunits=cm Normamps=260  Emergamps=260")
        self.dss.text("New LineGeometry.1PH-x4_ACSRx4_ACSR  nconds=2  nphases=1 "
                      " cond=1  wire=1/0_ACSR x=-0.1524 h=10.5156 units=m "
                      " cond=2  wire=1/0_ACSR x=0.1524  h=8.2296  units=m "
                      " reduce=y ")

        expected = '1PH-x4_ACSRx4_ACSR'.lower()
        self.dss.lines_write_geometry(expected)
        actual = self.dss.lines_read_geometry()
        assert expected == actual

    def test_lines_read_spacing(self):
        expected = ""
        actual = self.dss.lines_read_spacing()
        assert expected == actual

    def test_lines_write_spacing(self):
        self.dss.text("new LineSpacing.500 nconds=4 nphases=3 units=ft x=[-4 -1 3 0] h=[28 28 28 24]")
        expected = "500"
        self.dss.lines_write_spacing(expected)
        actual = self.dss.lines_read_spacing()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_lines_read_length(self):
        expected = 2000
        actual = self.dss.lines_read_length()
        assert expected == actual

    def test_lines_write_length(self):
        expected = 150
        self.dss.lines_write_length(expected)
        actual = self.dss.lines_read_length()
        assert expected == actual

    def test_lines_read_r1(self):

        self.dss.text("New linecode.Sequences nphases=3 "
                      "r1=0.3489 x1=0.426198 r0=0.588811 x0=1.29612 "
                      "c1=10.4308823411236  c0=4.48501282215346  "
                      "units=km baseFreq=60 normamps=310  emergamps=310  "
                      "faultrate=0.1 pctperm=20 repair=3")

        self.dss.text("New line.MyLine linecode=Sequences length=1")
        self.dss.lines_write_name('MyLine')

        expected = 0.3489
        actual = self.dss.lines_read_r1()
        assert expected == actual

    def test_lines_write_r1(self):
        # TODO issue with changing from liecodes
        self.dss.text("New linecode.Sequences nphases=3 "
                      "r1=0.3489 x1=0.426198 r0=0.588811 x0=1.29612 "
                      "c1=10.4308823411236  c0=4.48501282215346  "
                      "units=km baseFreq=60 normamps=310  emergamps=310  "
                      "faultrate=0.1 pctperm=20 repair=3")

        self.dss.text("New line.MyLine linecode=Sequences length=1")
        self.dss.lines_write_name('MyLine')

        expected = 0.1
        self.dss.lines_write_r1(expected)
        actual = self.dss.lines_read_r1()
        assert expected == actual

    def test_lines_read_x1(self):
        self.dss.text("New linecode.Sequences nphases=3 "
                      "r1=0.3489 x1=0.426198 r0=0.588811 x0=1.29612 "
                      "c1=10.4308823411236  c0=4.48501282215346  "
                      "units=km baseFreq=60 normamps=310  emergamps=310  "
                      "faultrate=0.1 pctperm=20 repair=3")

        self.dss.text("New line.MyLine linecode=Sequences length=1")
        self.dss.lines_write_name('MyLine')

        expected = 0.426198
        actual = self.dss.lines_read_x1()
        assert expected == actual

    def test_lines_write_x1(self):
        self.dss.text("New linecode.Sequences nphases=3 "
                      "r1=0.3489 x1=0.426198 r0=0.588811 x0=1.29612 "
                      "c1=10.4308823411236  c0=4.48501282215346  "
                      "units=km baseFreq=60 normamps=310  emergamps=310  "
                      "faultrate=0.1 pctperm=20 repair=3")

        self.dss.text("New line.MyLine linecode=Sequences length=1")
        self.dss.lines_write_name('MyLine')

        expected = 0.12
        self.dss.lines_write_x1(expected)
        actual = self.dss.lines_read_x1()
        assert expected == actual

    def test_lines_read_c1(self):
        if platform.architecture()[0] == "64bit":
            expected = 0.0006439569999378611
            actual = self.dss.lines_read_c1()
            assert expected == actual
        else:
            assert True

    def test_lines_write_c1(self):
        expected = 0.1
        self.dss.lines_write_c1(expected)
        actual = self.dss.lines_read_c1()
        assert expected == pytest.approx(actual)

    def test_lines_read_r0(self):
        if platform.architecture()[0] == "64bit":
            expected = 3.378880258497484e-05
            actual = self.dss.lines_read_r0()
            assert expected == actual
        else:
            assert True

    # TODO: Paulo - check if this is correct
    def test_lines_write_r0(self):
        expected = 1.5
        self.dss.lines_write_r0(expected)
        actual = self.dss.lines_read_r0()
        assert expected == actual

    def test_lines_read_x0(self):
        if platform.architecture()[0] == "64bit":
            expected = 7.664982290436836e-05
            actual = self.dss.lines_read_x0()
            assert expected == actual
        else:
            assert True

    def test_lines_write_x0(self):
        expected = 0.12
        self.dss.lines_write_x0(expected)
        actual = self.dss.lines_read_x0()
        assert expected == actual

    def test_lines_read_c0(self):
        if platform.architecture()[0] == "64bit":
            expected = 0.00030303858820605233
            actual = self.dss.lines_read_c0()
            assert expected == actual
        else:
            assert True

    # TODO: Paulo - check if this is correct
    def test_lines_write_c0(self):
        expected = 0.1
        self.dss.lines_write_c0(expected)
        actual = self.dss.lines_read_c0()
        assert expected == pytest.approx(actual)

    def test_lines_read_norm_amps(self):
        expected = 400
        actual = self.dss.lines_read_norm_amps()
        assert expected == actual

    def test_lines_write_norm_amps(self):
        expected = 500
        self.dss.lines_write_norm_amps(expected)
        actual = self.dss.lines_read_norm_amps()
        assert expected == actual

    def test_lines_read_emerg_amps(self):
        expected = 600
        actual = self.dss.lines_read_emerg_amps()
        assert expected == actual

    def test_lines_write_emerg_amps(self):
        expected = 500
        self.dss.lines_write_emerg_amps(expected)
        actual = self.dss.lines_read_emerg_amps()
        assert expected == actual

    def test_lines_read_rg(self):
        expected = 0.01805
        actual = self.dss.lines_read_rg()
        assert expected == actual

    def test_lines_write_rg(self):
        expected = 0.1
        self.dss.lines_write_rg(expected)
        actual = self.dss.lines_read_rg()
        assert expected == actual

    def test_lines_read_xg(self):
        expected = 0.155081
        actual = self.dss.lines_read_xg()
        assert expected == actual

    def test_lines_write_xg(self):
        expected = 0.1
        self.dss.lines_write_xg(expected)
        actual = self.dss.lines_read_xg()
        assert expected == actual

    def test_lines_read_rho(self):
        expected = 100
        actual = self.dss.lines_read_rho()
        assert expected == actual

    def test_lines_write_rho(self):
        expected = 0.1
        self.dss.lines_write_rho(expected)
        actual = self.dss.lines_read_rho()
        assert expected == actual

    def test_lines_read_season_rating(self):
        expected = 400
        actual = self.dss.lines_read_season_rating()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_lines_all_names(self):
        expected = ['650632', '632670', '670671', '671680', '632633', '632645', '645646', '692675', '671684', '684611', '684652', '671692']
        actual = self.dss.lines_all_names()
        assert expected == actual

    def test_lines_read_rmatrix(self):
        if platform.architecture()[0] == "64bit":
            expected = [6.562679425837321e-05, 2.9546262350090106e-05, 2.992506058534767e-05, 2.9546262350090106e-05, 6.392220219971417e-05, 2.9072764556018148e-05, 2.992506058534767e-05, 2.9072764556018148e-05, 6.466085875846642e-05]
            actual = self.dss.lines_read_rmatrix()
            assert expected == actual
        else:
            assert True

    def test_lines_write_rmatrix(self):
        if platform.architecture()[0] == "64bit":
            expected = [1.3569, 0.4591, 0.0, 0.4591, 1.3471, 0.0, 0.0, 0.0, 0.0]
            self.dss.lines_write_rmatrix("[1.3569 | 0.4591 1.3471]")
            actual = self.dss.lines_read_rmatrix()
            assert expected == actual
        else:
            assert True

    def test_lines_read_xmatrix(self):
        if platform.architecture()[0] == "64bit":
            expected = [0.00019278936183433795, 9.502153731436029e-05, 8.022946622755235e-05, 9.502153731436029e-05, 0.00019845239545143855, 7.289972037531847e-05, 8.022946622755235e-05, 7.289972037531847e-05, 0.00019599020692226434]
            actual = self.dss.lines_read_xmatrix()
            assert expected == actual
        else:
            assert True

    def test_lines_write_xmatrix(self):
        if platform.architecture()[0] == "64bit":
            expected = [1.3569, 0.4591, 0.0, 0.4591, 1.3471, 0.0, 0.0, 0.0, 0.0]
            self.dss.lines_write_xmatrix("[1.3569 | 0.4591 1.3471]")
            actual = self.dss.lines_read_xmatrix()
            assert True
        else:
            assert expected == actual

    def test_lines_read_yprim(self):
        if platform.architecture()[0] == "64bit":
            expected = [1.145090744360685, -3.300510291288572, -0.48584426251815294, 1.2202578674652254, -0.2660516357083827, 0.9121918679463129, -1.145090744360685, 3.3005104912135703, 0.48584426251815294, -1.2202579103062965, 0.2660516357083827, -0.912191910787384, -0.48584426251815294, 1.2202578674652254, 1.0026837665363062, -3.1275594644757296, -0.12630137793675839, 0.6966811885852322, 0.48584426251815294, -1.2202579103062965, -1.0026837665363062, 3.127559664400728, 0.12630137793675839, -0.6966812314263033, -0.2660516357083827, 0.9121918679463129, -0.12630137793675839, 0.6966811885852322, 0.886694192671301, -2.950512888096904, 0.2660516357083827, -0.912191910787384, 0.12630137793675839, -0.6966812314263033, -0.886694192671301, 2.950513088021902, -1.145090744360685, 3.3005104912135703, 0.48584426251815294, -1.2202579103062965, 0.2660516357083827, -0.912191910787384, 1.145090744360685, -3.300510291288572, -0.48584426251815294, 1.2202578674652254, -0.2660516357083827, 0.9121918679463129, 0.48584426251815294, -1.2202579103062965, -1.0026837665363062, 3.127559664400728, 0.12630137793675839, -0.6966812314263033, -0.48584426251815294, 1.2202578674652254, 1.0026837665363062, -3.1275594644757296, -0.12630137793675839, 0.6966811885852322, 0.2660516357083827, -0.912191910787384, 0.12630137793675839, -0.6966812314263033, -0.886694192671301, 2.950513088021902, -0.2660516357083827, 0.9121918679463129, -0.12630137793675839, 0.6966811885852322, 0.886694192671301, -2.950512888096904]
            actual = self.dss.lines_read_yprim()
            assert expected == actual
        else:
            assert True

    def test_lines_write_yprim(self):
        assert True
        # if platform.architecture()[0] == "64bit":
        #     expected = [1.3569, 0.4591, 0.0, 0.4591, 1.3471, 0.0, 0.0, 0.0, 0.0]
        #     self.dss.lines_write_yprim("[1.3569 | 0.4591 1.3471]")
        #     actual = self.dss.lines_read_yprim()
        #     assert expected == actual
        # else:
        #     assert True
