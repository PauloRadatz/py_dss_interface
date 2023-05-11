# -*- coding: utf-8 -*-
# @Time    : 6/4/2021 2:28 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_cktelement.py
# @Software: PyCharm

import platform
import pytest


class TestCktElement13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.circuit.set_active_element('Line.671692')

        return dss

    def test_num_terminals(self, dss):
        expected = 2
        actual = dss.cktelement.num_terminals
        assert actual == expected

    def test_num_conductors(self, dss):
        expected = 3
        actual = dss.cktelement.num_conductors
        assert actual == expected

    def test_num_phases(self, dss):
        expected = 3
        actual = dss.cktelement.num_phases
        assert actual == expected

    def test_open(self, dss):
        expected = 0
        actual = dss.cktelement.open_terminal(1)
        assert actual == expected

        expected = 1
        actual = dss.cktelement.is_terminal_open
        assert actual == expected

    def test_close(self, dss):
        expected = 0
        actual = dss.cktelement.open_terminal(2)
        assert actual == expected

        expected = 1
        actual = dss.cktelement.is_terminal_open
        assert actual == expected

        expected = 0
        actual = dss.cktelement.close_terminal(2)
        assert actual == expected

        expected = 0
        actual = dss.cktelement.is_terminal_open
        assert actual == expected

    def test_is_open(self, dss):
        expected = 0
        actual = dss.cktelement.is_terminal_open
        assert actual == expected

    def test_num_properties(self, dss):
        expected = 38
        actual = dss.cktelement.num_properties
        assert actual == expected

    def test_has_switch_control(self, dss):
        expected = 0
        actual = dss.cktelement.has_switch_control
        assert actual == expected

    def test_has_volt_control(self, dss):
        expected = 0
        actual = dss.cktelement.has_volt_control
        assert actual == expected

    def test_num_controls(self, dss):
        expected = 0
        actual = dss.cktelement.num_controls
        assert actual == expected

    def test_ocp_dev_index(self, dss):
        expected = 0
        actual = dss.cktelement.ocp_dev_index
        assert actual == expected

    def test_ocp_dev_type(self, dss):
        expected = 0
        actual = dss.cktelement.ocp_dev_type
        assert actual == expected

    def test_read_enabled(self, dss):
        expected = 1
        actual = dss.cktelement.is_enabled
        assert actual == expected

    def test_write_enabled(self, dss):
        expected = 0
        actual = dss.cktelement.enabled(0)
        assert actual == expected

        expected = 0
        actual = dss.cktelement.is_enabled
        assert actual == expected

    def test_read_norm_amps(self, dss):
        expected = 400.0
        actual = dss.cktelement.norm_amps
        assert actual == expected

    def test_write_norm_amps(self, dss):
        dss.cktelement.norm_amps = 100.0

        expected = 100.0
        actual = dss.cktelement.norm_amps
        assert actual == expected

    def test_read_emerg_amps(self, dss):
        expected = 600.0
        actual = dss.cktelement.emerg_amps
        assert actual == expected

    def test_write_emerg_amps(self, dss):
        dss.cktelement.emerg_amps = 150.0

        expected = 150.0
        actual = dss.cktelement.emerg_amps
        assert actual == expected

    # def test_variable_i(self, dss):
    #     expected = 3
    #     actual = dss.test_variable_i

    def test_name(self, dss):
        expected = 'Line.671692'
        actual = dss.cktelement.name
        assert actual == expected

    def test_read_display(self, dss):
        expected = 'Line_671692'
        actual = dss.cktelement.display
        assert actual == expected

    def test_write_display(self, dss):
        expected = "My_Line_671692"
        dss.cktelement.display = expected
        actual = dss.cktelement.display
        assert actual == expected

    # def test_guid(self, dss):
    #     expected = '{32F8E32C-6A2A-4C64-8EFB-1B6358094F3B}'
    #     actual = dss.cktelement.guid
    #     assert actual == expected

    def test_energymeter(self, dss):
        expected = "em1"
        dss.circuit.set_active_element('Line.650632')
        actual = dss.cktelement.energymeter
        assert actual == expected

    def test_controller(self, dss):
        # https://github.com/PauloRadatz/py_dss_interface/issues/2 - Issue solved =)
        dss.text("New 'Fuse.f1' MonitoredObj=Line.650632 MonitoredTerm=1 FuseCurve=Klink RatedCurrent=65")
        # After include a new element it becomes the active element. So, we need activate another element to test the
        # methods below
        dss.circuit.set_active_element('Line.650632')
        expected = "Fuse.f1"
        actual = dss.cktelement.controller("1")
        assert actual == expected

    def test_read_bus_names(self, dss):
        expected = ['671', '692']
        actual = dss.cktelement.bus_names
        assert actual == expected

    # def test_write_bus_names(self, dss):
    #     expected = ['671_new', '692_new']
    #     dss.cktelement.bus_names = expected
    #     # actual = dss.cktelement.bus_names
    #     # TODO does not work
    #     # assert actual == expected

    def test_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [2350.078784828011,
                        -221.0796482325248,
                        -1338.403130602017,
                        -2109.8005657418066,
                        -1015.4071554561633,
                        2083.115730276637,
                        2350.0787629180386,
                        -221.07964093085243,
                        -1338.4031344410694,
                        -2109.8005600677147,
                        -1015.4071496666263,
                        2083.115713199055]
            actual = dss.cktelement.voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_currents(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [219.09972381591797,
                        -73.0167236328125,
                        38.39052391052246,
                        -56.74092102050781,
                        -57.895368576049805,
                        170.77582168579102,
                        -219.09972381591797,
                        73.0167236328125,
                        -38.39052391052246,
                        56.74092102050781,
                        57.895368576049805,
                        -170.77582168579102]
            actual = dss.cktelement.currents
            assert [round(value, 21) for value in actual] == [round(value, 21) for value in expected]

    def test_powers(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [531.044124277299,
                        123.15656327805625,
                        68.33002988248371,
                        -156.93857539263408,
                        414.53317202448216,
                        52.804238327724974,
                        -531.0441189436859,
                        -123.15656327805621,
                        -68.33002941314726,
                        156.93857539263408,
                        -414.5331687728567,
                        -52.804238327724946]
            actual = dss.cktelement.powers
            assert [round(value, 22) for value in actual] == [round(value, 22) for value in expected]

    def test_losses(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [0.009054574999026954, 5.820766091e-11]
            actual = dss.cktelement.losses
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_phase_losses(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [5.33361313864589e-06,
                        2.910383e-14,
                        4.6933644625824e-07,
                        0.0,
                        3.25162545777857e-06,
                        2.910383e-14]
            actual = dss.cktelement.phase_losses
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_seq_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [82.5975271915408,
                        2391.5781387345023,
                        42.212520080202225,
                        82.59752865884842,
                        2391.5781244654136,
                        42.2125249966211]
            actual = dss.cktelement.seq_voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_seq_currents(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [67.92201952740132,
                        142.81151076209932,
                        71.9262709364223,
                        67.92201952740132,
                        142.81151076209932,
                        71.9262709364223]
            actual = dss.cktelement.seq_currents
            assert [round(value, 21) for value in actual] == [round(value, 21) for value in expected]

    def test_seq_powers(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [-3.6358786639185694,
                        -16.433154267781457,
                        1023.7692379604973,
                        42.103877880761,
                        -6.226032276396039,
                        -6.64849783208149,
                        3.6358800479387927,
                        16.43315426778146,
                        -1023.7692318419589,
                        -42.10387788076099,
                        6.226033828412597,
                        6.648497832081523]
            actual = dss.cktelement.seq_powers
            assert [round(value, 22) for value in actual] == [round(value, 22) for value in expected]

    def test_all_property_names(self, dss):
        expected = ['bus1', 'bus2', 'linecode', 'length', 'phases', 'r1', 'x1', 'r0', 'x0', 'C1', 'C0', 'rmatrix',
                    'xmatrix', 'cmatrix', 'Switch', 'Rg', 'Xg', 'rho', 'geometry', 'units', 'spacing', 'wires',
                    'EarthModel', 'cncables', 'tscables', 'B1', 'B0', 'Seasons', 'Ratings', 'LineType', 'normamps',
                    'emergamps', 'faultrate', 'pctperm', 'repair', 'basefreq', 'enabled', 'like']
        actual = dss.cktelement.property_names
        assert actual == expected

    def test_residuals(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [203.7660585822039, 11.613009723287425, 203.7660585822039, -168.38699026704103]
            actual = dss.cktelement.residuals_currents
            assert [round(value, 21) for value in actual] == [round(value, 21) for value in expected]

    def test_y_prim(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0,
                        0.0, 0.0, 0.0, 0.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0, 0.0,
                        0.0,
                        0.0, 0.0, -10000000.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0, 0.0, 0.0,
                        0.0,
                        0.0, 0.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                        0.0,
                        -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0]
            actual = dss.cktelement.y_prim
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_cplx_seq_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [-1.2438337433894162,
                        -82.58816123256486,
                        2386.0519864799708,
                        -162.48665018324897,
                        -34.72936790857,
                        23.995163183288923,
                        -1.2438403965520592,
                        -82.58816259983746,
                        2386.051972283723,
                        -162.48664862831083,
                        -34.729368969132395,
                        23.995170297295772]
            actual = dss.cktelement.cplx_seq_voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_cplx_seq_currents(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [66.53162638346356,
                        13.672725677490241,
                        141.96247509903242,
                        -15.549381656831635,
                        10.60562233342202,
                        -71.1400676534711,
                        -66.53162638346356,
                        -13.672725677490241,
                        -141.96247509903242,
                        15.549381656831635,
                        -10.60562233342202,
                        71.1400676534711]
            actual = dss.cktelement.cplx_seq_currents
            assert [round(value, 21) for value in actual] == [round(value, 21) for value in expected]

    def test_all_variables_names(self, dss):
        dss.text("New Storage.str bus=650 kw=50")
        dss.circuit.set_active_element("Storage.str")
        expected = ['kWh',
                    'State',
                    'kWOut',
                    'kWIn',
                    'kvarOut',
                    'DCkW',
                    'kWTotalLosses',
                    'kWInvLosses',
                    'kWIdlingLosses',
                    'kWChDchLosses',
                    'kWh Chng',
                    'InvEff',
                    'InverterON',
                    'Vref',
                    'Vavg (DRC)',
                    'VV Oper',
                    'VW Oper',
                    'DRC Oper',
                    'VV_DRC Oper',
                    'WP Oper',
                    'WV Oper',
                    'kWDesired',
                    'kW VW Limit',
                    'Limit kWOut Function',
                    'kVA Exceeded',
                    'Grid voltage',
                    'di/dt',
                    'it',
                    'it History',
                    'Rated VDC',
                    'Avg duty cycle',
                    'Target (Amps)',
                    'Series L',
                    'Max. Amps (phase)']
        actual = dss.cktelement.variables_names
        assert actual == expected

    # TODO gives warning
    def test_all_variables_values(self, dss):
        dss.text("New Storage.str bus=650 kw=50")
        dss.circuit.set_active_element("Storage.str")
        dss.text("solve")
        expected = [50.0,
                    1.0,
                    3.434007404732962,
                    0.0,
                    3.591803388852677e-09,
                    3.434007404732962,
                    0.6593341560814404,
                    0.0,
                    0.25,
                    0.4093341560814404,
                    0.0,
                    1.0,
                    1.0,
                    9999.0,
                    9999.0,
                    9999.0,
                    9999.0,
                    9999.0,
                    9999.0,
                    9999.0,
                    9999.0,
                    50.0,
                    9999.0,
                    25.0,
                    1.0,
                    0.0,
                    0.0,
                    0.0,
                    0.0,
                    8000.0,
                    0.0,
                    0.0,
                    0.0,
                    0.0]
        actual = dss.cktelement.variables_values
        assert actual == expected

    def test_node_order(self, dss):
        expected = [1, 2, 3, 1, 2, 3]
        actual = dss.cktelement.node_order
        assert actual == expected

    def test_currents_mag_ang(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [230.9461645195305,
                        -18.43106122609838,
                        68.50813414755872,
                        -55.91798358065394,
                        180.3226413266336,
                        108.72737054270922,
                        230.9461645195305,
                        161.5689387642301,
                        68.50813414755872,
                        124.08201640967452,
                        180.3226413266336,
                        -71.27262944761924]
            actual = dss.cktelement.currents_mag_ang
            assert [round(value, 22) for value in actual] == [round(value, 22) for value in expected]

    def test_voltages_mag_ang(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [2360.454724361668,
                        -5.374186385394906,
                        2498.515832891544,
                        -122.38998937216341,
                        2317.417277288996,
                        115.9867497775529,
                        2360.454701864133,
                        -5.374186258749673,
                        2498.5158301567194,
                        -122.38998951620508,
                        2317.4172594012734,
                        115.98674983388848]
            actual = dss.cktelement.voltages_mag_ang
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]
