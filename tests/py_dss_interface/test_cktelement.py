# -*- coding: utf-8 -*-
# @Time    : 6/4/2021 2:28 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_cktelement.py
# @Software: PyCharm

import platform
import pytest


class TestCktElement13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.circuit_set_active_element('Line.671692')

    def test_cktelement_num_terminals(self):
        expected = 2
        actual = self.dss.cktelement_num_terminals()
        assert actual == expected

    def test_cktelement_num_conductors(self):
        expected = 3
        actual = self.dss.cktelement_num_conductors()
        assert actual == expected

    def test_cktelement_num_phases(self):
        expected = 3
        actual = self.dss.cktelement_num_phases()
        assert actual == expected

    def test_cktelement_open(self):
        expected = 0
        actual = self.dss.cktelement_open(1)
        assert actual == expected

        expected = 1
        actual = self.dss.cktelement_is_open()
        assert actual == expected

    def test_cktelement_close(self):
        expected = 0
        actual = self.dss.cktelement_open(2)
        assert actual == expected

        expected = 1
        actual = self.dss.cktelement_is_open()
        assert actual == expected

        expected = 0
        actual = self.dss.cktelement_close(2)
        assert actual == expected

        expected = 0
        actual = self.dss.cktelement_is_open()
        assert actual == expected

    def test_cktelement_is_open(self):
        expected = 0
        actual = self.dss.cktelement_is_open()
        assert actual == expected

    def test_cktelement_num_properties(self):
        expected = 38
        actual = self.dss.cktelement_num_properties()
        assert actual == expected

    def test_cktelement_has_switch_control(self):
        expected = 0
        actual = self.dss.cktelement_has_switch_control()
        assert actual == expected

    def test_cktelement_has_volt_control(self):
        expected = 0
        actual = self.dss.cktelement_has_volt_control()
        assert actual == expected

    def test_cktelement_num_controls(self):
        expected = 0
        actual = self.dss.cktelement_num_controls()
        assert actual == expected

    def test_cktelement_ocp_dev_index(self):
        expected = 0
        actual = self.dss.cktelement_ocp_dev_index()
        assert actual == expected

    def test_cktelement_ocp_dev_type(self):
        expected = 0
        actual = self.dss.cktelement_ocp_dev_type()
        assert actual == expected

    def test_cktelement_read_enabled(self):
        expected = 1
        actual = self.dss.cktelement_read_enabled()
        assert actual == expected

    def test_cktelement_write_enabled(self):
        expected = 0
        actual = self.dss.cktelement_write_enabled(0)
        assert actual == expected

        expected = 0
        actual = self.dss.cktelement_read_enabled()
        assert actual == expected

    def test_cktelement_read_norm_amps(self):
        expected = 400.0
        actual = self.dss.cktelement_read_norm_amps()
        assert actual == expected

    def test_cktelement_write_norm_amps(self):
        expected = 0
        actual = self.dss.cktelement_write_norm_amps(100.0)
        assert actual == expected

        expected = 100.0
        actual = self.dss.cktelement_read_norm_amps()
        assert actual == expected

    def test_cktelement_read_emerg_amps(self):
        expected = 600.0
        actual = self.dss.cktelement_read_emerg_amps()
        assert actual == expected

    def test_cktelement_write_emerg_amps(self):
        expected = 0
        actual = self.dss.cktelement_write_emerg_amps(150.0)
        assert actual == expected

        expected = 150.0
        actual = self.dss.cktelement_read_emerg_amps()
        assert actual == expected

    # def test_cktelement_variable_i(self):
    #     expected = 3
    #     actual = self.dss.test_cktelement_variable_i()

    def test_cktelement_name(self):
        expected = 'Line.671692'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_cktelement_read_display(self):
        expected = 'Line_671692'
        actual = self.dss.cktelement_read_display()
        assert actual == expected

    def test_cktelement_write_display(self):
        expected = "My_Line_671692"
        self.dss.cktelement_write_display(expected)
        actual = self.dss.cktelement_read_display()
        assert actual == expected

    # def test_cktelement_guid(self):
    #     expected = '{32F8E32C-6A2A-4C64-8EFB-1B6358094F3B}'
    #     actual = self.dss.cktelement_guid()
    #     assert actual == expected

    def test_cktelement_energymeter(self):
        expected = "em1"
        self.dss.circuit_set_active_element('Line.650632')
        actual = self.dss.cktelement_energymeter()
        assert actual == expected

    def test_cktelement_controller(self):
        # https://github.com/PauloRadatz/py_dss_interface/issues/2 - Issue solved =)
        self.dss.text("New 'Fuse.f1' MonitoredObj=Line.650632 MonitoredTerm=1 FuseCurve=Klink RatedCurrent=65")
        # After include a new element it become the active element. So, we need activate another element to test the
        # methods below
        self.dss.circuit_set_active_element('Line.650632')
        expected = "Fuse.f1"
        actual = self.dss.cktelement_controller("1")
        assert actual == expected

    def test_cktelement_read_bus_names(self):
        expected = ['671', '692']
        actual = self.dss.cktelement_read_bus_names()
        assert actual == expected

    def test_cktelement_write_bus_names(self):
        expected = ['671_new', '692_new']
        self.dss.cktelement_write_bus_names(self.dss, expected)
        actual = self.dss.cktelement_read_bus_names()
        assert actual == expected

    def test_cktelement_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [2350.072713273249, -221.08573964636037, -1338.4057884569047, -2109.79926873945,
                        -1015.4001153916768,
                        2083.1115246837253, 2350.0726913632457, -221.08573234457813, -1338.4057922959519,
                        -2109.7992630653525, -1015.4001096022041, 2083.111507606075]
            actual = self.dss.cktelement_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]

    def test_cktelement_currents(self):
        if platform.architecture()[0] == "64bit":
            expected = [219.10003280639648, -73.017822265625, 38.39047050476074, -56.740970611572266,
                        -57.89472579956055,
                        170.77650451660156, -219.10003280639648, 73.017822265625, -38.39047050476074,
                        56.740970611572266,
                        57.89472579956055, -170.77650451660156]
            actual = self.dss.cktelement_currents()
            assert [round(value, 5) for value in actual] == [round(value, 5) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_powers(self):
        if platform.architecture()[0] == "64bit":
            expected = [531.0442078185483, 123.15729887953721, 68.33003035870593, -156.93863010669907,
                        414.5328159611783,
                        52.80531186087565, -531.0442024849057, -123.15729887953721, -68.33002988936931,
                        156.93863010669907,
                        -414.53281270953687, -52.80531186087562]
            actual = self.dss.cktelement_powers()
            assert [round(value, 4) for value in actual] == [round(value, 4) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_losses(self):
        if platform.architecture()[0] == "64bit":
            expected = [0.009054620692040771, 2.9103830456733704e-11]
            actual = self.dss.cktelement_losses()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_phase_losses(self):
        if platform.architecture()[0] == "64bit":
            expected = [5.333642708137631e-06, 0.0, 4.693366208812222e-07, 0.0, 3.251641406677663e-06,
                        2.9103830456733704e-14]
            actual = self.dss.cktelement_phase_losses()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_seq_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [82.60053533438243, 2391.57410131294, 42.214191911916366, 82.60053680171639, 2391.5740870438235,
                        42.21419682837919]
            actual = self.dss.cktelement_seq_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_seq_currents(self):
        if platform.architecture()[0] == "64bit":
            expected = [67.92228162359304, 142.8117989247778, 71.92650828459385, 67.92228162359304, 142.8117989247778,
                        71.92650828459385]
            actual = self.dss.cktelement_seq_currents()
            assert [round(value, 5) for value in actual] == [round(value, 5) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_seq_powers(self):
        if platform.architecture()[0] == "64bit":
            expected = [-3.6360767939550196, -16.43380473058689, 1023.7694666913296, 42.10653475759665,
                        -6.226334923023335,
                        -6.648749825544208, 3.6360781779859073, 16.43380473058686, -1023.7694605727667,
                        -42.10653475759662,
                        6.226336475050096, 6.648749825544179]
            actual = self.dss.cktelement_seq_powers()
            assert [round(value, 4) for value in actual] == [round(value, 4) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_all_property_names(self):
        expected = ['bus1', 'bus2', 'linecode', 'length', 'phases', 'r1', 'x1', 'r0', 'x0', 'C1', 'C0', 'rmatrix',
                    'xmatrix', 'cmatrix', 'Switch', 'Rg', 'Xg', 'rho', 'geometry', 'units', 'spacing', 'wires',
                    'EarthModel', 'cncables', 'tscables', 'B1', 'B0', 'Seasons', 'Ratings', 'LineType', 'normamps',
                    'emergamps', 'faultrate', 'pctperm', 'repair', 'basefreq', 'enabled', 'like']
        actual = self.dss.cktelement_all_property_names()
        assert actual == expected

    def test_cktelement_residuals(self):
        if platform.architecture()[0] == "64bit":
            expected = [203.766844870779, 11.612830692170245, 203.766844870779, -168.38716929815823]
            actual = self.dss.cktelement_residuals()
            assert [round(value, 5) for value in actual] == [round(value, 5) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_y_prim(self):
        if platform.architecture()[0] == "64bit":
            expected = [10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0,
                        0.0, 0.0, 0.0, 0.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0, 0.0,
                        0.0,
                        0.0, 0.0, -10000000.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0, 0.0, 0.0,
                        0.0,
                        0.0, 0.0, 0.0, -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                        0.0,
                        -10000000.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000000.0, 0.0]
            actual = self.dss.cktelement_y_prim()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_cplx_seq_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [-1.2443968584439062, -82.59116123402828, 2386.047643797686, -162.49099543761832,
                        -34.73053366599282,
                        23.996417025286178, -1.244403511636449, -82.59116260128519, 2386.0476296014167,
                        -162.49099388261294,
                        -34.730534726534245, 23.99642413931997]
            actual = self.dss.cktelement_cplx_seq_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_cplx_seq_currents(self):
        if platform.architecture()[0] == "64bit":
            expected = [66.53192583719891, 13.672570546468101, 141.96269129938753, -15.550054378231998,
                        10.605415669810057,
                        -71.1403384338611, -66.53192583719891, -13.672570546468101, -141.96269129938753,
                        15.550054378231998,
                        -10.605415669810057, 71.1403384338611]
            actual = self.dss.cktelement_cplx_seq_currents()
            assert [round(value, 5) for value in actual] == [round(value, 5) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_all_variables_names(self):
        # TODO: Paulo - https://github.com/PauloRadatz/py_dss_interface/issues/3
        self.dss.circuit_set_active_element("Load.611")
        expected = []
        actual = self.dss.cktelement_all_variables_names()
        assert actual == expected

    def test_cktelement_all_variables_values(self):
        # TODO: Paulo - https://github.com/PauloRadatz/py_dss_interface/issues/4
        expected = [0.0]
        actual = self.dss.cktelement_all_variables_values()
        assert actual == expected

    def test_cktelement_node_order(self):
        expected = [1, 2, 3, 1, 2, 3]
        actual = self.dss.cktelement_node_order()
        assert actual == expected

    def test_cktelement_currents_mag_ang(self):
        if platform.architecture()[0] == "64bit":
            expected = [230.9468050096784, -18.431295569374065, 68.50814529324387, -55.91804381551418,
                        180.32308163492308,
                        108.72710746089945, 230.9468050096784, 161.56870442095442, 68.50814529324387,
                        124.08195617481427,
                        180.32308163492308, -71.27289252942901]
            actual = self.dss.cktelement_currents_mag_ang()
            assert [round(value, 4) for value in actual] == [round(value, 4) for value in expected]
        else:
            assert 1 == 1

    def test_cktelement_voltages_mag_ang(self):
        if platform.architecture()[0] == "64bit":
            expected = [2360.449250025611, -5.3743473971090765, 2498.5161614343156, -122.39005677200043,
                        2317.4104122074677,
                        115.9866388766925, 2360.4492275280204, -5.3743472704625, 2498.516158699491, -122.39005691604211,
                        2317.410394319708, 115.98663893302955]
            actual = self.dss.cktelement_voltages_mag_ang()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]
        else:
            assert 1 == 1
