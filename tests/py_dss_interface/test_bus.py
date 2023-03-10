# -*- coding: utf-8 -*-
# @Time    : 6/22/2021 5:48 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test.py
# @Software: PyCharm

import pytest
import platform


class TestBus13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.circuit.set_active_bus('692')

        return dss

    def test_num_nodes(self, dss):
        expected = 3
        actual = dss.bus.num_nodes
        assert actual == expected

    def test_zsc_refresh(self, dss):
        expected = 1
        actual = dss.bus.zsc_refresh
        assert actual == expected

    def test_coord_defined(self, dss):
        expected = 1
        actual = dss.bus.coord_defined
        assert actual == expected

    def test_get_unique_node_number(self, dss):
        expected = 4
        actual = dss.bus.unique_node_number
        assert actual == expected

    def test_get_unique_node_number_specific_value(self, dss):
        expected = 3
        actual = dss.bus.unique_node_number = 3
        assert actual == expected

    def test_total_customers(self, dss):
        dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        dss.text("Relcalc restore=n")
        expected = 3
        actual = dss.bus.total_customers
        assert actual == expected

    def test_section_id(self, dss):
        # TODO returns one for every bus
        dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        dss.text("Relcalc restore=n")
        expected = 1
        actual = dss.bus.section_id
        assert actual == expected

    def test_kv_base(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = round(2.4017771198288433, 4)
            actual = round(dss.bus.kv_base, 4)
            assert actual == expected

    def test_read_x(self, dss):
        expected = 250
        actual = dss.bus.x
        assert actual == expected

        expected = 35.921882
        dss.bus.x = expected
        actual = dss.bus.x
        assert actual == expected

    def test_read_y(self, dss):
        expected = 100
        actual = dss.bus.y
        assert actual == expected

        expected = -84.141987
        dss.bus.y = expected
        actual = dss.bus.y
        assert actual == expected

    def test_distance(self, dss):
        expected = 1.2202
        actual = dss.bus.distance
        assert actual == expected

    def test_lambda(self, dss):
        dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        dss.text("Relcalc restore=n")
        expected = 10
        actual = dss.bus.bus_lambda
        assert actual == expected

    def test_interruptions_num(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            dss.text("Relcalc restore=n")
            expected = 164.00002
            actual = dss.bus.interruptions_num
            assert actual == expected

    def test_interruptions_avg_duration(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            dss.text("Relcalc restore=n")
            expected = 3.0000000000000004
            actual = dss.bus.interruptions_avg_duration
            assert actual == expected

    def test_interruptions_total_customers(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            dss.text("Relcalc restore=n")
            expected = 1968.0002400000003
            actual = dss.bus.interruptions_total_customers
            assert actual == expected

    def test_outage_customer_accum_duration(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            dss.text("Relcalc restore=n")
            expected = 1968.0002400000003
            actual = dss.bus.outage_customer_accum_duration
            assert actual == expected

    def test_line_total_miles(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            dss.text("Relcalc restore=n")
            expected = 0.0946969696969697
            actual = dss.bus.line_total_miles
            # assert actual == expected # TODO there is something strange here

    def test_read_latitude(self, dss):
        expected = 0
        actual = dss.bus.latitude
        assert actual == expected

        expected = 35.921882
        dss.bus.latitude = expected
        actual = dss.bus.latitude
        assert actual == expected

    def test_read_longitude(self, dss):
        expected = 0
        actual = dss.bus.longitude
        assert actual == expected

        expected = -84.141987
        dss.bus.longitude = expected
        actual = dss.bus.longitude
        assert actual == expected

    def test_name(self, dss):
        expected = "692"
        actual = dss.bus.name
        assert actual == expected

    def test_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [2350.0787629180386, -221.07964093085243, -1338.4031344410694, -2109.8005600677147,
                        -1015.4071496666263, 2083.115713199055]
            actual = dss.bus.voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_seq_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [82.59752865884842, 2391.5781244654136, 42.2125249966211]
            actual = dss.bus.seq_voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_nodes(self, dss):
        expected = [1, 2, 3]
        actual = dss.bus.nodes
        assert actual == expected

    def test_voc(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [-1015.4300936882696, 2083.1333886098464, 2350.084451419252, -221.06489668658924,
                        -1338.4058896357694, -2109.7912876883593]
            actual = dss.bus.voc
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_isc(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [3152.0299371281603, 3631.3058759243054, 2198.661801411833, -5249.483826338779,
                        -5239.348623560714, 1851.7201039001266]
            actual = dss.bus.isc
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_pu_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [0.9784749565294848, -0.09204835832002893, -0.5572553437166756, -0.8784331163159987,
                        -0.4227732628825221, 0.8673226570446733]
            actual = dss.bus.pu_voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_zsc_matrix(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [0.3153473369693269,
                        0.7139992594488932,
                        0.15287779657126946,
                        0.28341681758446546,
                        0.14253991525801543,
                        0.2666530951112187,
                        0.15287779657126965,
                        0.28341681758446513,
                        0.32012739407784085,
                        0.693450533703471,
                        0.15469551759446162,
                        0.34358501078677645,
                        0.1425399152580157,
                        0.26665309511121843,
                        0.15469551759446162,
                        0.34358501078677667,
                        0.2982018705768556,
                        0.7564753107317581]
            actual = dss.bus.zsc_matrix
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_zsc1(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [0.16118779073342565, 0.42342339346722063]
            actual = dss.bus.zsc1
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_zsc0(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [0.6113010201571722, 1.3170783169496814]
            actual = dss.bus.zsc0
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_ysc_matrix(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [0.593677975565496,
                        -1.5149038441061708,
                        -0.16471547512416856,
                        0.4751389024622499,
                        -0.08864564583568464,
                        0.3614274719361526,
                        -0.16471547512416818,
                        0.4751389024622498,
                        0.7387961886565929,
                        -1.6972925513173596,
                        -0.2640838228155729,
                        0.6193548592255308,
                        -0.08864564583568399,
                        0.3614274719361526,
                        -0.26408382281557335,
                        0.6193548592255311,
                        0.5768761487247634,
                        -1.5739302565597302]
            actual = dss.bus.ysc_matrix
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_cplx_sequence_voltages(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [-1.2438403965520592,
                        -82.58816259983746,
                        2386.051972283723,
                        -162.48664862831083,
                        -34.729368969132395,
                        23.995170297295772]
            actual = dss.bus.cplx_sequence_voltages
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_vll(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [3688.481897359108,
                        1888.7209191368622,
                        -322.99598477444306,
                        -4192.91627326677,
                        -3365.485912584665,
                        2304.195354129907]
            actual = dss.bus.vll
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_pu_vll(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [0.8866543022497856,
                        0.4540194517155919,
                        -0.07764326557077958,
                        -1.0079125656891275,
                        -0.8090110366790061,
                        0.5538931139735354]
            actual = dss.bus.pu_vll
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_vmag_angle(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [2360.454701864133,
                        -5.374186258749673,
                        2498.5158301567194,
                        -122.38998951620508,
                        2317.4172594012734,
                        115.98674983388848]
            actual = dss.bus.vmag_angle
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_pu_vmag_angle(self, dss):
        if platform.architecture()[0] == "64bit":
            expected = [0.9827950655272896,
                        -5.374186258749673,
                        1.0402779714775408,
                        -122.38998951620508,
                        0.9648760662548149,
                        115.98674983388848]
            actual = dss.bus.vmag_angle_pu
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_line_list(self, dss):
        expected = ['LINE.692675', 'LINE.671692']
        actual = dss.bus.line_list
        assert actual == expected

    def test_load_list(self, dss):
        expected = ['LOAD.692']
        actual = dss.bus.load_list
        assert actual == expected

    def test_axc_012_matrix(self, dss):
        if platform.architecture()[0] == "64bit":
            dss.text("solve mode=faultstudy")
            expected = [0.6113010201571724,
                        1.3170783169496811,
                        0.013086430482990352,
                        -0.017190929666878185,
                        -0.013622401841550946,
                        -0.03581821513822547,
                        -0.013622401817357882,
                        -0.03581821512135336,
                        0.16118779070817904,
                        0.42342339346689384,
                        0.03459090167453695,
                        0.04240625692503372,
                        0.013086430458797885,
                        -0.01719092968375094,
                        -0.021153549673459254,
                        0.04168470714773151,
                        0.161187790758672,
                        0.4234233934675475]
            actual = dss.bus.axc_012_matrix
            assert [round(value, 20) for value in actual] == [round(value, 20) for value in expected]

    def test_all_pce_active_bus(self, dss):
        expected = ['Load.692']
        actual = dss.bus.all_pce_active_bus
        assert actual == expected

    def test_all_pde_active_bus(self, dss):
        expected = ['Line.692675', 'Line.671692']
        actual = dss.bus.all_pde_active_bus
        assert actual == expected
