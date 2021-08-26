# -*- coding: utf-8 -*-
# @Time    : 6/22/2021 5:48 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_bus.py
# @Software: PyCharm

import pytest
import platform


class TestBus13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.circuit_set_active_bus('692')

    def test_bus_num_nodes(self):
        expected = 3
        actual = self.dss.bus_num_nodes()
        assert actual == expected

    def test_bus_zsc_refresh(self):
        expected = 1
        actual = self.dss.bus_zsc_refresh()
        assert actual == expected

    def test_bus_coord_defined(self):
        expected = 1
        actual = self.dss.bus_coord_defined()
        assert actual == expected

    def test_bus_get_unique_node_number(self):
        #TODO
        expected = 0
        actual = self.dss.bus_get_unique_node_number()
        assert actual == expected

    def test_bus_total_customers(self):
        self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        self.dss.text("Relcalc restore=n")
        expected = 3
        actual = self.dss.bus_total_customers()
        assert actual == expected

    def test_bus_section_id(self):
        # TODO
        expected = 0
        actual = self.dss.bus_section_id()
        assert actual == expected

    def test_bus_kv_base(self):
        if platform.architecture()[0] == "64bit":
            expected = round(2.4017771198288433, 4)
            actual = round(self.dss.bus_kv_base(), 4)
            assert actual == expected

    def test_bus_read_x(self):
        expected = 250
        actual = self.dss.bus_read_x()
        assert actual == expected

        expected = 35.921882
        self.dss.bus_write_x(expected)
        actual = self.dss.bus_read_x()
        assert actual == expected

    def test_bus_read_y(self):
        expected = 100
        actual = self.dss.bus_read_y()
        assert actual == expected

        expected = -84.141987
        self.dss.bus_write_y(expected)
        actual = self.dss.bus_read_y()
        assert actual == expected

    def test_bus_distance(self):
        expected = 1.2202
        actual = self.dss.bus_distance()
        assert actual == expected

    def test_bus_lambda(self):
        self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        self.dss.text("Relcalc restore=n")
        expected = 10
        actual = self.dss.bus_lambda()
        assert actual == expected

    def test_bus_interruptions_num(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            self.dss.text("Relcalc restore=n")
            expected = 164.00002
            actual = self.dss.bus_interruptions_num()
            assert actual == expected

    def test_bus_interruptions_avg_duration(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            self.dss.text("Relcalc restore=n")
            expected = 3.0000000000000004
            actual = self.dss.bus_interruptions_avg_duration()
            assert actual == expected

    def test_bus_interruptions_total_customers(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            self.dss.text("Relcalc restore=n")
            expected = 1968.0002400000003
            actual = self.dss.bus_interruptions_total_customers()
            assert actual == expected

    def test_bus_outage_customer_accum_duration(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            self.dss.text("Relcalc restore=n")
            expected = 1968.0002400000003
            actual = self.dss.bus_outage_customer_accum_duration()
            assert actual == expected

    def test_bus_line_total_miles(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
            self.dss.text("Relcalc restore=n")
            expected = 0.09469955881439135
            actual = self.dss.bus_line_total_miles()
            assert actual == expected

    def test_bus_read_latitude(self):
        expected = 0
        actual = self.dss.bus_read_latitude()
        assert actual == expected

        expected = 35.921882
        self.dss.bus_write_latitude(expected)
        actual = self.dss.bus_read_latitude()
        assert actual == expected

    def test_bus_read_longitude(self):
        expected = 0
        actual = self.dss.bus_read_longitude()
        assert actual == expected

        expected = -84.141987
        self.dss.bus_write_longitude(expected)
        actual = self.dss.bus_read_longitude()
        assert actual == expected

    def test_bus_name(self):
        expected = "692"
        actual = self.dss.bus_name()
        assert actual == expected

    def test_bus_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [2350.0726913632457, -221.08573234457813, -1338.4057922959519, -2109.7992630653525, -1015.4001096022041, 2083.111507606075]
            actual = self.dss.bus_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]

    def test_bus_seq_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [82.60053680171639, 2391.5740870438235, 42.21419682837919]
            actual = self.dss.bus_seq_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_nodes(self):
        expected = [1, 2, 3]
        actual = self.dss.bus_nodes()
        assert actual == expected

    def test_bus_voc(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [-1015.4230522077399, 2083.1291838874104, 2350.078382909445, -221.07098582892812, -1338.4085461681827, -2109.789989443137]
            actual = self.dss.bus_voc()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_isc(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [3151.9436977113414, 3631.2030732888024, 2198.5994484421412, -5249.339041660827, -5239.204244446338, 1851.6707151591527]
            actual = self.dss.bus_isc()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_pu_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [0.9784724285868448, -0.09205089453110173, -0.5572564503367948, -0.8784325762982129, -0.42277033169279427, 0.867320906010847]
            actual = self.dss.bus_pu_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_zsc_matrix(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [0.3153571853629021, 0.7140167870270339, 0.15288269190543224, 0.2834235319289957,
                        0.14254440981577635, 0.266659685394725, 0.15288269190543266, 0.2834235319289955,
                        0.32013735294808515, 0.6934673112029895, 0.15470056782589817, 0.34359339152399104,
                        0.14254440981577626, 0.26665968539472434, 0.15470056782589778, 0.343593391523991,
                        0.29821101707626313, 0.756494867585397]
            actual = self.dss.bus_zsc_matrix()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_zsc1(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [0.16119262861338127, 0.4234341189892362]
            actual = self.dss.bus_zsc1()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_zsc0(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [0.6113202981604879, 1.317110727836948]
            actual = self.dss.bus_zsc0()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_ysc_matrix(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [0.5936658821397857, -1.5148637612878215, -0.16471187373739576, 0.475126563866544,
                        -0.08864392285714026, 0.3614180297100929, -0.1647118737373953, 0.4751265638665447,
                        0.7387806072983646, -1.6972477662392382, -0.2640772495349568, 0.619338363995164,
                        -0.08864392285713961, 0.36141802971009174, -0.2640772495349573, 0.6193383639951636,
                        0.5768624979080198, -1.5738877843162216]
            actual = self.dss.bus_ysc_matrix()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_cplx_sequence_voltages(self):
        if platform.architecture()[0] == "64bit":
            expected = [-1.244403511636449, -82.59116260128519, 2386.0476296014167, -162.49099388261294, -34.730534726534245, 23.99642413931997]
            actual = self.dss.bus_cplx_sequence_voltages()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_vll(self):
        if platform.architecture()[0] == "64bit":
            expected = [3688.4784836591975, 1888.7135307207743, -323.00568269374776, -4192.910770671428, -3365.47280096545, 2304.197239950653]
            actual = self.dss.bus_vll()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_pu_vll(self):
        if platform.architecture()[0] == "64bit":
            expected = [0.8866534816488456, 0.4540176756540323, -0.07764559680138167, -1.0079112429498625, -0.809007884847464, 0.5538935672958301]
            actual = self.dss.bus_pu_vll()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_vmag_angle(self):
        if platform.architecture()[0] == "64bit":
            expected = [2360.4492275280204, -5.3743472704625, 2498.516158699491, -122.39005691604211, 2317.410394319708, 115.98663893302955]
            actual = self.dss.bus_vmag_angle()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_pu_vmag_angle(self):
        if platform.architecture()[0] == "64bit":
            expected = [0.9827927862416442, -5.3743472704625, 1.0402781082690726, -122.39005691604211, 0.9648732079206634, 115.98663893302955]
            actual = self.dss.bus_pu_vmag_angle()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_line_list(self):
        expected = ['LINE.692675', 'LINE.671692']
        actual = self.dss.bus_line_list()
        assert actual == expected

    def test_bus_load_list(self):
        expected = ['LOAD.692']
        actual = self.dss.bus_load_list()
        assert actual == expected

    def test_bus_axc_012_matrix(self):
        if platform.architecture()[0] == "64bit":
            self.dss.text("solve mode=faultstudy")
            expected = [0.6113202981604879, 1.3171107278369476, 0.013087177141174577, -0.0171913688019798,
                        -0.013623188217551707, -0.035819354684213156, -0.013623188193357644, -0.035819354667340125,
                        0.16119262858813385, 0.42343411898890954, 0.03459211105819357, 0.04240719913058216,
                        0.013087177116980847, -0.017191368818853817, -0.021154088203983595, 0.041685643107486084,
                        0.16119262863862857, 0.4234341189895631]
            actual = self.dss.bus_axc_012_matrix()
            assert [round(value, 6) for value in actual] == [round(value, 6) for value in expected]


    def test_bus_all_pce_active_bus(self):
        expected = ['Load.692']
        actual = self.dss.bus_all_pce_active_bus()
        assert actual == expected

    def test_bus_all_pde_active_bus(self):
        expected = ['Line.692675', 'Line.671692']
        actual = self.dss.bus_all_pde_active_bus()
        assert actual == expected
