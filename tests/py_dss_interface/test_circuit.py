# -*- coding: utf-8 -*-
# @Time    : 6/4/2021 7:51 AM
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com
# @File    : test_circuit.py
# @Software: PyCharm

import pytest


class TestCircuit13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    def test_circuit_num_ckt_elements(self):
        expected = 39
        actual = self.dss.circuit_num_ckt_elements()
        assert actual == expected

    def test_circuit_num_buses(self):
        expected = 16
        actual = self.dss.circuit_num_buses()
        assert actual == expected

    def test_circuit_num_nodes(self):
        expected = 41
        actual = self.dss.circuit_num_nodes()
        assert actual == expected

    def test_circuit_first_pc_element(self):
        expected = 1
        actual = self.dss.circuit_first_pc_element()
        assert actual == expected

        expected = 'Load.671'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_circuit_next_pc_element(self):
        expected = 3
        self.dss.circuit_first_pc_element()
        self.dss.circuit_next_pc_element()
        actual = self.dss.circuit_next_pc_element()
        assert actual == expected

        expected = "Load.634b"
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_circuit_first_pd_element(self):
        expected = 1
        actual = self.dss.circuit_first_pd_element()
        assert actual == expected

        expected = 'Transformer.sub'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_circuit_next_pd_element(self):
        expected = 3
        self.dss.circuit_first_pd_element()
        self.dss.circuit_next_pd_element()
        actual = self.dss.circuit_next_pd_element()
        assert actual == expected

        expected = "Transformer.reg2"
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_circuit_sample(self):
        expected = 0
        actual = self.dss.circuit_sample()
        assert actual == expected

    def test_circuit_set_active_bus_i(self):
        expected = 0
        actual = self.dss.circuit_set_active_bus_i(1)
        assert actual == expected

        expected = '650'
        actual = self.dss.bus_name()
        assert actual == expected

    def circuit_first_element(self):
        expected = 0
        actual = self.dss.circuit_first_element()
        assert actual == expected

        expected = 'Line.650632'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def circuit_next_element(self):
        expected = 0
        self.dss.circuit_first_element()
        actual = self.dss.circuit_next_element()
        assert actual == expected

        expected = 'Line.632670'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_circuit_update_storage_t(self):
        expected = 0
        actual = self.dss.circuit_update_storage_t()
        assert actual == expected

    def test_circuit_parent_pd_element(self):
        expected = 1
        self.dss.circuit_first_element()
        self.dss.circuit_next_element()
        actual = self.dss.circuit_parent_pd_element()
        assert actual == expected

        expected = 'Line.650632'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_circuit_end_of_time_step_update(self):
        expected = 0
        actual = self.dss.circuit_end_of_time_step_update()
        assert actual == expected

    def test_circuit_name(self):
        expected = 'ieee13nodeckt'
        actual = self.dss.circuit_name()
        assert actual == expected

    def test_circuit_disable(self):
        pass

    def test_circuit_enable(self):
        pass

    def test_circuit_set_active_element(self):
        expected = '26'
        actual = self.dss.circuit_set_active_element("Line.650632")
        assert actual == expected

        expected = 'Line.650632'
        actual = self.dss.cktelement_name()
        assert actual == expected

    def test_active_class_get_name(self):
        expected = '650632'
        self.dss.circuit_set_active_element("Line.650632")
        actual = self.dss.active_class_get_name()
        assert actual == expected

    def test_circuit_set_active_bus(self):
        expected = '8'
        actual = self.dss.circuit_set_active_bus("692")
        assert actual == expected

        expected = '692'
        actual = self.dss.bus_name()
        assert actual == expected

    def test_circuit_set_active_class(self):
        # Not sure how to use it
        expected = '23'
        actual = self.dss.circuit_set_active_class("Capacitor")
        assert actual == expected

