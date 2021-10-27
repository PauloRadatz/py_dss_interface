# -*- coding: utf-8 -*-
# @Time    : 8/19/2021 10:50 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_generators.py
# @Software: PyCharm
# now

import pytest


class TestGenerators13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text(
            "New Generator.G1  Bus1=645.2 phases=1  kV=2.4 kW=100 kvar=-50 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400")
        self.dss.generators_write_name("G1")

    def include_generator(self):
        self.dss.text(
            'New Generator.G2 Bus1=645.1 phases=1  kV=2.4 kW=100 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400'
        )

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_generators_first(self):
        expected = 1
        actual = self.dss.generators_first()
        assert expected == actual

    def test_generators_next(self):
        expected = 0
        actual = self.dss.generators_next()
        assert expected == actual

    def test_generators_count(self):
        expected = 1
        actual = self.dss.generators_count()
        assert expected == actual

    def test_generators_read_forced_on(self):
        expected = 0
        actual = self.dss.generators_read_forced_on()
        assert expected == actual

    def test_generators_write_forced_on(self):
        expected = 1
        self.dss.generators_write_forced_on(expected)
        actual = self.dss.generators_read_forced_on()
        assert expected == actual

    def test_generators_read_phases(self):
        expected = 1
        actual = self.dss.generators_read_phases()
        assert expected == actual

    def test_generators_write_phases(self):
        expected = 3
        self.dss.generators_write_phases(expected)
        actual = self.dss.generators_read_phases()
        assert expected == actual

    def test_generators_read_idx(self):
        expected = 1
        actual = self.dss.generators_read_idx()
        assert expected == actual

    def test_generators_write_idx(self):
        self.include_generator()
        expected = 2
        self.dss.generators_write_idx(expected)
        actual = self.dss.generators_read_idx()
        assert expected == actual

    def test_generators_read_model(self):
        expected = 3
        actual = self.dss.generators_read_model()
        assert expected == actual

    def test_generators_write_model(self):
        expected = 2
        self.dss.generators_write_model(expected)
        actual = self.dss.generators_read_model()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_generators_read_name(self):
        expected = "G1"
        actual = self.dss.generators_read_name()
        assert expected.lower() == actual.lower()

    def test_generators_write_name(self):
        self.include_generator()
        expected = "G2"
        self.dss.generators_write_name(expected)
        actual = self.dss.generators_read_name()
        assert expected.lower() == actual.lower()

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_generators_read_kv(self):
        expected = 2.4
        actual = self.dss.generators_read_kv()
        assert expected == actual

    def test_generators_write_kv(self):
        expected = 4.8
        self.dss.generators_write_kv(expected)
        actual = self.dss.generators_read_kv()
        assert expected == actual

    def test_generators_read_kw(self):
        expected = 100
        actual = self.dss.generators_read_kw()
        assert expected == actual

    # TODO: not writing kW
    def test_generators_write_kw(self):
        expected = 200.0
        self.dss.generators_write_kw(expected)
        actual = self.dss.generators_read_kw()
        assert expected == actual

    def test_generators_read_kvar(self):
        expected = -50
        actual = self.dss.generators_read_kvar()
        assert expected == actual

    def test_generators_write_kvar(self):
        expected = -10
        self.dss.generators_write_kvar(expected)
        actual = self.dss.generators_read_kvar()
        assert expected == actual

    def test_generators_read_pf(self):
        expected = -0.894427
        actual = self.dss.generators_read_pf()
        assert expected == pytest.approx(actual)

    def test_generators_write_pf(self):
        expected = -0.90
        self.dss.generators_write_pf(expected)
        actual = self.dss.generators_read_pf()
        assert expected == pytest.approx(actual)

    def test_generators_read_kva_rated(self):
        expected = 120
        actual = self.dss.generators_read_kva_rated()
        assert expected == pytest.approx(actual)

    def test_generators_write_kva_rated(self):
        expected = 123
        self.dss.generators_write_kva_rated(expected)
        actual = self.dss.generators_read_kva_rated()
        assert expected == pytest.approx(actual)

    def test_generators_read_vmax_pu(self):
        expected = 1.1
        actual = self.dss.generators_read_vmax_pu()
        assert expected == pytest.approx(actual)

    def test_generators_write_vmax_pu(self):
        expected = 1.2
        self.dss.generators_write_vmax_pu(expected)
        actual = self.dss.generators_read_vmax_pu()
        assert expected == pytest.approx(actual)

    def test_generators_read_vmin_pu(self):
        expected = 0.9
        actual = self.dss.generators_read_vmin_pu()
        assert expected == pytest.approx(actual)

    def test_generators_write_vmin_pu(self):
        expected = 0.85
        self.dss.generators_write_vmin_pu(expected)
        actual = self.dss.generators_read_vmin_pu()
        assert expected == pytest.approx(actual)

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_generators_all_names(self):
        self.include_generator()
        expected = ["g1", "g2"]
        actual = self.dss.generators_all_names()
        assert expected == actual

    def test_generators_register_names(self):
        expected = ['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Hours', '$']
        actual = self.dss.generators_register_names()
        assert expected == actual

    def test_generators_register_values(self):
        expected = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = self.dss.generators_register_values()
        assert expected == actual
