# -*- coding: utf-8 -*-
# @Time    : 8/19/2021 10:50 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_generators.py
# @Software: PyCharm
# now

import pytest


class TestGenerators13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text(
            "New Generator.G1  Bus1=645.2 phases=1  kV=2.4 kW=100 kvar=-50 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400")
        dss.generators_write_name("G1")

        return dss

    def include_generator(self, dss):
        dss.text(
            'New Generator.G2 Bus1=645.1 phases=1  kV=2.4 kW=100 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400'
        )

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_generators_first(self, dss):
        expected = 1
        actual = dss.generators_first()
        assert actual == expected

    def test_generators_next(self, dss):
        expected = 0
        actual = dss.generators_next()
        assert actual == expected

    def test_generators_count(self, dss):
        expected = 1
        actual = dss.generators_count()
        assert actual == expected

    def test_generators_read_forced_on(self, dss):
        expected = 0
        actual = dss.generators_read_forced_on()
        assert actual == expected

    def test_generators_write_forced_on(self, dss):
        expected = 1
        dss.generators_write_forced_on(expected)
        actual = dss.generators_read_forced_on()
        assert actual == expected

    def test_generators_read_phases(self, dss):
        expected = 1
        actual = dss.generators_read_phases()
        assert actual == expected

    def test_generators_write_phases(self, dss):
        expected = 3
        dss.generators_write_phases(expected)
        actual = dss.generators_read_phases()
        assert actual == expected

    def test_generators_read_idx(self, dss):
        expected = 1
        actual = dss.generators_read_idx()
        assert actual == expected

    def test_generators_write_idx(self, dss):
        self.include_generator(dss)
        expected = 2
        dss.generators_write_idx(expected)
        actual = dss.generators_read_idx()
        assert actual == expected

    def test_generators_read_model(self, dss):
        expected = 3
        actual = dss.generators_read_model()
        assert actual == expected

    def test_generators_write_model(self, dss):
        expected = 2
        dss.generators_write_model(expected)
        actual = dss.generators_read_model()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_generators_read_name(self, dss):
        expected = "G1"
        actual = dss.generators_read_name()
        assert expected.lower() == actual.lower()

    def test_generators_write_name(self, dss):
        self.include_generator(dss)
        expected = "G2"
        dss.generators_write_name(expected)
        actual = dss.generators_read_name()
        assert expected.lower() == actual.lower()

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_generators_read_kv(self, dss):
        expected = 2.4
        actual = dss.generators_read_kv()
        assert actual == expected

    def test_generators_write_kv(self, dss):
        expected = 4.8
        dss.generators_write_kv(expected)
        actual = dss.generators_read_kv()
        assert actual == expected

    def test_generators_read_kw(self, dss):
        expected = 100
        actual = dss.generators_read_kw()
        assert actual == expected

    def test_generators_write_kw(self, dss):
        expected = 400.0
        dss.generators_write_kw(expected)
        actual = dss.generators_read_kw()
        assert actual == expected

    def test_generators_read_kvar(self, dss):
        expected = -50
        actual = dss.generators_read_kvar()
        assert actual == expected

    def test_generators_write_kvar(self, dss):
        expected = -10
        dss.generators_write_kvar(expected)
        actual = dss.generators_read_kvar()
        assert actual == expected

    def test_generators_read_pf(self, dss):
        expected = -0.894427
        actual = dss.generators_read_pf()
        assert expected == pytest.approx(actual)

    def test_generators_write_pf(self, dss):
        expected = -0.90
        dss.generators_write_pf(expected)
        actual = dss.generators_read_pf()
        assert expected == pytest.approx(actual)

    def test_generators_read_kva_rated(self, dss):
        expected = 120
        actual = dss.generators_read_kva_rated()
        assert expected == pytest.approx(actual)

    def test_generators_write_kva_rated(self, dss):
        expected = 123
        dss.generators_write_kva_rated(expected)
        actual = dss.generators_read_kva_rated()
        assert expected == pytest.approx(actual)

    def test_generators_read_vmax_pu(self, dss):
        expected = 1.1
        actual = dss.generators_read_vmax_pu()
        assert expected == pytest.approx(actual)

    def test_generators_write_vmax_pu(self, dss):
        expected = 1.2
        dss.generators_write_vmax_pu(expected)
        actual = dss.generators_read_vmax_pu()
        assert expected == pytest.approx(actual)

    def test_generators_read_vmin_pu(self, dss):
        expected = 0.9
        actual = dss.generators_read_vmin_pu()
        assert expected == pytest.approx(actual)

    def test_generators_write_vmin_pu(self, dss):
        expected = 0.85
        dss.generators_write_vmin_pu(expected)
        actual = dss.generators_read_vmin_pu()
        assert expected == pytest.approx(actual)

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_generators_all_names(self, dss):
        self.include_generator(dss)
        expected = ["g1", "g2"]
        actual = dss.generators_all_names()
        assert actual == expected

    def test_generators_register_names(self, dss):
        expected = ['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Hours', '$']
        actual = dss.generators_register_names()
        assert actual == expected

    def test_generators_register_values(self, dss):
        expected = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = dss.generators_register_values()
        assert actual == expected
