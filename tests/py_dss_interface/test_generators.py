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
        dss.name_write("G1")

        return dss

    @staticmethod
    def include_generator(dss):
        dss.text(
            'New Generator.G2 Bus1=645.1 phases=1  kV=2.4 kW=100 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400'
        )

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_generators_first(self, dss):
        expected = 1
        actual = dss.first()
        assert actual == expected

    def test_generators_next(self, dss):
        expected = 0
        actual = dss.next()
        assert actual == expected

    def test_generators_count(self, dss):
        expected = 1
        actual = dss.count()
        assert actual == expected

    def test_generators_read_forced_on(self, dss):
        expected = 0
        actual = dss.forced_on_read()
        assert actual == expected

    def test_generators_write_forced_on(self, dss):
        expected = 1
        dss.forced_on_write(expected)
        actual = dss.forced_on_read()
        assert actual == expected

    def test_generators_read_phases(self, dss):
        expected = 1
        actual = dss.phases_read()
        assert actual == expected

    def test_generators_write_phases(self, dss):
        expected = 3
        dss.phases_write(expected)
        actual = dss.phases_read()
        assert actual == expected

    def test_generators_read_idx(self, dss):
        expected = 1
        actual = dss.idx_read()
        assert actual == expected

    def test_generators_write_idx(self, dss):
        self.include_generator(dss)
        expected = 2
        dss.idx_write(expected)
        actual = dss.idx_read()
        assert actual == expected

    def test_generators_read_model(self, dss):
        expected = 3
        actual = dss.model_read()
        assert actual == expected

    def test_generators_write_model(self, dss):
        expected = 2
        dss.model_write(expected)
        actual = dss.model_read()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_generators_read_name(self, dss):
        expected = "G1"
        actual = dss.name_read()
        assert expected.lower() == actual.lower()

    def test_generators_write_name(self, dss):
        self.include_generator(dss)
        expected = "G2"
        dss.name_write(expected)
        actual = dss.name_read()
        assert expected.lower() == actual.lower()

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_generators_read_kv(self, dss):
        expected = 2.4
        actual = dss.kv_read()
        assert actual == expected

    def test_generators_write_kv(self, dss):
        expected = 4.8
        dss.kv_write(expected)
        actual = dss.kv_read()
        assert actual == expected

    def test_generators_read_kw(self, dss):
        expected = 100
        actual = dss.kw_read()
        assert actual == expected

    def test_generators_write_kw(self, dss):
        expected = 400.0
        dss.kw_write(expected)
        actual = dss.kw_read()
        assert actual == expected

    def test_generators_read_kvar(self, dss):
        expected = -50
        actual = dss.kvar_read()
        assert actual == expected

    def test_generators_write_kvar(self, dss):
        expected = -10
        dss.kvar_write(expected)
        actual = dss.kvar_read()
        assert actual == expected

    def test_generators_read_pf(self, dss):
        expected = -0.894427
        actual = dss.pf_read()
        assert expected == pytest.approx(actual)

    def test_generators_write_pf(self, dss):
        expected = -0.90
        dss.pf_write(expected)
        actual = dss.pf_read()
        assert expected == pytest.approx(actual)

    def test_generators_read_kva_rated(self, dss):
        expected = 120
        actual = dss.kva_rated_read()
        assert expected == pytest.approx(actual)

    def test_generators_write_kva_rated(self, dss):
        expected = 123
        dss.kva_rated_write(expected)
        actual = dss.kva_rated_read()
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
        actual = dss.names()
        assert actual == expected

    def test_generators_register_names(self, dss):
        expected = ['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Hours', '$']
        actual = dss.register_names()
        assert actual == expected

    def test_generators_register_values(self, dss):
        expected = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = dss.register_values()
        assert actual == expected
