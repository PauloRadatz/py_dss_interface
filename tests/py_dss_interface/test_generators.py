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
        dss.text("New Generator.G1  Bus1=645.2 phases=1  kV=2.4 kW=100 kvar=-50 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400")
        dss.generators.name = "G1"

        return dss

    @staticmethod
    def include_generator(dss):
        dss.text('New Generator.G2 Bus1=645.1 phases=1  kV=2.4 kW=100 Model=3 Vpu=1 Maxkvar=500 Minkvar=-400')

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_generators_first(self, dss):
        expected = 1
        actual = dss.generators.first()
        assert actual == expected

    def test_generators_next(self, dss):
        expected = 0
        actual = dss.generators.next()
        assert actual == expected

    def test_generators_count(self, dss):
        expected = 1
        actual = dss.generators.count
        assert actual == expected

    def test_generators_read_forced_on(self, dss):
        expected = 0
        actual = dss.generators.forced_on
        assert actual == expected

    def test_generators_write_forced_on(self, dss):
        expected = 1
        dss.generators.forced_on = expected
        actual = dss.generators.forced_on
        assert actual == expected

    def test_generators_read_phases(self, dss):
        expected = 1
        actual = dss.generators.phases
        assert actual == expected

    def test_generators_write_phases(self, dss):
        expected = 3
        dss.generators.phases = expected
        actual = dss.generators.phases
        assert actual == expected

    def test_generators_read_idx(self, dss):
        expected = 1
        actual = dss.generators.idx
        assert actual == expected

    def test_generators_write_idx(self, dss):
        self.include_generator(dss)
        expected = 2
        dss.generators.idx = expected
        actual = dss.generators.idx
        assert actual == expected

    def test_generators_read_model(self, dss):
        expected = 3
        actual = dss.generators.model
        assert actual == expected

    def test_generators_write_model(self, dss):
        expected = 2
        dss.generators.model = expected
        actual = dss.generators.model
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_generators_read_name(self, dss):
        expected = "G1"
        actual = dss.generators.name
        assert expected.lower() == actual.lower()

    def test_generators_write_name(self, dss):
        self.include_generator(dss)
        expected = "G2"
        dss.generators.name = expected
        actual = dss.generators.name
        assert expected.lower() == actual.lower()

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_generators_read_kv(self, dss):
        expected = 2.4
        actual = dss.generators.kv
        assert actual == expected

    def test_generators_write_kv(self, dss):
        expected = 4.8
        dss.generators.kv = expected
        actual = dss.generators.kv
        assert actual == expected

    def test_generators_read_kw(self, dss):
        expected = 100
        actual = dss.generators.kw
        assert actual == expected

    def test_generators_write_kw(self, dss):
        expected = 400.0
        dss.generators.kw = expected
        actual = dss.generators.kw
        assert actual == expected

    def test_generators_read_kvar(self, dss):
        expected = -50
        actual = dss.generators.kvar
        assert actual == expected

    def test_generators_write_kvar(self, dss):
        expected = -10
        dss.generators.kvar = expected
        actual = dss.generators.kvar
        assert actual == expected

    def test_generators_read_pf(self, dss):
        expected = -0.894427
        actual = dss.generators.pf
        assert expected == pytest.approx(actual)

    def test_generators_write_pf(self, dss):
        expected = -0.90
        dss.generators.pf = expected
        actual = dss.generators.pf
        assert expected == pytest.approx(actual)

    def test_generators_read_kva(self, dss):
        expected = 120
        actual = dss.generators.kva
        assert expected == pytest.approx(actual)

    def test_generators_write_kva(self, dss):
        expected = 123
        dss.generators.kva = expected
        actual = dss.generators.kva
        assert expected == pytest.approx(actual)

    def test_generators_read_vmax_pu(self, dss):
        expected = 1.1
        actual = dss.generators.vmax_pu
        assert expected == pytest.approx(actual)

    def test_generators_write_vmax_pu(self, dss):
        expected = 1.2
        dss.generators.vmax_pu = expected
        actual = dss.generators.vmax_pu
        assert expected == pytest.approx(actual)

    def test_generators_read_vmin_pu(self, dss):
        expected = 0.9
        actual = dss.generators.vmin_pu
        assert expected == pytest.approx(actual)

    def test_generators_write_vmin_pu(self, dss):
        expected = 0.85
        dss.generators.vmin_pu = expected
        actual = dss.generators.vmin_pu
        assert expected == pytest.approx(actual)

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_generators_all_names(self, dss):
        self.include_generator(dss)
        expected = ["g1", "g2"]
        actual = dss.generators.names
        assert actual == expected

    def test_generators_register_names(self, dss):
        expected = ['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Hours', '$']
        actual = dss.generators.register_names
        assert actual == expected

    def test_generators_register_values(self, dss):
        expected = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = dss.generators.register_values
        assert actual == expected
