# -*- coding: utf-8 -*-
# @Time    : 8/27/2021 11:10 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_pvsystems.py
# @Software: PyCharm

import pytest


class TestPVSystems13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus

        dss.text(r"New XYCurve.MyPvsT npts=4  xarray=[0  25  75  100]  yarray=[1 1 1 1]")
        dss.text(r"New XYCurve.MyEff npts=4  xarray=[.1  .2  .4  1.0]  yarray=[1 1 1 1]")
        dss.text(r"New PVSystem.PV1 phases=3 "
                 r"bus1=680 "
                 r"kV=4.16  "
                 r"kVA=600  "
                 r"irrad=1  "
                 r"Pmpp=500 "
                 r"temperature=25 "
                 r"PF=1 "
                 r"%cutin=0.1 "
                 r"%cutout=0.1  "
                 r"effcurve=Myeff  "
                 r"P-TCurve=MyPvsT")
        dss.solution.solve()
        dss.pvsystems.name = 'PV1'
        dss.circuit.set_active_element("pvsystem.'PV1'")

        return dss

    def test_pvsystems_count(self, dss):
        expected = 1
        actual = dss.pvsystems.count
        assert actual == expected

    def test_pvsystems_first(self, dss):
        expected = 1
        actual = dss.pvsystems.first()
        assert actual == expected

    def test_pvsystems_next(self, dss):
        dss.text(r"New PVSystem.PV2 phases=3 "
                 r"bus1=680 "
                 r"kV=4.16  "
                 r"kVA=600  "
                 r"irrad=1  "
                 r"Pmpp=500 "
                 r"temperature=25 "
                 r"PF=1 "
                 r"%cutin=0.1 "
                 r"%cutout=0.1  "
                 r"effcurve=Myeff  "
                 r"P-TCurve=MyPvsT")
        expected = 2
        dss.pvsystems.first()
        actual = dss.pvsystems.next()
        assert actual == expected

    def test_pvsystems_read_idx(self, dss):
        expected = 1
        actual = dss.pvsystems.idx
        assert actual == expected

    def test_pvsystems_write_idx(self, dss):
        expected = 1
        dss.pvsystems.idx = 1
        actual = dss.pvsystems.idx
        assert actual == expected

    def test_pvsystems_read_irradiance(self, dss):
        expected = 1.0
        actual = dss.pvsystems.irradiance
        assert actual == expected

    def test_pvsystems_write_irradiance(self, dss):
        dss.circuit.set_active_element("pvsystem.'PV1'")
        expected = 0.5
        dss.pvsystems.irradiance = expected
        dss.text("solve")
        actual = dss.pvsystems.irradiance
        assert actual == expected

    def test_pvsystems_kw(self, dss):
        dss.circuit.set_active_element("pvsystem.'PV1'")
        expected = 500.0
        dss.text("solve")
        actual = dss.pvsystems.kw_output
        assert actual == expected

    def test_pvsystems_read_kvar(self, dss):
        expected = 0
        actual = dss.pvsystems.kvar
        assert actual == expected

    def test_pvsystems_write_kvar(self, dss):
        expected = 100
        dss.pvsystems.kvar = expected
        dss.solution.solve()
        actual = dss.pvsystems.kvar
        # assert actual == expected # Todo needs a power flow to get the kvar, it is not the property

    def test_pvsystems_read_pf(self, dss):
        expected = 1
        actual = dss.pvsystems.pf
        assert actual == expected

    def test_pvsystems_write_pf(self, dss):
        expected = -0.97
        dss.pvsystems.pf = expected
        actual = dss.pvsystems.pf
        assert actual == expected

    def test_pvsystems_read_kva_rated(self, dss):
        expected = 600
        actual = dss.pvsystems.kva
        assert actual == expected

    def test_pvsystems_write_kva_rated(self, dss):
        expected = 1000
        dss.pvsystems.kva = expected
        actual = dss.pvsystems.kva
        assert actual == expected

    def test_pvsystems_read_pmpp(self, dss):
        expected = 500.0
        actual = dss.pvsystems.pmpp
        assert actual == expected

    def test_pvsystems_write_pmpp(self, dss):
        expected = 1000.0
        dss.pvsystems.pmpp = expected
        actual = dss.pvsystems.pmpp
        assert actual == expected

    def test_pvsystems_read_name(self, dss):
        expected = "pv1"
        actual = dss.pvsystems.name
        assert actual == expected

    def test_pvsystems_write_name(self, dss):
        dss.text(r"New PVSystem.PV2 phases=3 "
                 r"bus1=680 "
                 r"kV=4.16  "
                 r"kVA=600  "
                 r"irrad=1  "
                 r"Pmpp=500 "
                 r"temperature=25 "
                 r"PF=1 "
                 r"%cutin=0.1 "
                 r"%cutout=0.1  "
                 r"effcurve=Myeff  "
                 r"P-TCurve=MyPvsT")
        expected = "pv2"
        dss.pvsystems.name = expected
        actual = dss.pvsystems.name
        assert actual == expected

    def test_pvsystems_all_names(self, dss):
        dss.text(r"New PVSystem.PV2 phases=3 "
                 r"bus1=680 "
                 r"kV=4.16  "
                 r"kVA=600  "
                 r"irrad=1  "
                 r"Pmpp=500 "
                 r"temperature=25 "
                 r"PF=1 "
                 r"%cutin=0.1 "
                 r"%cutout=0.1  "
                 r"effcurve=Myeff  "
                 r"P-TCurve=MyPvsT")
        expected = ["pv1", "pv2"]
        actual = dss.pvsystems.names
        assert actual == expected
