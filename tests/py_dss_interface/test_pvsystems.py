# -*- coding: utf-8 -*-
# @Time    : 8/27/2021 11:10 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_pvsystems.py
# @Software: PyCharm

import pytest
import platform


class TestPVSystems13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

        self.dss.text(r"New XYCurve.MyPvsT npts=4  xarray=[0  25  75  100]  yarray=[1 1 1 1]")
        self.dss.text(r"New XYCurve.MyEff npts=4  xarray=[.1  .2  .4  1.0]  yarray=[1 1 1 1]")
        self.dss.text(r"New PVSystem.PV1 phases=3 "
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
        self.dss.solution_solve()
        self.dss.pvsystems_write_name('PV1')

    def test_pvsystems_count(self):
        expected = 1
        actual = self.dss.pvsystems_count()
        assert actual == expected

    def test_pvsystems_first(self):
        expected = 1
        actual = self.dss.pvsystems_first()
        assert actual == expected

    def test_pvsystems_next(self):
        self.dss.text(r"New PVSystem.PV2 phases=3 "
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
        self.dss.pvsystems_first()
        actual = self.dss.pvsystems_next()
        assert actual == expected

    def test_pvsystems_read_idx(self):
        expected = 1
        actual = self.dss.pvsystems_read_idx()
        assert actual == expected

    def test_pvsystems_write_idx(self):
        expected = 1
        self.dss.pvsystems_write_idx(1)
        actual = self.dss.pvsystems_read_idx()
        assert actual == expected

    def test_pvsystems_read_irradiance(self):
        expected = 1.0
        actual = self.dss.pvsystems_read_irradiance()
        assert actual == expected

    def test_pvsystems_write_irradiance(self):
        expected = 0.5
        self.dss.pvsystems_write_irradiance(expected)
        actual = self.dss.pvsystems_read_irradiance()
        assert actual == expected

    def test_pvsystems_kw(self):
        expected = 500.0
        actual = self.dss.pvsystems_kw()
        assert actual == expected

    def test_pvsystems_read_kvar(self):
        expected = 0
        actual = self.dss.pvsystems_read_kvar()
        assert actual == expected

    def test_pvsystems_write_kvar(self):
        expected = 100
        self.dss.pvsystems_write_kvar(expected)
        self.dss.solution_solve()
        actual = self.dss.pvsystems_read_kvar()
        # assert actual == expected # Todo needs a power flow to get the kvar, it is not the property

    def test_pvsystems_read_pf(self):
        expected = 1
        actual = self.dss.pvsystems_read_pf()
        assert actual == expected

    def test_pvsystems_write_pf(self):
        expected = -0.97
        self.dss.pvsystems_write_pf(expected)
        actual = self.dss.pvsystems_read_pf()
        assert actual == expected

    def test_pvsystems_read_kva_rated(self):
        expected = 600
        actual = self.dss.pvsystems_read_kva_rated()
        assert actual == expected

    def test_pvsystems_write_kva_rated(self):
        expected = 1000
        self.dss.pvsystems_write_kva_rated(expected)
        actual = self.dss.pvsystems_read_kva_rated()
        assert actual == expected

    def test_pvsystems_read_pmpp(self):
        expected = 500.0
        actual = self.dss.pvsystems_read_pmpp()
        assert actual == expected

    def test_pvsystems_write_pmpp(self):
        expected = 1000.0
        self.dss.pvsystems_write_pmpp(expected)
        actual = self.dss.pvsystems_read_pmpp()
        assert actual == expected

    def test_pvsystems_read_name(self):
        expected = "pv1"
        actual = self.dss.pvsystems_read_name()
        assert actual == expected

    def test_pvsystems_write_name(self):
        self.dss.text(r"New PVSystem.PV2 phases=3 "
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
        self.dss.pvsystems_write_name(expected)
        actual = self.dss.pvsystems_read_name()
        assert actual == expected

    def test_pvsystems_all_names(self):
        self.dss.text(r"New PVSystem.PV2 phases=3 "
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
        actual = self.dss.pvsystems_all_names()
        assert actual == expected
