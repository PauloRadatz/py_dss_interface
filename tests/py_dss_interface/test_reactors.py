# -*- coding: utf-8 -*-
# @Time    : 10/7/2024 9:33 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_reactors.py
# @Software: PyCharm

import pytest


class TestBus13Reactors:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("new reactor.r bus1=675 phases=3 kv=4.16 kvar=600")
        dss.reactors.name = "r"
        return dss

    def test_first(self, dss):
        expected = 1
        actual = dss.reactors.first()
        assert actual == expected

        expected = 'r'
        actual = dss.reactors.name
        assert actual == expected

    def test_next(self, dss):
        expected = 1
        actual = dss.reactors.first()
        assert actual == expected

        expected = 1
        actual = dss.reactors.next()
        assert actual == expected

    def test_count(self, dss):
        expected = 1
        actual = dss.reactors.count
        assert actual == expected

    def test_read_parallel(self, dss):
        expected = 0
        actual = dss.reactors.parallel
        assert actual == expected

    def test_write_parallel(self, dss):
        dss.reactors.parallel = 1
        expected = 1
        actual = dss.reactors.parallel
        assert actual == expected

    def test_read_kv(self, dss):
        expected = 4.16
        actual = dss.reactors.kv
        assert actual == expected

    def test_write_kv(self, dss):
        dss.reactors.kv = 4.161
        expected = 4.161
        actual = dss.reactors.kv
        assert actual == expected

    def test_read_kvar(self, dss):
        expected = 600
        actual = dss.reactors.kvar
        assert actual == expected

    def test_write_kvar(self, dss):
        dss.reactors.kvar = 610
        expected = 610
        actual = dss.reactors.kvar
        assert actual == expected

    def test_read_imh(self, dss):
        expected = 76.50754953253067
        actual = dss.reactors.imh
        assert actual == expected

    def test_write_imh(self, dss):
        dss.reactors.imh = 80
        expected = 80
        actual = dss.reactors.imh
        assert actual == expected

    def test_read_r(self, dss):
        expected = 0
        actual = dss.reactors.r
        assert actual == expected

    def test_write_r(self, dss):
        dss.reactors.r = 10
        expected = 10
        actual = dss.reactors.r
        assert actual == expected

    def test_read_rp(self, dss):
        expected = 0
        actual = dss.reactors.rp
        assert actual == expected

    def test_write_rp(self, dss):
        dss.reactors.rp = 10
        expected = 10
        actual = dss.reactors.rp
        assert actual == expected

    def test_read_x(self, dss):
        expected = 28.84266666666667
        actual = dss.reactors.x
        assert actual == expected

    def test_write_x(self, dss):
        dss.reactors.x = 10
        expected = 10
        actual = dss.reactors.x
        assert actual == expected

    def test_name(self, dss):
        expected = "r"
        actual = dss.reactors.name
        assert actual == expected

    def test_read_l_curve(self, dss):
        expected = ""
        actual = dss.reactors.l_curve
        assert actual == expected

    def test_write_l_curve(self, dss):
        dss.text("new xycurve.curve npts=1 x=[1] y=[1]")
        dss.reactors.l_curve = "curve"
        expected = "curve"
        actual = dss.reactors.l_curve
        assert actual == expected

    def test_read_r_curve(self, dss):
        expected = ""
        actual = dss.reactors.r_curve
        assert actual == expected

    def test_write_r_curve(self, dss):
        dss.text("new xycurve.curve npts=1 x=[1] y=[1]")
        dss.reactors.r_curve = "curve"
        expected = "curve"
        actual = dss.reactors.r_curve
        assert actual == expected

    def test_names(self, dss):
        expected = ["r"]
        actual = dss.reactors.names
        assert actual == expected

    def test_read_rmatrix(self, dss):
        expected = [0]
        actual = dss.reactors.rmatrix
        assert actual == expected

    def test_write_rmatrix(self, dss):
        expected = [0.086, 0.029, 0.02, 0.029, 0.088, 0.029, 0.02, 0.029, 0.08]
        dss.reactors.rmatrix = expected
        actual = dss.reactors.rmatrix
        assert actual == expected

    def test_read_xmatrix(self, dss):
        expected = [0]
        actual = dss.reactors.xmatrix
        assert actual == expected

    def test_write_xmatrix(self, dss):
        expected = [0.086, 0.029, 0.02, 0.029, 0.088, 0.029, 0.02, 0.029, 0.08]
        dss.reactors.xmatrix = expected
        actual = dss.reactors.xmatrix
        assert actual == expected

    def test_read_z(self, dss):
        expected = [0, 0]
        actual = dss.reactors.z
        assert actual == expected

    def test_write_z(self, dss):
        expected = [0.086, 0.029]
        dss.reactors.z = expected
        actual = dss.reactors.z
        assert actual == expected

    def test_read_z0(self, dss):
        expected = [0, 0]
        actual = dss.reactors.z0
        assert actual == expected

    def test_write_z0(self, dss):
        expected = [0.086, 0.029]
        dss.reactors.z0 = expected
        actual = dss.reactors.z0
        assert actual == expected

    def test_read_z1(self, dss):
        expected = [0, 0]
        actual = dss.reactors.z1
        assert actual == expected

    def test_write_z1(self, dss):
        expected = [0.086, 0.029]
        dss.reactors.z1 = expected
        actual = dss.reactors.z1
        assert actual == expected

    def test_read_z2(self, dss):
        expected = [0, 0]
        actual = dss.reactors.z2
        assert actual == expected

    def test_write_z2(self, dss):
        expected = [0.086, 0.029]
        dss.reactors.z2 = expected
        actual = dss.reactors.z2
        assert actual == expected
