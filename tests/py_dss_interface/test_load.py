# -*- coding: utf-8 -*-
# @Time    : 6/24/2021 10:51 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_load.py
# @Software: PyCharm

import pytest
import platform
import math as m


class TestLoad13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.loads_write_name('671')

    def test_loads_count(self):
        expected = 15
        actual = self.dss.loads_count()
        assert actual == expected

    def test_loads_first(self):
        expected = 1
        actual = self.dss.loads_first()
        assert actual == expected

    def test_loads_read_name(self):
        expected = '671'
        actual = self.dss.loads_read_name()
        assert actual == expected

    def test_loads_read_kv(self):
        expected = 4.16
        actual = self.dss.loads_read_kv()
        assert actual == expected

    def test_loads_read_kw(self):
        expected = 1155
        actual = self.dss.loads_read_kw()
        assert actual == expected

    def test_loads_read_kvar(self):
        expected = 0.8682431421244591
        actual = self.dss.loads_read_pf()
        assert actual == expected

    def test_loads_read_kva(self):
        expected = (self.dss.loads_read_kw()**2 + self.dss.loads_read_kvar()**2)**(1/2)
        actual = self.dss.loads_read_kva()
        assert actual == expected

    def test_loads_read_kwh(self):
        expected = 0
        actual = self.dss.loads_read_kwh()
        assert actual == expected

    def test_loads_read_model(self):
        expected = 1
        actual = self.dss.loads_read_model()
        assert actual == expected

    def test_loads_read_pct_mean(self):
        expected = 50
        actual = self.dss.loads_read_pct_mean()
        assert actual == expected

    def test_loads_read_pct_std_dev(self):
        expected = 10
        actual = self.dss.loads_read_pct_std_dev()
        assert actual == expected

    def test_loads_read_allocation_factor(self):
        expected = 0.5
        actual = self.dss.loads_read_allocation_factor()
        assert actual == expected

    def test_loads_read_c_factor(self):
        expected = 4
        actual = self.dss.loads_read_c_factor()
        assert actual == expected

    def test_loads_read_cvr_watts(self):
        expected = 1
        actual = self.dss.loads_read_cvr_watts()
        assert actual == expected

    def test_loads_read_cvr_vars(self):
        expected = 2
        actual = self.dss.loads_read_cvr_vars()
        assert actual == expected

