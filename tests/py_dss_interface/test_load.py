# -*- coding: utf-8 -*-
# @Time    : 6/24/2021 10:51 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_load.py
# @Software: PyCharm

import pytest
import platform


class TestLoad13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    def test_circuit_num_ckt_elements(self):
        expected = 15
        actual = self.dss.loads_count()
        assert actual == expected
