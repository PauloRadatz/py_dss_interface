# -*- coding: utf-8 -*-
# @Time     : 09/07/2021 02:56
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_text.py
# @Software : VSCode

import pytest


class TestText13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.solution_solve()

    # ===================================================================
    # String methods
    # ===================================================================
    def test_text(self):
        expected = "2000"
        actual = self.dss.text('? Line.650632.Length')
        assert expected == actual
