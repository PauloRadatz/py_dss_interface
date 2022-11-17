# -*- coding: utf-8 -*-
# @Time     : 09/07/2021 02:56
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_text.py
# @Software : VSCode

import pytest


class TestText13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.solution.solve()

        return dss

    # ===================================================================
    # String methods
    # ===================================================================
    def test_text(self, dss):
        expected = "2000"
        actual = dss.text('? Line.650632.Length')
        assert actual == expected
