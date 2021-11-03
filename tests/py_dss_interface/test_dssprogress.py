# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 10:10 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_dssprogress.py
# @Software: PyCharm


import pytest


class TestDSSProgress13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_dssprogress_pct_progress(self):
        # TODO: Returning zero, is it correct?
        expected = 0
        actual = self.dss.dssprogress_pct_progress(12.5)
        assert actual == expected

    def test_dssprogress_show(self):
        expected = 0
        actual = self.dss.dssprogress_show()
        assert actual == expected

    def test_dssprogress_close(self):
        expected = 0
        actual = self.dss.dssprogress_close()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_dssprogress_caption(self):
        # TODO: Its returning "0", is it correct?
        expected = "0"
        actual = self.dss.dssprogress_caption("my_caption")
        assert actual == expected
