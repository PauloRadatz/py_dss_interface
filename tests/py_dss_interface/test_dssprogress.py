# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 10:10 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_dssprogress.py
# @Software: PyCharm


import pytest


class TestDSSProgress13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    # def test_dssprogress_pct_progress(self, dss):
    #     # TODO: Returning zero, is it correct?
    #     expected = 0
    #     actual = dss.dssprogress_pct_progress(12.5)
    #     dss.dssprogress_close()
    #     assert actual == expected
    #
    # def test_dssprogress_show(self, dss):
    #     expected = 0
    #     actual = dss.dssprogress_show()
    #     dss.dssprogress_close()
    #     assert actual == expected
    #
    # def test_dssprogress_close(self, dss):
    #     expected = 0
    #     actual = dss.dssprogress_close()
    #     assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    # def test_dssprogress_caption(self, dss):
    #     # TODO: Its returning "0", is it correct?
    #     expected = "0"
    #     actual = dss.dssprogress_caption("my_caption")
    #     assert actual == expected
