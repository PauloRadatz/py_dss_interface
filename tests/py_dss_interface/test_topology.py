# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 04:15 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_topology.py
# @Software : VSCode

import pytest


class TestTopology13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.solution_solve()

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_topology_num_loops(self):
        expected = 1
        actual = self.dss.topology_num_loops()
        assert expected == actual

    def test_topology_num_isolated_branches(self):
        expected = 0
        actual = self.dss.topology_num_isolated_branches()
        assert expected == actual

    def test_topology_num_isolated_loadss(self):
        expected = 0
        actual = self.dss.topology_num_isolated_loadss()
        assert expected == actual

    def test_topology_first(self):
        expected = 1
        actual = self.dss.topology_first()
        assert expected == actual

    def test_topology_next(self):
        expected = 0
        actual = self.dss.topology_next()
        assert expected == actual

    def test_topology_active_branch(self):
        expected = 0
        actual = self.dss.topology_active_branch()
        assert expected == actual

    def test_topology_forward_branch(self):
        expected = 0
        actual = self.dss.topology_forward_branch()
        assert expected == actual

    # TODO: Error in this test, OSError: exception: access violation reading 0x0000000000000028
    # def test_topology_backward_branch(self):
    #     expected = 0
    #     actual = self.dss.topology_backward_branch()
    #     assert expected == actual

    def test_topology_looped_branch(self):
        expected = 0
        actual = self.dss.topology_looped_branch()
        assert expected == actual

    def test_topology_parallel_branch(self):
        expected = 0
        actual = self.dss.topology_parallel_branch()
        assert expected == actual

    def test_topology_first_load(self):
        expected = 0
        actual = self.dss.topology_first_load()
        assert expected == actual

    def test_topology_next_load(self):
        expected = 0
        actual = self.dss.topology_next_load()
        assert expected == actual

    def test_topology_active_level(self):
        expected = 0
        actual = self.dss.topology_active_level()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================

    def test_topology_read_branch_name(self):
        expected = ''
        actual = self.dss.topology_read_branch_name()
        assert expected == actual

    # TODO: Needs to specify the correct input
    # def test_topology_write_branch_name(self):
    #     expected = ''
    #     self.dss.topology_write_branch_name(expected)
    #     actual = self.dss.topology_read_branch_name()
    #     assert expected == actual

    def test_topology_read_bus_name(self):
        expected = ''
        actual = self.dss.topology_read_bus_name()
        assert expected == actual

    # TODO: Needs to specify the correct input
    # def test_topology_write_bus_name(self):
    #     expected = ''
    #     self.dss.topology_write_branch_name(expected)
    #     actual = self.dss.topology_read_bus_name()
    #     assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_topology_all_looped_pairs(self):
        expected = ['Transformer.reg3', 'Transformer.reg2', 'Transformer.reg2',
                    'Line.650632', 'Transformer.reg1', 'Line.650632']
        actual = self.dss.topology_all_looped_pairs()
        assert expected == actual

    def test_topology_all_isolated_branches(self):
        expected = []
        actual = self.dss.topology_all_isolated_branches()
        assert expected == actual

    def test_topology_all_isolated_loads(self):
        expected = []
        actual = self.dss.topology_all_isolated_loads()
        assert expected == actual
