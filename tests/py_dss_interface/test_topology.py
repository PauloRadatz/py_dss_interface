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
        assert actual == expected

    def test_topology_num_isolated_branches(self):
        expected = 0
        actual = self.dss.topology_num_isolated_branches()
        assert actual == expected

    def test_topology_num_isolated_loadss(self):
        self.dss.text("New Load.test bus1=A")
        self.dss.solution_solve()
        expected = 1
        actual = self.dss.topology_num_isolated_loadss()
        assert actual == expected

    def test_topology_first(self):
        expected = 1
        actual = self.dss.topology_first()
        assert actual == expected

        expected = 'Vsource.source'
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    def test_topology_next(self):
        expected = 1
        self.dss.topology_first()
        actual = self.dss.topology_next()
        assert actual == expected

        expected = 'Transformer.sub'
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    def test_topology_active_branch(self):
        self.dss.topology_write_branch_name('Transformer.sub')
        expected = 1
        actual = self.dss.topology_active_branch()
        assert actual == expected

        expected = 'Transformer.sub'
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    def test_topology_forward_branch(self):
        self.dss.topology_write_branch_name('Vsource.source')
        expected = 1
        actual = self.dss.topology_forward_branch()
        assert actual == expected

        expected = 'Transformer.sub'
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    def test_topology_backward_branch(self):
        self.dss.topology_write_branch_name('Transformer.sub')
        expected = 1
        actual = self.dss.topology_backward_branch()
        assert actual == expected

        expected = 'Vsource.source'
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    # TODO
    def test_topology_looped_branch(self):
        self.dss.topology_write_branch_name('Transformer.sub')
        expected = 0
        actual = self.dss.topology_looped_branch()
        assert actual == expected

        self.dss.topology_write_branch_name('Transformer.reg3')
        expected = 1
        actual = self.dss.topology_looped_branch()
        assert actual == expected

    # TODO
    def test_topology_parallel_branch(self):
        expected = 0
        actual = self.dss.topology_parallel_branch()
        assert actual == expected

    def test_topology_first_load(self):
        self.dss.topology_write_branch_name("Line.670671")
        expected = 1
        actual = self.dss.topology_first_load()
        assert actual == expected # TODO understand

        # expected = 'Load.671'
        # actual = self.dss.topology_read_branch_name()
        # assert actual == expected

    def test_topology_next_load(self):
        expected = 0
        actual = self.dss.topology_next_load()
        assert actual == expected

    def test_topology_active_level(self):
        self.dss.topology_write_branch_name("Line.670671")
        expected = 5
        actual = self.dss.topology_active_level()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================

    def test_topology_read_branch_name(self):
        self.dss.topology_write_branch_name('Transformer.sub')
        expected = 'Transformer.sub'
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    def test_topology_write_branch_name(self):
        expected = 'Transformer.sub'
        self.dss.topology_write_branch_name(expected)
        actual = self.dss.topology_read_branch_name()
        assert actual == expected

    def test_topology_read_bus_name(self):
        self.dss.topology_write_branch_name('Transformer.sub')
        expected = 'sourcebus'
        actual = self.dss.topology_read_bus_name()
        assert actual == expected

    # TODO: Needs to specify the correct input
    def test_topology_write_bus_name(self):
        # expected = '670'
        # self.dss.topology_write_bus_name(expected)
        # actual = self.dss.topology_read_bus_name()
        # assert actual == expected
        #
        # expected = 'Line.650632'
        # actual = self.dss.topology_read_branch_name()
        # assert actual == expected
        pass

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_topology_all_looped_pairs(self):
        expected = ['Transformer.reg3', 'Transformer.reg2', 'Transformer.reg2',
                    'Line.650632', 'Transformer.reg1', 'Line.650632']
        actual = self.dss.topology_all_looped_pairs()
        assert actual == expected

    def test_topology_all_isolated_branches(self):
        expected = []
        actual = self.dss.topology_all_isolated_branches()
        assert actual == expected

    def test_topology_all_isolated_loads(self):
        expected = []
        actual = self.dss.topology_all_isolated_loads()
        assert actual == expected
