# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 04:15 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_topology.py
# @Software : VSCode

import pytest


class TestTopology13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.solution.solve()

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_topology_num_loops(self, dss):
        expected = 1
        actual = dss.topology.num_loops
        assert actual == expected

    def test_topology_num_isolated_branches(self, dss):
        expected = 0
        actual = dss.topology.num_isolated_branches
        assert actual == expected

    def test_topology_num_isolated_loads(self, dss):
        dss.text("edit Line.670671 enabled=no")
        expected = 7
        actual = dss.topology.num_isolated_loads
        assert actual == expected

    def test_topology_first(self, dss):
        expected = 1
        actual = dss.topology.first()
        assert actual == expected

        expected = 'Vsource.source'
        actual = dss.topology.branch_name
        assert actual == expected

    def test_topology_next(self, dss):
        expected = 1
        dss.topology.first()
        actual = dss.topology.next()
        assert actual == expected

        expected = 'Transformer.sub'
        actual = dss.topology.branch_name
        assert actual == expected

    def test_topology_active_branch(self, dss):
        dss.topology.branch_name = 'Transformer.sub'
        expected = 1
        actual = dss.topology.active_branch
        assert actual == expected

        expected = 'Transformer.sub'
        actual = dss.topology.branch_name
        assert actual == expected

    def test_topology_forward_branch(self, dss):
        dss.topology.branch_name = 'Vsource.source'
        expected = 1
        actual = dss.topology.forward_branch()
        assert actual == expected

        expected = 'Transformer.sub'
        actual = dss.topology.branch_name
        assert actual == expected

    def test_topology_backward_branch(self, dss):
        dss.topology.branch_name = 'Transformer.sub'
        expected = 1
        actual = dss.topology.backward_branch()
        assert actual == expected

        expected = 'Vsource.source'
        actual = dss.topology.branch_name
        assert actual == expected

    # TODO
    def test_topology_looped_branch(self, dss):
        dss.topology.branch_name = 'Transformer.sub'
        expected = 0
        actual = dss.topology.looped_branch()
        assert actual == expected

        dss.topology.branch_name = 'Transformer.reg3'
        expected = 1
        actual = dss.topology.looped_branch()
        assert actual == expected

    # TODO
    def test_topology_parallel_branch(self, dss):
        expected = 0
        actual = dss.topology.parallel_branch()
        assert actual == expected

    def test_topology_first_load(self, dss):
        dss.topology.branch_name = "Line.670671"
        expected = 1
        actual = dss.topology.first_load()
        assert actual == expected  # TODO understand

        # expected = 'Load.671'
        # actual = dss.topology.branch_name()
        # assert actual == expected

    def test_topology_next_load(self, dss):
        # TODO understand it
        dss.topology.branch_name = "Transformer.XFM1"

        expected = "Load.634a"
        dss.topology.first_load()
        # actual = dss.topology.branch_name
        actual = dss.cktelement.name
        assert actual == expected

        expected = 1
        actual = dss.topology.next_load()
        assert actual == expected

        expected = "Load.634b"
        # actual = dss.topology.branch_name
        actual = dss.cktelement.name
        assert actual == expected

    def test_topology_active_level(self, dss):
        dss.topology.branch_name = "Line.670671"
        expected = 5
        actual = dss.topology.active_level
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================

    def test_topology_read_branch_name(self, dss):
        dss.topology.branch_name = 'Transformer.sub'
        expected = 'Transformer.sub'
        actual = dss.topology.branch_name
        assert actual == expected

    def test_topology_write_branch_name(self, dss):
        expected = 'Transformer.sub'
        dss.topology.branch_name = expected
        actual = dss.topology.branch_name
        assert actual == expected

    def test_topology_read_bus_name(self, dss):
        dss.topology.branch_name = 'Transformer.sub'
        expected = "sourcebus"
        actual = dss.topology.bus_name
        assert actual == expected

    # TODO: Needs to specify the correct input
    def test_topology_write_bus_name(self, dss):
        # expected = '670'
        # dss.topology.bus_name(expected)
        # actual = dss.topology.bus_name()
        # assert actual == expected
        #
        # expected = 'Line.650632'
        # actual = dss.topology.branch_name()
        # assert actual == expected
        pass

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_topology_all_looped_pairs(self, dss):
        expected = ['Transformer.reg3', 'Transformer.reg2', 'Transformer.reg2',
                    'Line.650632', 'Transformer.reg1', 'Line.650632']
        actual = dss.topology.all_looped_pairs
        assert actual == expected

    def test_topology_all_isolated_branches(self, dss):
        dss.text("edit Line.670671 enabled=no")
        expected = ['Capacitor.cap1',
                    'Capacitor.cap2',
                    'Line.670671',
                    'Line.671680',
                    'Line.692675',
                    'Line.671684',
                    'Line.684611',
                    'Line.684652',
                    'Line.671692']
        actual = dss.topology.all_isolated_branches
        assert actual == expected

    def test_topology_all_isolated_loads(self, dss):
        dss.text("edit Line.670671 enabled=no")
        expected = ['Load.671',
                    'Load.692',
                    'Load.675a',
                    'Load.675b',
                    'Load.675c',
                    'Load.611',
                    'Load.652']
        actual = dss.topology.all_isolated_loads
        assert actual == expected
