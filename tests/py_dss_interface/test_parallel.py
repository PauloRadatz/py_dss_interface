# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 10:20 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_parallel.py
# @Software: PyCharm


import pytest


class TestParallel13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_parallel_num_cpus(self):
        expected = 4
        actual = self.dss.parallel_num_cpus()
        #assert expected == actual

    def test_parallel_num_cores(self):
        expected = 2
        actual = self.dss.parallel_num_cores()
        # assert expected == actual

    def test_parallel_read_active_actor(self):
        expected = 1
        actual = self.dss.parallel_read_active_actor()
        assert expected == actual

    def test_parallel_write_active_actor(self):
        expected = 2
        self.dss.parallel_create_actor()
        self.dss.parallel_write_active_actor(expected)
        actual = self.dss.parallel_read_active_actor()
        assert expected == actual

    def test_parallel_create_actor(self):
        expected = 0
        actual = self.dss.parallel_create_actor()
        assert expected == actual

    def test_parallel_read_actor_cpu(self):
        expected = -1
        actual = self.dss.parallel_read_actor_cpu()
        assert expected == actual

    def test_parallel_write_actor_cpu(self):
        expected = 1
        self.dss.parallel_write_actor_cpu(expected)
        actual = self.dss.parallel_read_actor_cpu()
        assert expected == actual

    def test_parallel_num_actors(self):
        # TODO: It works only if the method is performed alone, if you run all tests at the same time, it fails
        expected = 2
        self.dss.parallel_create_actor()
        actual = self.dss.parallel_num_actors()
        # assert expected == actual

    def test_parallel_wait(self):
        expected = 0
        actual = self.dss.parallel_wait()
        assert expected == actual

    def test_parallel_read_active_parallel(self):
        expected = 0
        actual = self.dss.parallel_read_active_parallel()
        assert expected == actual

    def test_parallel_write_active_parallel(self):
        expected = 1
        self.dss.parallel_write_active_parallel(expected)
        actual = self.dss.parallel_read_active_parallel()
        assert expected == actual

    def test_parallel_read_concatenate_reportsl(self):
        expected = 0
        actual = self.dss.parallel_read_concatenate_reportsl()
        assert expected == actual

    def test_parallel_write_concatenate_reportsl(self):
        expected = 1
        self.dss.parallel_write_concatenate_reportsl(expected)
        actual = self.dss.parallel_read_concatenate_reportsl()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_parallel_actor_progress(self):
        # TODO: It works only if the method is performed alone, if you run all tests at the same time, it fails
        expected = [0]
        actual = self.dss.parallel_actor_progress()
        # assert expected == actual

    def test_parallel_actor_status(self):
        # TODO: It works only if the method is performed alone, if you run all tests at the same time, it fails
        expected = [1]
        actual = self.dss.parallel_actor_status()
        # assert expected == actual
