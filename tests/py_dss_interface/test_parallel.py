# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 10:20 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_parallel.py
# @Software: PyCharm


import pytest


class TestParallel13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_parallel_num_cpus(self, dss):
        expected = 4
        actual = dss.parallel_num_cpus()
        #assert actual == expected

    def test_parallel_num_cores(self, dss):
        expected = 2
        actual = dss.parallel_num_cores()
        # assert actual == expected

    def test_parallel_read_active_actor(self, dss):
        expected = 1
        actual = dss.parallel_read_active_actor()
        assert actual == expected

    def test_parallel_write_active_actor(self, dss):
        expected = 2
        dss.parallel_create_actor()
        dss.parallel_write_active_actor(expected)
        actual = dss.parallel_read_active_actor()
        assert actual == expected

    def test_parallel_create_actor(self, dss):
        expected = 0
        actual = dss.parallel_create_actor()
        assert actual == expected

    def test_parallel_read_actor_cpu(self, dss):
        expected = -1
        actual = dss.parallel_read_actor_cpu()
        assert actual == expected

    def test_parallel_write_actor_cpu(self, dss):
        expected = 1
        dss.parallel_write_actor_cpu(expected)
        actual = dss.parallel_read_actor_cpu()
        assert actual == expected

    def test_parallel_num_actors(self, dss):
        # TODO: It works only if the method is performed alone, if you run all tests at the same time, it fails
        expected = 2
        dss.parallel_create_actor()
        actual = dss.parallel_num_actors()
        # assert actual == expected

    def test_parallel_wait(self, dss):
        expected = 0
        actual = dss.parallel_wait()
        assert actual == expected

    def test_parallel_read_active_parallel(self, dss):
        expected = 0
        actual = dss.parallel_read_active_parallel()
        assert actual == expected

    def test_parallel_write_active_parallel(self, dss):
        expected = 1
        dss.parallel_write_active_parallel(expected)
        actual = dss.parallel_read_active_parallel()
        assert actual == expected

    def test_parallel_read_concatenate_reportsl(self, dss):
        expected = 0
        actual = dss.parallel_read_concatenate_reportsl()
        assert actual == expected

    def test_parallel_write_concatenate_reportsl(self, dss):
        expected = 1
        dss.parallel_write_concatenate_reportsl(expected)
        actual = dss.parallel_read_concatenate_reportsl()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_parallel_actor_progress(self, dss):
        # TODO: It works only if the method is performed alone, if you run all tests at the same time, it fails
        expected = [0]
        actual = dss.parallel_actor_progress()
        # assert actual == expected

    def test_parallel_actor_status(self, dss):
        # TODO: It works only if the method is performed alone, if you run all tests at the same time, it fails
        expected = [1]
        actual = dss.parallel_actor_status()
        # assert actual == expected
