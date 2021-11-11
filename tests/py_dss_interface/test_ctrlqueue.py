# -*- coding: utf-8 -*-
# @Time    : 6/27/2021 3:25 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_ctrlqueue.py
# @Software: PyCharm

import pytest


class TestBus13CtrlQueue:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("set loadmult=0.2")
        dss.solution_init_snap()
        dss.solution_solve_no_control()
        dss.solution_sample_control_devices()
        return dss

    def test_ctrlqueue_ctrlqueue(self, dss):
        actual_0 = dss.ctrlqueue_ctrlqueue()[0]
        actual_1 = dss.ctrlqueue_ctrlqueue()[1]
        actual_2 = dss.ctrlqueue_ctrlqueue()[2]
        actual_3 = dss.ctrlqueue_ctrlqueue()[3]

        assert actual_0 == 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device'
        assert actual_1 == '9, 0, 15, 0, 0, reg3 '
        assert actual_2 == '8, 0, 15, 0, 0, reg2 '
        assert actual_3 == '7, 0, 15, 0, 0, reg1 '

        dss.solution_do_control_actions()

        expected = ['No events']
        actual = dss.ctrlqueue_ctrlqueue()

        assert actual == expected

    def test_ctrlqueue_clear_queue(self, dss):
        dss.ctrlqueue_clear_queue()
        expected = ['No events']
        actual = dss.ctrlqueue_ctrlqueue()

        assert actual == expected

    def test_ctrlqueue_delete(self, dss):
        dss.ctrlqueue_delete(8)
        actual_0 = dss.ctrlqueue_ctrlqueue()[0]
        actual_1 = dss.ctrlqueue_ctrlqueue()[1]
        actual_2 = dss.ctrlqueue_ctrlqueue()[2]

        assert actual_0 == 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device'
        assert actual_1 == '9, 0, 15, 0, 0, reg3 '
        assert actual_2 == '7, 0, 15, 0, 0, reg1 '

        dss.ctrlqueue_delete(0)
        actual_0 = dss.ctrlqueue_ctrlqueue()[0]
        actual_1 = dss.ctrlqueue_ctrlqueue()[1]
        actual_2 = dss.ctrlqueue_ctrlqueue()[2]

        assert actual_0 == 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device'
        assert actual_1 == '9, 0, 15, 0, 0, reg3 '
        assert actual_2 == '7, 0, 15, 0, 0, reg1 '

    def test_ctrlqueue_num_actions(self, dss):
        pass
        # TODO
        # expected = 3
        # actual = dss.ctrlqueue_num_actions()
        # assert actual == expected

    def test_ctrlqueue_action(self, dss):
        # TODO
        pass
        # dss.ctrlqueue_action()

    def test_ctrlqueue_action_code(self, dss):
        # TODO
        pass
        # dss.ctrlqueue_action_code()

    def test_ctrlqueue_device_handle(self, dss):
        # TODO
        dss.ctrlqueue_device_handle()

    def test_ctrlqueue_queue_size(self, dss):
        expected = 3
        actual = dss.ctrlqueue_queue_size()
        assert actual == expected

    def test_ctrlqueue_pop_action(self, dss):
        # TODO
        pass
        # dss.ctrlqueue_pop_action()

    def test_ctrlqueue_do_all_queue(self, dss):
        expected = 0
        actual = dss.ctrlqueue_do_all_queue()
        assert actual == expected

        expected = ['No events']
        actual = dss.ctrlqueue_ctrlqueue()

        assert actual == expected

    def test_ctrlqueue_clear_actions(self, dss):
        pass
