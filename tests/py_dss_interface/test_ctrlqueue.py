# -*- coding: utf-8 -*-
# @Time    : 6/27/2021 3:25 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : test_ctrlqueue.py
# @Software: PyCharm

import pytest
import platform


class TestBus13CtrlQueue:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("set loadmult=0.2")
        self.dss.solution_init_snap()
        self.dss.solution_solve_no_control()
        self.dss.solution_sample_control_devices()

    def test_ctrlqueue_ctrlqueue(self):

        actual_0 = self.dss.ctrlqueue_ctrlqueue()[0]
        actual_1 = self.dss.ctrlqueue_ctrlqueue()[1]
        actual_2 = self.dss.ctrlqueue_ctrlqueue()[2]
        actual_3 = self.dss.ctrlqueue_ctrlqueue()[3]

        assert actual_0 == 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device'
        assert actual_1 == '9, 0, 15, 0, 0, reg3 '
        assert actual_2 == '8, 0, 15, 0, 0, reg2 '
        assert actual_3 == '7, 0, 15, 0, 0, reg1 '

        self.dss.solution_do_control_actions()

        expected = ['No events']
        actual = self.dss.ctrlqueue_ctrlqueue()

        assert actual == expected

    def test_ctrlqueue_clear_queue(self):
        self.dss.ctrlqueue_clear_queue()
        expected = ['No events']
        actual = self.dss.ctrlqueue_ctrlqueue()

        assert actual == expected

    def test_ctrlqueue_delete(self):
        self.dss.ctrlqueue_delete(8)
        actual_0 = self.dss.ctrlqueue_ctrlqueue()[0]
        actual_1 = self.dss.ctrlqueue_ctrlqueue()[1]
        actual_2 = self.dss.ctrlqueue_ctrlqueue()[2]

        assert actual_0 == 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device'
        assert actual_1 == '9, 0, 15, 0, 0, reg3 '
        assert actual_2 == '7, 0, 15, 0, 0, reg1 '

        self.dss.ctrlqueue_delete(0)
        actual_0 = self.dss.ctrlqueue_ctrlqueue()[0]
        actual_1 = self.dss.ctrlqueue_ctrlqueue()[1]
        actual_2 = self.dss.ctrlqueue_ctrlqueue()[2]

        assert actual_0 == 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device'
        assert actual_1 == '9, 0, 15, 0, 0, reg3 '
        assert actual_2 == '7, 0, 15, 0, 0, reg1 '

    def test_ctrlqueue_num_actions(self):
        pass
        # todo
        # expected = 3
        # actual = self.dss.ctrlqueue_num_actions()
        # assert actual == expected

    def test_ctrlqueue_action(self):
        #todo
        pass
        # self.dss.ctrlqueue_action()

    def test_ctrlqueue_action_code(self):
        #todo
        pass
        # self.dss.ctrlqueue_action_code()

    def test_ctrlqueue_device_handle(self):
        # TODO
        self.dss.ctrlqueue_device_handle()

    def test_ctrlqueue_queue_size(self):
        expected = 3
        actual = self.dss.ctrlqueue_queue_size()
        assert actual == expected

    def test_ctrlqueue_pop_action(self):
        #todo
        pass
        # self.dss.ctrlqueue_pop_action()

    def test_ctrlqueue_do_all_queue(self):
        expected = 0
        actual = self.dss.ctrlqueue_do_all_queue()
        assert actual == expected

        expected = ['No events']
        actual = self.dss.ctrlqueue_ctrlqueue()

        assert actual == expected

    def test_ctrlqueue_clear_actions(self):
        pass





