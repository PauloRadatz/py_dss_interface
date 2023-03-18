# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class SolutionI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t SolutionI(int32_t Parameter, int32_t Argument);

    This interface returns an integer according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _solve(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def _mode_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def _mode_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(2), ctypes.c_int32(argument)))

    def _hour_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(3), ctypes.c_int32(0)))

    def _hour_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(4), ctypes.c_int32(argument)))

    def _year_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(5), ctypes.c_int32(0)))

    def _year_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(6), ctypes.c_int32(argument)))

    def _iterations(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(7), ctypes.c_int32(0)))

    def _max_iterations_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(8), ctypes.c_int32(0)))

    def _max_iterations_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(9), ctypes.c_int32(argument)))

    def _number_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(10), ctypes.c_int32(0)))

    def _number_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(11), ctypes.c_int32(argument)))

    def _random_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(12), ctypes.c_int32(0)))

    def _random_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(13), ctypes.c_int32(argument)))

    def _load_model_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(14), ctypes.c_int32(0)))

    def _load_model_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(15), ctypes.c_int32(argument)))

    def _add_type_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(16), ctypes.c_int32(0)))

    def _add_type_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(17), ctypes.c_int32(argument)))

    def _algorithm_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(18), ctypes.c_int32(0)))

    def _algorithm_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(19), ctypes.c_int32(argument)))

    def _control_mode_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(20), ctypes.c_int32(0)))

    def _control_mode_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(21), ctypes.c_int32(argument)))

    def _control_iterations_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(22), ctypes.c_int32(0)))

    def _control_iterations_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(23), ctypes.c_int32(argument)))

    def _max_control_iterations_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(24), ctypes.c_int32(0)))

    def _max_control_iterations_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(25), ctypes.c_int32(argument)))

    def _sample_do_control_actions(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(26), ctypes.c_int32(0)))

    def _check_fault_status(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(27), ctypes.c_int32(0)))

    def _solve_direct(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(28), ctypes.c_int32(0)))

    def _solve_power_flow(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(29), ctypes.c_int32(0)))

    def _solve_no_control(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(30), ctypes.c_int32(0)))

    def _solve_plus_control(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(31), ctypes.c_int32(0)))

    def _init_snap(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(32), ctypes.c_int32(0)))

    def _check_controls(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(33), ctypes.c_int32(0)))

    def _sample_control_devices(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(34), ctypes.c_int32(0)))

    def _do_control_actions(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(35), ctypes.c_int32(0)))

    def _build_y_matrix(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(36), ctypes.c_int32(0)))

    def _system_y_changed(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(37), ctypes.c_int32(0)))

    def _converged_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(38), ctypes.c_int32(0)))

    def _converged_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(39), ctypes.c_int32(argument)))

    def _total_iterations(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(40), ctypes.c_int32(0)))

    def _most_iterations_done(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(41), ctypes.c_int32(0)))

    def _control_actions_done_read(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(42), ctypes.c_int32(0)))

    def _control_actions_done_write(self, argument) -> int:
        argument = Base._check_int_param(argument)
        return int(self._dss_obj.SolutionI(ctypes.c_int32(43), ctypes.c_int32(argument)))

    def _finish_time_step(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(44), ctypes.c_int32(0)))

    def _clean_up(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(45), ctypes.c_int32(0)))

    def _solve_all(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(46), ctypes.c_int32(0)))

    def _calc_inc_matrix(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(47), ctypes.c_int32(0)))

    # TODO include in test
    def _calc_inc_matrix_0(self) -> int:
        return int(self._dss_obj.SolutionI(ctypes.c_int32(48), ctypes.c_int32(0)))
