# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Base import Base


class CapacitorsI(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are integers.

    The structure of the interface is as follows:
        int32_t CapacitorsI(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _num_steps(self) -> int:
        return self._dss_obj.CapacitorsI(0, 0)

    def _num_steps_write(self, argument: int) -> int:
        argument = Base._check_int_param(argument, default=1)
        return self._dss_obj.CapacitorsI(1, argument)

    def is_delta(self) -> int:
        return self._dss_obj.CapacitorsI(2, 0)

    def _is_delta_write(self, argument: int = 1) -> int:
        argument = Base._check_int_param(argument, default=1)
        return self._dss_obj.CapacitorsI(3, argument)

    def _first(self) -> int:
        return self._dss_obj.CapacitorsI(4, 0)

    def _next(self) -> int:
        return self._dss_obj.CapacitorsI(5, 0)

    def _count(self) -> int:
        return self._dss_obj.CapacitorsI(6, 0)

    def _add_step(self) -> int:
        result = self._dss_obj.CapacitorsI(7, 0)
        # Base.check_assertion_result(result, "Capacitor step problem detect!", "A problem occur when tried to adds "
        #                                                                       "step to a capacitor. Check capacitor/"
        #                                                                       "bank capacitor existence or available "
        #                                                                       "steps!", expected_value=1)
        return result

    def _subtract_step(self) -> int:
        result = self._dss_obj.CapacitorsI(8, 0)
        # Base.check_assertion_result(result, "Capacitor step problem detect!", "A problem occur when tried to subtract "
        #                                                                       "step to a capacitor. Check capacitor/"
        #                                                                       "bank capacitor existence or available "
        #                                                                       "steps!",
        #                             expected_value=1)
        return result

    def _available_steps(self) -> int:
        return self._dss_obj.CapacitorsI(9, 0)

    def _open_all_steps(self) -> int:
        return self._dss_obj.CapacitorsI(10, 0)

    def _close_all_steps(self) -> int:
        return self._dss_obj.CapacitorsI(11, 0)
