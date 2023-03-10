# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models.Base import Base


class LoadsI(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are integers.

    The structure of the interface is as follows:
        int32_t DSSLoads(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def _first(self) -> int:
        """Allows to set the active load into the first load registered in the active circuit. As a result,
        this property will return the number 1. The parameter argument can be filled with a 0. """
        return self._dss_obj.DSSLoads(ctypes.c_int32(0), ctypes.c_int32(0))

    def _next(self) -> int:
        """Sets the active load into the next load registered in the active circuit. As a result, this property will
        deliver the index of the active load. The parameter argument can be filled with a 0. """
        return self._dss_obj.DSSLoads(ctypes.c_int32(1), ctypes.c_int32(0))

    def _idx(self) -> int:
        """Allows to read the index of the active load. The parameter argument can be filled with a 0."""
        return self._dss_obj.DSSLoads(ctypes.c_int32(2), ctypes.c_int32(0))

    def _idx_write(self, argument) -> int:
        """Allows to write the index of the active load. The parameter argument must contain the index of the desired
        active load. The return value will be equal to 0."""
        argument = Base._check_int_param(argument)
        return self._dss_obj.DSSLoads(ctypes.c_int32(3), ctypes.c_int32(argument))

    def _count(self) -> int:
        """Returns the number of load elements within the active circuit. The parameter argument can be filled with a
        0. """
        return self._dss_obj.DSSLoads(ctypes.c_int32(4), ctypes.c_int32(0))

    def _class(self) -> int:
        """Allows to read the code number used to separate loads by class or group. The parameter argument can be
        filled with a 0. """
        return self._dss_obj.DSSLoads(ctypes.c_int32(5), ctypes.c_int32(0))

    def _class_write(self, argument) -> int:
        """Allows to read the code number used to separate loads by class or group. The parameter argument can be
        filled with a 0. """
        argument = Base._check_int_param(argument)
        return self._dss_obj.DSSLoads(ctypes.c_int32(6), ctypes.c_int32(argument))

    def _model(self) -> int:
        """Allows to read the model of the active load. The parameter argument can be filled with a 0."""
        return self._dss_obj.DSSLoads(ctypes.c_int32(7), ctypes.c_int32(0))

    def _model_write(self, argument) -> int:
        """Allows to write the model of the active load using the parameter argument. This parameter will return a 0."""
        argument = Base._check_int_param(argument)
        return self._dss_obj.DSSLoads(ctypes.c_int32(8), ctypes.c_int32(argument))

    def _num_cust(self) -> int:
        """Allows to read the number of customer of the active load. The parameter argument can be filled with a 0."""
        return self._dss_obj.DSSLoads(ctypes.c_int32(9), ctypes.c_int32(0))

    def _num_cust_write(self, argument) -> int:
        """Allows to write the number of customers of the active load using the parameter argument. This parameter
        will return a 0. """
        argument = Base._check_int_param(argument)
        return self._dss_obj.DSSLoads(ctypes.c_int32(10), ctypes.c_int32(argument))

    def _status(self) -> int:
        """Allows to read Response to load multipliers: Fixed (growth only - 1), Exempt (no LD curve - 2), Variable (
        all - 0), of the active load. The parameter argument can be filled with a 0. """
        return self._dss_obj.DSSLoads(ctypes.c_int32(11), ctypes.c_int32(0))

    def _status_write(self, argument) -> int:
        """Allows to read Response to load multipliers: Fixed (growth only - 1), Exempt (no LD curve - 2), Variable (
        all - 0), of the active load. This parameter will return a 0. """
        argument = Base._check_int_param(argument)
        return self._dss_obj.DSSLoads(ctypes.c_int32(12), ctypes.c_int32(argument))

    def _is_delta(self) -> int:
        """Allows to read if the active load is connected in delta, if the answer is positive, this function will
        deliver a 1; otherwise, the answer will be 0. The parameter argument can be filled with a 0. """
        return self._dss_obj.DSSLoads(ctypes.c_int32(13), ctypes.c_int32(0))

    def _is_delta_write(self, argument) -> int:
        """Allows to read if the active load is connected in delta, if the answer is positive, this function will
        deliver a 1; otherwise, the answer will be 0. This parameter will return a 0. """
        argument = Base._check_int_param(argument)
        return self._dss_obj.DSSLoads(ctypes.c_int32(14), ctypes.c_int32(argument))
