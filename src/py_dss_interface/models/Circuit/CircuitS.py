# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CircuitS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr CircuitS(int32_t Parameter, CStr Argument);

    This interface returns a string according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def circuit_name(self) -> str:
        """Returns the name of the active circuit."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def circuit_disable(self) -> str:
        """Allows to disable an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver the string ?Ok?."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def circuit_enable(self) -> str:
        """Allows to enable an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver the string ?Ok?."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def circuit_set_active_element(self, argument: str) -> str:
        """Allows to activate an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver a string with the index of the active element."""
        argument = Base.check_string_param(argument, default="")
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def circuit_set_active_bus(self, argument: str) -> str:
        """Allows to activate a bus of the active circuit, the bus must be specified by name.
        As a result, this parameter will deliver a string with the index of the active Bus."""
        argument = Base.check_string_param(argument, default="")
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def circuit_set_active_class(self, argument: str) -> str:
        """Allows tto activate a Class of the active circuit, the Class must be specified by name.
        As a result, this parameter will deliver a string with the index of the active Class."""
        argument = Base.check_string_param(argument, default="")
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')
