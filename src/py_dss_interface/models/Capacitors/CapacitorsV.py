# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class CapacitorsV(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are Variants.

    The structure of the interface is as follows:
        void CapacitorsV(int32_t Parameter, VARIANT *Argument)

    This interface returns a Variant, the first parameter is used to specify the property of the class to be used and
    the second parameter can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def capacitors_all_names(self) -> List[str]:
        """Gets a variant array of strings with all Capacitor names in the circuit."""
        return Bridge.var_array_function(self.dss_obj.CapacitorsV, 0, None, '')

    def capacitors_read_states(self) -> List[int]:
        """Gets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        result = Bridge.var_array_function(self.dss_obj.CapacitorsV, 1, None, '')
        if result == -1:
            raise ValueError("An error ocurred when tries to READ Capacitors states! ")
        return result

    def capacitors_write_states(self, dss_, argument: list) -> int:
        """Sets a variant array of integers [0..numsteps-1] indicating the state of each step. If value is -1 and
        error has occurred.
        :param dss_: an instance of dss object
        :param argument: list with status of Capacitor states
        """
        dss_.capacitors_write_name(dss_.capacitors_read_name())
        result = dss_.text(f'edit Capacitor.{dss_.capacitors_read_name()} states={argument}')
        if result != '':
            raise ValueError("An error occurred when tries to WRITE Capacitors states! ")
        return result
