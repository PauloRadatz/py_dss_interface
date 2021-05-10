# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CapacitorsV(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are Variants.
    The structure of the interface is as follows:
        void CapacitorsV(int32_t Parameter, VARIANT *Argument)

    This interface returns a Variant, the first parameter is used to specify the property of the class to be used and
    the second parameter can be used to modify the value of the property when necessary. Reading and writing
    properties are separated and require a different parameter number to be executed.

    That parameter is an integer and could be call by the theses methods below.
    """

    def capacitors_allnames(self):
        """Gets a variant array of strings with all Capacitor names in the circuit."""
        return self.get_variant(0)

    def capacitors_read_states(self):
        """Gets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        result = self.get_variant(1)
        if result == -1:
            raise ValueError("An error ocurred when tries to READ Capacitors states! ")
        return result

    # TODO: must be change when linux compatibility comes
    def capacitors_write_states(self, argument):
        """Sets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.type(self).__name__(ctypes.c_int(2), variant_pointer)
        result = variant_pointer.contents.value
        if result == -1:
            raise ValueError("An error ocurred when tries to WRITE Capacitors states! ")
        return result
