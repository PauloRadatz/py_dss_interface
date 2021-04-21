# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CircuitS(Base):

    def name(self):
        """Returns the name of the active circuit."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def disable(self):
        """Allows to disable an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver the string ?Ok?."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def enable(self):
        """Allows to enable an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver the string ?Ok?."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def set_active_element(self, argument):
        """Allows to activate an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver a string with the index of the active element."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def set_active_bus(self, argument):
        """Allows to activate a bus of the active circuit, the bus must be specified by name.
        As a result, this parameter will deliver a string with the index of the active Bus."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def set_active_class(self, argument):
        """Allows tto activate a Class of the active circuit, the Class must be specified by name.
        As a result, this parameter will deliver a string with the index of the active Class."""
        result = ctypes.c_char_p(self.dss_obj.CircuitS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')
