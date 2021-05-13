# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class TransformersV(Base):
    """
    This interface can be used to read/modify the properties of the Transformers Class where the values are Variants.

    The structure of the interface is as follows:
        void TransformersV(int32_t Parameter, VARIANT *Argument) ;

    This interface returns a Variant, the variable “parameter” is used to specify the property of the class to be
    used and the variable “argument” can be used to modify the value of the property when necessary. Reading and
    writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def transformers_allNames(self):
        """Gets a variant array of strings with all Transformer names in the active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TransformersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def transformers_wdgvoltages(self):
        """Gets a variant array of doubles containing the voltages at the active winding on the active transformer.
        These voltages come as complex pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TransformersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def transformers_wdgcurrents(self):
        """Gets a a variant array of doubles containing the currents at the active winding on the active transformer.
        These currents come as complex pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TransformersV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
