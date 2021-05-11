# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from typing import List

from comtypes import automation
from py_dss_interface.models.Base import Base


class CktElementV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CktElementV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant (the format depends on the parameter) with the result of the query according to
    the value of the variable Parameter, which can be one of the following.
    """

    def cktelement_read_bus_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the buses connected to the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_write_bus_names(self, argument):
        """Allows to fix an array of strings with the names of all the buses connected to the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.CktElementV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_voltages(self) -> List[float]:
        """Delivers an array of doubles with the voltages at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_currents(self) -> List[float]:
        """Delivers an array of doubles with the currents at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_powers(self) -> List[float]:
        """Delivers an array of doubles with the powers at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_phase_losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses per phase at the terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component voltages per phase at the terminals of the active
         circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component Currents per phase at the terminals of the active
         circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seq_powers(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component powers per phase at the terminals of the active
        circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(9), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_all_property_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the properties of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(10), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_residuals(self) -> List[float]:
        """Delivers an array of doubles with the residual currents (magnitude, angle) in all the nodes of the active
        circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(11), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_y_prim(self) -> List[float]:
        """Delivers an array of doubles with the Y primitive matrix (complex) of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(12), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_cplx_seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence voltages for all terminals of the active circuit
        element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(13), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_cplx_seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence currents for all terminals of the active circuit
        element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(14), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_all_variables_names(self) -> List[str]:
        """Delivers a Variant array of strings listing all the published state variable names, if the active circuit
        element is a PCElement. Otherwise, null string."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(15), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_all_variables_values(self) -> List[float]:
        """Delivers a Variant array of doubles listing all the values of the state variables, if the active circuit
        element is a PCElement. Otherwise, null string."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(16), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_node_order(self) -> List[int]:
        """Delivers a Variant array integers variant array of integer containing the node numbers (representing phases,
        for example) for each conductor of each terminal."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(17), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_currents_mag_ang(self) -> List[float]:
        """Delivers the currents in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(18), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_voltages_mag_ang(self) -> List[float]:
        """Delivers the voltages in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(19), variant_pointer)
        return variant_pointer.contents.value
