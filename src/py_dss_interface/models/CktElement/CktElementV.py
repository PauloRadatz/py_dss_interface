# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class CktElementV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CktElementV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant (the format depends on the parameter) with the result of the query according to
    the value of the variable Parameter, which can be one of the following.
    """

    def _bus_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the buses connected to the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 0, None, '')

    def _bus_names_write(self, argument: List[str]):
        """Allows to fix an array of strings with the names of all the buses connected to the active circuit element."""

        # # TODO learn what to do.
        # for i in range(len(argument)):
        #     argument[i] = argument[i].encode('ascii')

        return Bridge.variant_pointer_write(self.dss_obj.CktElementV, 1, argument)

    def _voltages(self) -> List[float]:
        """Delivers an array of doubles with the voltages at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 2, None, '')

    def _currents(self) -> List[float]:
        """Delivers an array of doubles with the currents at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 3, None, '')

    def _powers(self) -> List[float]:
        """Delivers an array of doubles with the powers at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 4, None, '')

    def _losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 5, None, '')

    def _phase_losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses per phase at the terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 6, None, '')

    def _seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component voltages per phase at the terminals of the active
         circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 7, None, '')

    def _seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component Currents per phase at the terminals of the active
         circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 8, None, '')

    def _seq_powers(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component powers per phase at the terminals of the active
        circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 9, None, '')

    def _property_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the properties of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 10, None, '')

    def _residuals_currents(self) -> List[float]:
        """Delivers an array of doubles with the residual currents (magnitude, angle) in all the nodes of the active
        circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 11, None, '')

    def _y_prim(self) -> List[float]:
        """Delivers an array of doubles with the Y primitive matrix (complex) of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 12, None, '')

    def _cplx_seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence voltages for all terminals of the active circuit
        element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 13, None, '')

    def _cplx_seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence currents for all terminals of the active circuit
        element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 14, None, '')

    # https://github.com/PauloRadatz/py_dss_interface/issues/3
    def _variables_names(self) -> List[str]:
        """Delivers a Variant array of strings listing all the published state variable names, if the active circuit
        element is a PCElement. Otherwise, null string."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 15, None, '')

    # https://github.com/PauloRadatz/py_dss_interface/issues/4
    def _variables_values(self) -> List[float]:
        """Delivers a Variant array of doubles listing all the values of the state variables, if the active circuit
        element is a PCElement. Otherwise, null string."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 16, None, '')

    def _node_order(self) -> List[int]:
        """Delivers a Variant array integers variant array of integer containing the node numbers (representing phases,
        for Example) for each conductor of each terminal."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 17, None, '')

    def _currents_mag_ang(self) -> List[float]:
        """Delivers the currents in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        return Bridge.var_array_function(self.dss_obj.CktElementV, 18, None, '')

    def _voltages_mag_ang(self) -> List[float]:
        """Delivers the voltages in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        return Bridge.var_array_function(self.dss_obj.CktElementV, 19, None, '')
