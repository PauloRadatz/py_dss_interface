# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
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

    def cktelement_read_bus_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the buses connected to the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 0, None, '')

    def cktelement_write_bus_names(self, dss, argument: List[str]) -> str:
        """Allows to fix an array of strings with the names of all the buses connected to the active circuit element."""
        # 1 get size of number of elements conected to the active circuit
        total_connected = len(self.cktelement_read_bus_names())
        result = '0'
        for _ in range(total_connected):
            result = dss.text(f"Edit {dss.cktelement_name()} Bus1={argument[0]} Bus2={argument[1]}")
            if result != '':
                result = Base.warn_msg(f"An error occur when tried to *rename Buses* connected to "
                                       f"*{dss.cktelement_name()}*", Exception)
            return "Buses renamed succesfully!!"
        return result

    def cktelement_voltages(self) -> List[float]:
        """Delivers an array of doubles with the voltages at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 2, None, '')

    def cktelement_currents(self) -> List[float]:
        """Delivers an array of doubles with the currents at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 3, None, '')

    def cktelement_powers(self) -> List[float]:
        """Delivers an array of doubles with the powers at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 4, None, '')

    def cktelement_losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses at terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 5, None, '')

    def cktelement_phase_losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses per phase at the terminals of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 6, None, '')

    def cktelement_seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component voltages per phase at the terminals of the active
         circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 7, None, '')

    def cktelement_seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component Currents per phase at the terminals of the active
         circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 8, None, '')

    def cktelement_seq_powers(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component powers per phase at the terminals of the active
        circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 9, None, '')

    def cktelement_all_property_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the properties of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 10, None, '')

    def cktelement_residuals(self) -> List[float]:
        """Delivers an array of doubles with the residual currents (magnitude, angle) in all the nodes of the active
        circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 11, None, '')

    def cktelement_y_prim(self) -> List[float]:
        """Delivers an array of doubles with the Y primitive matrix (complex) of the active circuit element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 12, None, '')

    def cktelement_cplx_seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence voltages for all terminals of the active circuit
        element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 13, None, '')

    def cktelement_cplx_seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence currents for all terminals of the active circuit
        element."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 14, None, '')

    # https://github.com/PauloRadatz/py_dss_interface/issues/3
    def cktelement_all_variables_names(self) -> List[str]:
        """Delivers a Variant array of strings listing all the published state variable names, if the active circuit
        element is a PCElement. Otherwise, null string."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 15, None, '')

    # https://github.com/PauloRadatz/py_dss_interface/issues/4
    def cktelement_all_variables_values(self) -> List[float]:
        """Delivers a Variant array of doubles listing all the values of the state variables, if the active circuit
        element is a PCElement. Otherwise, null string."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 16, None, '')

    def cktelement_node_order(self) -> List[int]:
        """Delivers a Variant array integers variant array of integer containing the node numbers (representing phases,
        for Example) for each conductor of each terminal."""
        return Bridge.var_array_function(self.dss_obj.CktElementV, 17, None, '')

    def cktelement_currents_mag_ang(self) -> List[float]:
        """Delivers the currents in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        return Bridge.var_array_function(self.dss_obj.CktElementV, 18, None, '')

    def cktelement_voltages_mag_ang(self) -> List[float]:
        """Delivers the voltages in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        return Bridge.var_array_function(self.dss_obj.CktElementV, 19, None, '')
