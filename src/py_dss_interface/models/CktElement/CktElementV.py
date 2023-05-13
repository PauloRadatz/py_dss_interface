# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.utils.Error import Error


class CktElementV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void CktElementV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant (the format depends on the parameter) with the result of the query according to
    the value of the variable Parameter, which can be one of the following.
    """

    def _bus_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 0)

    def _bus_names_write(self, argument: List[str]):
        # # TODO learn what to do.
        # for i in range(len(argument)):
        #     argument[i] = argument[i].encode('ascii')

        Error.method_not_working("setter of dss.cktelement.bus_names")
        # return Bridge.variant_pointer_write(self.dss_obj.CktElementV, 1, argument)

    def _voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 2)

    def _currents(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 3)

    def _powers(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 4)

    def _losses(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 5)

    def _phase_losses(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 6)

    def _seq_voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 7)

    def _seq_currents(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 8)

    def _seq_powers(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 9)

    def _property_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 10)

    def _residuals_currents(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 11)

    def _y_prim(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 12)

    def _cplx_seq_voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 13)

    def _cplx_seq_currents(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 14)

    # https://github.com/PauloRadatz/py_dss_interface/issues/3
    def _variables_names(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 15)

    # https://github.com/PauloRadatz/py_dss_interface/issues/4
    def _variables_values(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 16)

    def _node_order(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 17)

    def _currents_mag_ang(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 18)

    def _voltages_mag_ang(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.CktElementV, 19)
