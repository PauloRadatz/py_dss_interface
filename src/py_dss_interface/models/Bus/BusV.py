# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class BusV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void BUSV(int32_t Parameter, VARIANT *Argument)

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def _voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 0)

    def _seq_voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 1)

    def _nodes(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 2)

    def _voc(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 3)

    def _isc(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 4)

    def _pu_voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 5)

    def _zsc_matrix(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 6)

    def _zsc1(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 7)

    def _zsc0(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 8)

    def _ysc_matrix(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 9)

    def _cplx_sequence_voltages(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 10)

    def _vll(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 11)

    def _pu_vll(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 12)

    def _vmag_angle(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 13)

    def _vmag_angle_pu(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 14)

    def _line_list(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 15)

    def _load_list(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 16)

    def _axc_012_matrix(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 17)

    def _all_pce_active_bus(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 18)

    def _all_pde_active_bus(self) -> List[str]:
        return Bridge.variant_pointer_read(self._dss_obj.BUSV, 19)
