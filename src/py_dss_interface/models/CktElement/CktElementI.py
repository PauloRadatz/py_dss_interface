# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CktElementI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t CktElementI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _num_terminals(self) -> int:
        return int(self._dss_obj.CktElementI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def _num_conductors(self) -> int:
        return int(self._dss_obj.CktElementI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def _num_phases(self) -> int:
        return int(self._dss_obj.CktElementI(ctypes.c_int32(2), ctypes.c_int32(0)))

    def _open_terminal(self, argument: int) -> int:
        return int(self._dss_obj.CktElementI(ctypes.c_int32(3), ctypes.c_int32(argument)))

    def _close_terminal(self, argument: int) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def _is_terminal_open(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(5), ctypes.c_int32(0))

    def _num_properties(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(6), ctypes.c_int32(0))

    def _has_switch_control(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _has_volt_control(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(8), ctypes.c_int32(0))

    def _num_controls(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(9), ctypes.c_int32(0))

    def _ocp_dev_index(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(10), ctypes.c_int32(0))

    def _ocp_dev_type(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(11), ctypes.c_int32(0))

    def _is_enabled(self) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(12), ctypes.c_int32(0))

    def _enabled(self, argument: int) -> int:
        return self._dss_obj.CktElementI(ctypes.c_int32(13), ctypes.c_int32(argument))
