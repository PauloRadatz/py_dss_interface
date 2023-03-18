# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class DSSInterfaceI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t DSSI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _num_circuits(self) -> int:
        return int(self._dss_obj.DSSI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def _clear_all(self):
        return self._dss_obj.DSSI(ctypes.c_int32(1), ctypes.c_int32(0))

    def _show_panel(self):
        return self._dss_obj.DSSI(ctypes.c_int32(2), ctypes.c_int32(0))

    def _start(self) -> int:
        return int(self._dss_obj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0)))

    def _num_classes(self) -> int:
        return int(self._dss_obj.DSSI(ctypes.c_int32(4), ctypes.c_int32(0)))

    def _num_user_classes(self) -> int:
        return int(self._dss_obj.DSSI(ctypes.c_int32(5), ctypes.c_int32(0)))

    def _reset(self):
        return self._dss_obj.DSSI(ctypes.c_int32(6), ctypes.c_int32(0))

    def _allow_forms_read(self):
        return self._dss_obj.DSSI(ctypes.c_int32(7), ctypes.c_int32(0))

    def _allow_forms_write(self, argument: int):
        """Sets if the DSS allows forms (1) or not (0), default (1). PAY ATTENTION: If arg=0 Then NoFormsAllowed :=
        TRUE (Only set to False) else NoFormsAllowed := FALSE; """
        return self._dss_obj.DSSI(ctypes.c_int32(8), argument)
