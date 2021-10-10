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

    def dss_num_circuits(self) -> int:
        """Gets the number of circuits currently defined."""
        return int(self.dss_obj.DSSI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def dss_clear_all(self):
        """Clears all circuit definitions."""
        return self.dss_obj.DSSI(ctypes.c_int32(1), ctypes.c_int32(0))

    def dss_show_panel(self):
        """Shows non-MDI child form of the Main DSS Edit form."""
        return self.dss_obj.DSSI(ctypes.c_int32(2), ctypes.c_int32(0))

    def dss_start(self) -> int:
        """Validates the user and starts the DSS. Returns TRUE (1) if successful."""
        return int(self.dss_obj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0)))

    def dss_num_classes(self) -> int:
        """Gets the number of DSS intrinsic classes."""
        return int(self.dss_obj.DSSI(ctypes.c_int32(4), ctypes.c_int32(0)))

    def dss_num_user_classes(self) -> int:
        """Gets the number of user-defined classes."""
        return int(self.dss_obj.DSSI(ctypes.c_int32(5), ctypes.c_int32(0)))

    def dss_reset(self):
        """Resets DSS initialization for restarts, etc. from applets. Nothing implemented in the OpenDSS Original
        Source Code """
        return self.dss_obj.DSSI(ctypes.c_int32(6), ctypes.c_int32(0))

    def dss_read_allow_forms(self):
        """Gets if the DSS allows forms (1) or not (0), default (1)."""
        return self.dss_obj.DSSI(ctypes.c_int32(7), ctypes.c_int32(0))

    def dss_write_allow_forms(self, argument: int):
        """Sets if the DSS allows forms (1) or not (0), default (1). PAY ATTENTION: If arg=0 Then NoFormsAllowed :=
        TRUE (Only set to False) else NoFormsAllowed := FALSE; """
        return self.dss_obj.DSSI(ctypes.c_int32(8), argument)
