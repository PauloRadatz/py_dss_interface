# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class DSSInterface(Base):

    # DSSI (int)
    def dss_numcircuits(self):
        """Gets the number of circuits currently defined."""
        result = int(self.dss_obj.DSSI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def dss_clearall(self):
        """Clears all circuit definitions."""
        self.dss_obj.DSSI(ctypes.c_int32(1), ctypes.c_int32(0))

    def dss_showpanel(self):
        """Shows non-MDI child form of the Main DSS Edit form."""
        self.dss_obj.DSSI(ctypes.c_int32(2), ctypes.c_int32(0))

    def dss_start(self):
        """Validates the user and starts the DSS. Returns TRUE (1) if successful."""
        result = int(self.dss_obj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def dss_numclasses(self):
        """Gets the number of DSS intrinsic classes."""
        result = int(self.dss_obj.DSSI(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result

    def dss_numuserclasses(self):
        """Gets the number of user-defined classes."""
        result = int(self.dss_obj.DSSI(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result

    def dss_reset(self):
        """Resets DSS initialization for restarts, etc. from applets."""
        self.dss_obj.DSSI(ctypes.c_int32(6), ctypes.c_int32(0))

    def dss_read_allow_forms(self):
        """Gets if the DSS allows forms (1) or not (0), default (1)."""
        self.dss_obj.DSSI(ctypes.c_int32(7), ctypes.c_int32(0))

    def dss_write_allowforms(self, argument):
        """Sets if the DSS allows forms (1) or not (0), default (1)."""
        self.dss_obj.DSSI(ctypes.c_int32(8), ctypes.c_int32(argument))

    # DSSS (String)
    def dss_newcircuit(self, argument):
        """Makes a new circuit, the name of the circuit must be specified in the Argument."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dss_version(self):
        """Gets the version string for the DSS."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def dss_read_datapath(self):
        """Gets the Data File Path. Default for reports, etc. from DSS."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def dss_write_datapath(self, argument):
        """Gets the Data File Path. Default for reports, etc. from DSS."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dss_default_editor(self):
        """Gets the path name for the default text editor."""
        result = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # DSSV (Variant)
    def dss_classes(self):
        """Gets the list of DSS intrinsic classes (names of the classes)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value

    def dss_user_classes(self):
        """Gets list of user-defined classes (names of the classes)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.DSSV(ctypes.c_int32(1), variant_pointer)
        return variant_pointer.contents.value
