# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Meters import Meters
from py_dss_interface.models.Text.Text import Text


class MetersV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void MetersV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    # TODO add type return

    def _names(self):
        """Returns an array of all Energy Meter names."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(0), ctypes.c_int(0), None)

    def _register_names(self):
        """Returns an array of strings containing the names of the registers."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(1), ctypes.c_int(0), None)

    def _register_values(self):
        """Returns an array of values contained in the Meter registers for the active Meter."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(2), ctypes.c_int(0), None)

    def _totals(self):
        """Returns the totals for all registers of all Meters."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(3), ctypes.c_int(0), None)

    def _peak_current_read(self):
        """Returns an array of doubles with the Peak Current Property."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(4), ctypes.c_int(0), None)

    def _peak_current_write(self, argument):
        """Receives an array of doubles to set values of Peak Current Property."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        mt = Meters.Meters(self.dss_obj)
        mt_name = mt.name
        return t.text(f'edit EnergyMeter.{mt_name} peakcurrent = {argument}')

    def _calc_current_read(self):
        """Returns the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(6), ctypes.c_int(0), None)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/6
    # TODO include in test
    def _calc_current_write(self, argument: str):
        """Sets the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(7), argument, None)

    def _alloc_factors_read(self):
        """Returns an array of doubles: allocation factors for the active Meter."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(8), ctypes.c_int(0), None)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/7
    def _alloc_factors_write(self, argument):
        """Receives an array of doubles to set the phase allocation factors for the active Meter."""
        # argument = Base.check_string_param(argument)
        # return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(9), ctypes.c_int(1), None)
        t = Text(self.dss_obj)
        t.text("get mode ")
        return t.text(f'Allocateload {argument}')

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/8
    def _all_end_elements(self):
        """Returns a variant array of names of all zone end elements."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(10), ctypes.c_int(0), None)

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/9
    # TODO include in test
    def _all_branches_in_zone(self):
        """Returns a wide string list of all branches in zone of the active Energy Meter object."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(11), ctypes.c_int(0), None)

    def _all_pce_in_zone(self):
        """This parameter returns a wide string list of all the PCE in zone of the active Energy Meter object."""
        return Bridge.var_array_function(self.dss_obj.MetersV, ctypes.c_int(12), ctypes.c_int(0), None)
