# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Meters.MetersS import MetersS
from py_dss_interface.models.Text.Text import Text


class MetersV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void MetersV(int32_t Parameter, VARIANT *Argument);

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def meters_allnames(self):
        """Returns an array of all Energy Meter names."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(0), ctypes.c_int(0), None)

    def meters_registernames(self):
        """Returns an array of strings containing the names of the registers."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(1), ctypes.c_int(0), None)

    def meters_registervalues(self):
        """Returns an array of values contained in the Meter registers for the active Meter."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(2), ctypes.c_int(0), None)

    def meters_totals(self):
        """Returns the totals for all registers of all Meters."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(3), ctypes.c_int(0), None)

    def meters_read_peakcurrent(self):
        """Returns an array of doubles with the Peak Current Property."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(4), ctypes.c_int(0), None)

    def meters_write_peakcurrent(self, argument):
        """Receives an array of doubles to set values of Peak Current Property."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        mt = MetersS(self.dss_obj)
        mt_name = mt.meters_read_name()
        return t.text(f'edit EnergyMeter.{mt_name} peakcurrent = {argument}')

    def meters_read_calcurrent(self):
        """Returns the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(6), ctypes.c_int(0), None)

    # # TODO: test in the future with a presence of an energymeter
    # TODO: comtypes
    # def meters_write_calcurrent(self, argument):
    #     """Sets the magnitude of the real part of the Calculated Current (normally determined by solution)
    #     for the meter to force some behavior on Load Allocation."""
    #     return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(7), argument, None)

    def meters_read_allocfactors(self):
        """Returns an array of doubles: allocation factors for the active Meter."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(8), ctypes.c_int(0), None)

    # TODO: comtypes
    def meters_write_allocfactors(self, argument):
        """Receives an array of doubles to set the phase allocation factors for the active Meter."""
        # argument = Base.check_string_param(argument)
        # return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(9), ctypes.c_int(1), None)
        t = Text(self.dss_obj)
        a = t.text("get mode ")
        return t.text(f'Allocateload {argument}')

    def meters_allendelements(self):
        """Returns a variant array of names of all zone end elements."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(10), ctypes.c_int(0), None)

    def meters_allbranchesinzone(self):
        """Returns a wide string list of all branches in zone of the active Energy Meter object."""
        return Bridge.VarArrayFunction(self.dss_obj.MetersV, ctypes.c_int(11), ctypes.c_int(0), None)
