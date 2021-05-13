# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class PVSystems(Base):

    # PVsystemsF (Float)
    def pvsystems_read_irradiance(self):
        """Gets the present value of the Irradiance property in W/sq-m."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def pvsystems_write_irradiance(self, argument):
        """Sets the present value of the Irradiance property in W/sq-m."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def pvsystems_kw(self):
        """Gets the kW output."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def pvsystems_read_kvar(self):
        """Gets the kvar value."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def pvsystems_write_kvar(self, argument):
        """Sets the kvar value."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(4), ctypes.c_double(argument)))
        return result

    def pvsystems_read_pf(self):
        """Gets the power factor value."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def pvsystems_write_pf(self, argument):
        """Sets the power factor value."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(6), ctypes.c_double(argument)))
        return result

    def pvsystems_read_kvarated(self):
        """Gets the rated kVA of the PVSystem."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    def pvsystems_write_kvarated(self, argument):
        """Sets the rated kVA of the PVSystem."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(8), ctypes.c_double(argument)))
        return result

    def pvsystems_read_pmpp(self):
        """Gets the Pmpp."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(9), ctypes.c_double(0)))
        return result

    def pvsystems_read_kv(self):
        """Gets the kV."""
        result = float(self.dss_obj.PVsystemsF(ctypes.c_int32(11), ctypes.c_double(0)))
        return result

    # PVsystemsS (String)
    def pvsystems_read_name(self):
        """Gets the name of the active PVSystem."""
        result = ctypes.c_char_p(self.dss_obj.PVsystemsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def pvsystems_write_name(self, argument):
        """Sets the name of the active PVSystem."""
        result = ctypes.c_char_p(self.dss_obj.PVsystemsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # PVsystemsV (Variant)
    def pvsystems_allnames(self):
        """Gets the variant array of string containing names of all PVSystems in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.PVsystemsV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value
