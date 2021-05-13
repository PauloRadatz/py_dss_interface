# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class Sensors(Base):
    """
    This interface implements the Sensors (ISensors) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: .
    """

    # SensorsS (String)
    def sensors_read_name(self):
        """Gets the name of the active sensor object."""
        result = ctypes.c_char_p(self.dss_obj.SensorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def sensors_write_name(self, argument):
        """Sets the name of the active sensor object."""
        result = ctypes.c_char_p(self.dss_obj.SensorsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def sensors_read_meteredelement(self):
        """Gets the full name of the measured element."""
        result = ctypes.c_char_p(self.dss_obj.SensorsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def sensors_write_meteredelement(self, argument):
        """Sets the full name of the measured element."""
        result = ctypes.c_char_p(self.dss_obj.SensorsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SensorsV (Variant)
    def sensors_allnames(self):
        """Returns a variant array of sensor names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SensorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def sensors_read_currents(self):
        """Gets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SensorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def sensors_write_currents(self, argument):
        """Sets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.SensorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def sensors_read_kvars(self):
        """Gets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SensorsV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def sensors_write_kvars(self, argument):
        """Sets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.SensorsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def sensors_read_kws(self):
        """Gets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.SensorsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def sensors_write_kws(self, argument):
        """Sets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.SensorsV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value
