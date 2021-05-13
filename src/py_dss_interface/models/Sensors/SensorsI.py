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
    # SensorsI (int)
    def sensors_count(self):
        """Gets number of sensors in active circuit."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def sensors_first(self):
        """Sets the first sensor active. Returns 0 if none."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def sensors_next(self):
        """Sets the next sensor active. Returns 0 if none."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def sensors_read_isdelta(self):
        """Returns 1 if the sensor is connected in delta; otherwise, returns 0."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def sensors_write_isdelta(self, argument):
        """Allows to set 1 if the sensor is connected in delta; otherwise, set 0 (argument)."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def sensors_read_reversedelta(self):
        """Returns 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def sensors_write_reversedelta(self, argument):
        """Allows to set 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def sensors_read_meteredterminal(self):
        """Gets the number of the measured terminal in the measured element."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def sensors_write_meteredterminal(self, argument):
        """Sets the number of the measured terminal in the measured element."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def sensors_reset(self):
        """Clears the active sensor."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def sensors_resetall(self):
        """Clears all sensors in the active circuit."""
        result = self.dss_obj.SensorsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    # SensorsF (Float)
    def sensors_read_pcterror(self):
        """Gets the assumed percent error in the Sensor measurement. Default is 1."""
        result = float(self.dss_obj.SensorsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def sensors_write_pcterror(self, argument):
        """Sets the assumed percent error in the Sensor measurement. Default is 1."""
        result = float(self.dss_obj.SensorsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def sensors_read_weight(self):
        """Gets the weighting factor for this sensor measurement with respect to the other sensors. Default is 1."""
        result = float(self.dss_obj.SensorsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def sensors_write_weight(self, argument):
        """Sets the weighting factor for this sensor measurement with respect to the other sensors. Default is 1."""
        result = float(self.dss_obj.SensorsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def sensors_read_kvbase(self):
        """Gets the voltage base for the sensor measurements. LL for 2 and 3 - phase sensors, LN for 1-phase sensors."""
        result = float(self.dss_obj.SensorsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def sensors_write_kvbase(self, argument):
        """Sets the voltage base for the sensor measurements. LL for 2 and 3 - phase sensors, LN for 1-phase sensors."""
        result = float(self.dss_obj.SensorsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

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
