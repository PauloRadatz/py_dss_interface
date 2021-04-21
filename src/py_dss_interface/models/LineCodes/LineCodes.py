# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class LineCodes(Base):
    # LineCodesI (int)
    def linecodes_count(self):
        """Gets the number of Line Objects in Active Circuit."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def linecodes_first(self):
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def linecodes_next(self):
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def linecodes_read_units(self):
        """Delivers the units of the active LineCode as an integer."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def linecodes_write_units(self, argument):
        """Sets the units of the active LineCode. The units must be specified as an integer in the argument.
        Please refer to the OpenDSS User manual for more information."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def linecodes_read_phasess(self):
        """Delivers the number of phases of the active LineCode as an integer."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def linecodes_write_phasess(self, argument):
        """Sets the number of phases of the active LineCode. The units must be specified as an integer in the argument."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def linecodes_isz1z0(self):
        """Gets the flag (Boolean 1/0) denoting whether the impedance data were entered in symmetrical components."""
        result = self.dss_obj.LineCodesI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    # LineCodesF (Float)
    def linecodes_read_r1(self):
        """Gets the Positive-sequence resistance in ohms per unit length for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def linecodes_write_r1(self, argument):
        """Sets the Positive-sequence resistance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def linecodes_read_x1(self):
        """Gets the Positive-sequence reactance in ohms per unit length for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def linecodes_write_x1(self, argument):
        """Sets the Positive-sequence reactance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def linecodes_read_r0(self):
        """Gets the Zero-sequence resistance in ohms per unit length for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def linecodes_write_r0(self, argument):
        """Sets the Zero-sequence resistance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def linecodes_read_x0(self):
        """Gets the Zero-sequence reactance in ohms per unit length for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def linecodes_write_x0(self, argument):
        """Sets the Zero-sequence reactance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def linecodes_read_c1(self):
        """Gets the Positive-sequence capacitance in ohms per unit length for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def linecodes_write_c1(self, argument):
        """Sets the Positive-sequence capacitance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def linecodes_read_c0(self):
        """Gets the Zero-sequence capacitance in ohms per unit length for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def linecodes_write_c0(self, argument):
        """Sets the Zero-sequence capacitance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def linecodes_read_normamps(self):
        """Gets the normal ampere rating for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def linecodes_write_normamps(self, argument):
        """Sets the normal ampere rating for the active LineCode. This value must be specified in the argument
        as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def linecodes_read_emergamps(self):
        """Gets the Emergency ampere rating for the active LineCode."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def linecodes_write_emergamps(self, argument):
        """Sets the Emergency ampere rating for the active LineCode. This value must be specified in the argument
        as a double."""
        result = float(self.dss_obj.LineCodesF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    # LineCodesS (String)
    def linecodes_read_name(self):
        """Gets the name of the active LineCode element."""
        result = ctypes.c_char_p(self.dss_obj.LineCodesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def linecodes_write_name(self, argument):
        """Sets the name of the active LineCode element. The new value must be specified in the argument as a string."""
        result = ctypes.c_char_p(self.dss_obj.LineCodesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # LineCodesV (Variant)
    def linecodes_read_rmatrix(self):
        """Gets the resistance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_rmatrix(self, argument):
        """Sets the resistance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LineCodesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_read_xmatrix(self):
        """Gets the reactance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_xmatrix(self, argument):
        """Sets the reactance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LineCodesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_read_cmatrix(self):
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_cmatrix(self, argument):
        """Sets the capacitance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LineCodesV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_allnames(self):
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LineCodesV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value
