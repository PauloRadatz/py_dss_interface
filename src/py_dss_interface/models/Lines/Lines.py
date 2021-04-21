# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Lines(Base):

    # LinesI (int)
    def lines_first(self):
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dss_obj.LinesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def lines_next(self):
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dss_obj.LinesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def lines_read_phases(self):
        """Gets the number of phases of the active line object."""
        result = self.dss_obj.LinesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def lines_write_phases(self, argument):
        """Sets the number of phases of the active line object."""
        result = self.dss_obj.LinesI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def lines_numcust(self):
        """Gets the number of customers on this line section."""
        result = self.dss_obj.LinesI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def lines_parent(self):
        """Gets the parents of the active Line to be the active Line. Return 0 if no parent or action fails."""
        result = self.dss_obj.LinesI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def lines_count(self):
        """Gets the number of Line Objects in Active Circuit."""
        result = self.dss_obj.LinesI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def lines_read_units(self):
        """Gets the units of the line (distance, check manual for details)."""
        result = self.dss_obj.LinesI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def lines_write_units(self, argument):
        """Sets the units of the line (distance, check manual for details)."""
        result = self.dss_obj.LinesI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    # LinesF (Float)
    def lines_read_length(self):
        """Gets the length of line section in units compatible with the LineCode definition."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def lines_write_length(self, argument):
        """Sets the length of line section in units compatible with the LineCode definition."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def lines_read_r1(self):
        """Gets the positive sequence resistance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def lines_write_r1(self, argument):
        """Sets the positive sequence resistance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def lines_read_x1(self):
        """Gets the positive sequence reactance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def lines_write_x1(self, argument):
        """Sets the positive sequence reactance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def lines_read_r0(self):
        """Gets the zero sequence resistance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def lines_write_r0(self, argument):
        """Sets the zero sequence resistance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def lines_read_x0(self):
        """Gets the zero sequence reactance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def lines_write_x0(self, argument):
        """Sets the zero sequence reactance, ohm per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def lines_read_c1(self):
        """Gets the positive sequence capacitance, nanofarads per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def lines_write_c1(self, argument):
        """Sets the positive sequence capacitance, nanofarads per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def lines_read_c0(self):
        """Gets the zero sequence capacitance, nanofarads per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def lines_write_c0(self, argument):
        """Sets the zero sequence capacitance, nanofarads per unit length."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def lines_read_normamps(self):
        """Gets the normal ampere rating of line section."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def lines_write_normamps(self, argument):
        """Sets the normal ampere rating of Line."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def lines_read_emergamps(self):
        """Gets the emergency (maximum) ampere rating of Line."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def lines_write_emergamps(self, argument):
        """Sets the emergency (maximum) ampere rating of Line."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def lines_read_rg(self):
        """Gets the earth return value used to compute line impedances at power frequency."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def lines_write_rg(self, argument):
        """Sets the earth return value used to compute line impedances at power frequency."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def lines_read_xg(self):
        """Gets the earth return reactance value used to compute line impedances at power frequency."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def lines_write_xg(self, argument):
        """Sets the earth return reactance value used to compute line impedances at power frequency."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def lines_read_rho(self):
        """Gets the earth resistivity, m-ohms."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def lines_write_rho(self, argument):
        """Sets the earth resistivity, m-ohms."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(23), ctypes.c_double(argument)))
        return result

    def lines_read_seasonrating(self):
        """Returns the rating for the current season (in Amps) if the SeasonalRatings option is active."""
        result = float(self.dss_obj.LinesF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    # LinesS (String)
    def lines_read_name(self):
        """Gets the name of the active Line element."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_name(self, argument):
        """Sets the name of the Line element to set it active."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_bus1(self):
        """Gets the name of bus for terminal 1."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_bus1(self, argument):
        """Sets the name of bus for terminal 1."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_bus2(self):
        """Gets the name of bus for terminal 2."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_bus2(self, argument):
        """Sets the name of bus for terminal 2."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_linecode(self):
        """Gets the name of LineCode object that defines the impedances."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_linecode(self, argument):
        """Sets the name of LineCode object that defines the impedances."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_geometry(self):
        """Gets the name of the Line geometry code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_geometry(self, argument):
        """Sets the name of the Line geometry code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_spacing(self):
        """Gets the name of the Line spacing code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_spacing(self, argument):
        """Sets the name of the Line spacing code."""
        result = ctypes.c_char_p(self.dss_obj.LinesS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    # LinesV (Variant)
    def lines_allnames(self):
        """Gets the name of all Line Objects."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_rmatrix(self):
        """Gets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_rmatrix(self, argument):
        """Sets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_xmatrix(self):
        """Gets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_xmatrix(self, argument):
        """Sets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_cmatrix(self):
        """Gets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_cmatrix(self, argument):
        """Sets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_yprim(self):
        """Gets the YPrimitive of the active Line."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LinesV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_yprim(self, argument):
        """Does nothing at present."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LinesV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value
