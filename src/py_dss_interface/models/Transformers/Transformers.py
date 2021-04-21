# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Transformers(Base):
    def transformers_read_numwindings(self):
        """Gets the number of windings on this transformer. Allocates memory; set or change this property first."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def transformers_write_numwindings(self, argument):
        """Sets the number of windings on this transformer. Allocates memory; set or change this property first."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(1), ctypes.c_int32(argument))
        return result

    def transformers_read_wdg(self):
        """Gets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def transformers_write_wdg(self, argument):
        """Sets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def transformers_read_numtaps(self):
        """Gets the active winding number of tap steps between MinTap and MaxTap."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def transformers_write_numtaps(self, argument):
        """Sets the active winding number of tap steps between MinTap and MaxTap."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def transformers_read_isdelta(self):
        """Gets the information about if the active winding is delta (1) or wye (0) connection."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def transformers_write_isdelta(self, argument):
        """Sets the information about if the active winding is delta (1) or wye (0) connection."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def transformers_first(self):
        """Sets the first Transformer active. Return 0 if no more."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def transformers_next(self):
        """Sets the next Transformer active. Return 0 if no more."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def transformers_count(self):
        """Gets the number of transformers within the active circuit."""
        result = self.dss_obj.TransformersI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

        # TransformersF (Float)

    def transformers_read_r(self):
        """Gets the active winding resistance in %."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def transformers_write_r(self, argument):
        """Sets the active winding resistance in %."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def transformers_read_tap(self):
        """Gets the active winding tap in per-unit."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def transformers_write_tap(self, argument):
        """Sets the active winding tap in per-unit."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def transformers_read_mintap(self):
        """Gets the active winding minimum tap in per-unit."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def transformers_write_mintap(self, argument):
        """Sets the active winding minimum tap in per-unit."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def transformers_read_maxtap(self):
        """Gets the active winding maximum tap in per-unit."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def transformers_write_maxtap(self, argument):
        """Sets the active winding maximum tap in per-unit."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def transformers_read_kv(self):
        """Gets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def transformers_write_kv(self, argument):
        """Sets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def transformers_read_kva(self):
        """Gets the active winding kVA rating. On winding 1, this also determines normal and
        emergency current ratings for all windings."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def transformers_write_kva(self, argument):
        """Sets the active winding kVA rating. On winding 1, this also determines normal and
        emergency current ratings for all windings."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def transformers_read_xneut(self):
        """Gets the active winding neutral reactance [ohms] for wye connections."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def transformers_write_xneut(self, argument):
        """Sets the active winding neutral reactance [ohms] for wye connections."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def transformers_read_rneut(self):
        """Gets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def transformers_write_rneut(self, argument):
        """Sets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def transformers_read_xhl(self):
        """Gets the percent reactance between windings 1 and 2, on winding 1 kVA base.
        Use for 2 winding or 3 winding transformers."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def transformers_write_xhl(self, argument):
        """Sets the percent reactance between windings 1 and 2, on winding 1 kVA base.
        Use for 2 winding or 3 winding transformers."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def transformers_read_xht(self):
        """Gets the percent reactance between windings 1 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def transformers_write_xht(self, argument):
        """Sets the percent reactance between windings 1 and 3, on winding 1 kVA base.
        Use for 3 winding transformers only."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def transformers_read_xlt(self):
        """Gets he percent reactance between windings 2 and 3, on winding 1 kVA base.
         Use for 3 winding transformers only."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def transformers_write_xlt(self, argument):
        """Sets the percent reactance between windings 2 and 3, on winding 1 kVA base.
        Use for 3 winding transformers only."""
        result = float(self.dss_obj.TransformersF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

        # TransformersS (String)

    def transformers_read_xfmrcode(self):
        """Gets the name of an XfrmCode that supplies electrical parameters for this transformer."""
        result = ctypes.c_char_p(self.dss_obj.TransformersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def transformers_write_xfmrcode(self, argument):
        """Sets the name of an XfrmCode that supplies electrical parameters for this transformer."""
        result = ctypes.c_char_p(self.dss_obj.TransformersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def transformers_read_name(self):
        """Gets the active transformer name."""
        result = ctypes.c_char_p(self.dss_obj.TransformersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def transformers_write_name(self, argument):
        """Sets the active transformer by name."""
        result = ctypes.c_char_p(self.dss_obj.TransformersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def transformers_strwdgvoltages(self):
        """Gets the voltages at the active winding of the active transformer in string format."""
        result = ctypes.c_char_p(self.dss_obj.TransformersS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TransformersV (Variant)

    def transformers_allNames(self):
        """Gets a variant array of strings with all Transformer names in the active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TransformersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def transformers_wdgvoltages(self):
        """Gets a variant array of doubles containing the voltages at the active winding on the active transformer.
        These voltages come as complex pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TransformersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def transformers_wdgcurrents(self):
        """Gets a a variant array of doubles containing the currents at the active winding on the active transformer.
        These currents come as complex pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TransformersV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
