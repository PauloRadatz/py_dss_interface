# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class RegControls(Base):
    # RegControlsI (int)
    def regcontrols_first(self):
        """Sets the first RegControl active. Returns 0 if no more."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def regcontrols_next(self):
        """Sets the next RegControl active. Returns 0 if no more"""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def regcontrols_read_tapwinding(self):
        """Gets the tapped winding number."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def regcontrols_write_tapwinding(self, argument):
        """Sets the tapped winding number."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def regcontrols_read_winding(self):
        """Gets the winding number for PT and CT connections."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def regcontrols_write_winding(self, argument):
        """Sets the winding number for PT and CT connections."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def regcontrols_read_isreversible(self):
        """Gets the setting in the reverse direction, usually not applicable to substation transformers."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def regcontrols_write_isreversible(self, argument):
        """Sets the different settings for the reverse direction (see Manual for details),
        usually not applicable to substation transformers."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def regcontrols_read_isinversetime(self):
        """Gets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def regcontrols_write_isinversetime(self, argument):
        """Sets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(9), ctypes.c_int32(argument))
        return result

    def regcontrols_read_maxtapchange(self):
        """Gets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def regcontrols_write_maxtapchange(self, argument):
        """Sets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(11), ctypes.c_int32(argument))
        return result

    def regcontrols_count(self):
        """Gets the number of RegControl objects in Active Circuit."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def regcontrols_read_tapnumber(self):
        """Gets the actual tap number of the active RegControl."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def regcontrols_write_tapnumber(self, argument):
        """Sets the actual tap number of the active RegControl."""
        result = self.dss_obj.RegControlsI(ctypes.c_int32(14), ctypes.c_int32(argument))
        return result

    # RegControlsF (Float)
    def regcontrols_read_ctprimary(self):
        """Gets the CT primary ampere rating (secondary is 0.2 amperes)."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def regcontrols_write_ctprimary(self, argument):
        """Sets the CT primary ampere rating (secondary is 0.2 amperes)."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def regcontrols_read_ptratio(self):
        """Gets the PT ratio for voltage control settings."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def regcontrols_write_ptratio(self, argument):
        """Sets the PT ratio for voltage control settings."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardr(self):
        """Gets the LDC R settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardr(self, argument):
        """Sets the LDC R settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardx(self):
        """Gets the LDC X settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardx(self, argument):
        """Sets sets the LDC X settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reverser(self):
        """Gets the reverse LDC R settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def regcontrols_write_reverser(self, argument):
        """Sets the reverse LDC R settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reverserx(self):
        """Gets the reverse LDC X settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def regcontrols_write_reverserx(self, argument):
        """Sets the reverse LDC X settings in Volts."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def regcontrols_read_delay(self):
        """Gets the time delay [s] after arming before the first tap change.
        Control may reset before actually changing taps."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def regcontrols_write_delay(self, argument):
        """Sets the time delay [s] after arming before the first tap change.
        Control may reset before actually changing taps."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def regcontrols_read_tapdelay(self):
        """Gets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
        taps."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def regcontrols_write_tapdelay(self, argument):
        """Sets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
        taps."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def regcontrols_read_voltagelimit(self):
        """Gets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def regcontrols_write_voltagelimit(self, argument):
        """Sets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardband(self):
        """Gets the regulation bandwidth in forward direction, centered on Vreg."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardband(self, argument):
        """Sets the regulation bandwidth in forward direction, centered on Vreg."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardvreg(self):
        """Gets the target voltage in the forward direction, on PT secondary base."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardvreg(self, argument):
        """Sets the target voltage in the forward direction, on PT secondary base."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reverseband(self):
        """Gets the bandwidth in reverse direction, centered on reverse Vreg."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def regcontrols_write_reverseband(self, argument):
        """Sets the bandwidth in reverse direction, centered on reverse Vreg."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(23), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reversevreg(self):
        """Gets the target voltage in the reverse direction, on PT secondary base."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    def regcontrols_write_reversevreg(self, argument):
        """Sets the target voltage in the reverse direction, on PT secondary base."""
        result = float(self.dss_obj.RegControlsF(ctypes.c_int32(25), ctypes.c_double(argument)))
        return result

    # RegControlsS (String)
    def regcontrols_read_name(self):
        """Gets the active RegControl name."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_name(self, argument):
        """Sets the active RegControl name."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_monitoredbus(self):
        """Gets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_monitoredbus(self, argument):
        """Sets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_transformer(self):
        """Gets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_transformer(self, argument):
        """Sets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dss_obj.RegControlsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # RegControlsV (Variant)
    def regcontrols_allnames(self):
        """Gets a variant array of strings containing all RegControl names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.RegControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
