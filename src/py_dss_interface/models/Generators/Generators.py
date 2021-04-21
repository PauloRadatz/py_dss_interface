# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Generators(Base):

    # GeneratorsI (int)
    def generators_first(self):
        """Sets first generator to be active. Returns 0 if None."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def generators_next(self):
        """Sets next generator to be active. Returns 0 if None."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def generators_read_forcedon(self):
        """Returns 1 if the generator is forced ON regardless of other dispatch criteria; otherwise, returns 0."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def generators_write_forcedon(self, argument):
        """Allows to force ON regardless of other dispatch criteria. To force ON put 1 in the argument, otherwise put 0."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def generators_read_phases(self):
        """Returns the number of phases of the active generator."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def generators_write_phases(self, argument):
        """Sets the number of phases (argument) of the active generator."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def generators_count(self):
        """Returns the number of generators Objects in Active Circuit."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def generators_read_idx(self):
        """Gets the active generator by Index into generators list. 1..Count."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def generators_write_idx(self, argument):
        """Sets the active generator (argument) by Index into generators list. 1..Count."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def generators_read_model(self):
        """Gets the active generator Model (see Manual for details)."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def generators_write_model(self, argument):
        """Sets the active generator Model (see Manual for details)."""
        result = self.dss_obj.GeneratorsI(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    # GeneratorsF (Float)
    def generators_read_kv(self):
        """Gets the voltage base for the active generator, kV."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def generators_write_kv(self, argument):
        """Sets the voltage base for the active generator, kV."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def generators_read_kw(self):
        """Gets the kW output for the active generator, kvar is updated for current power factor."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def generators_write_kw(self, argument):
        """Sets the kW output for the active generator, kvar is updated for current power factor."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def generators_read_kvar(self):
        """Gets the kvar output for the active generator, kW is updated for current power factor."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def generators_write_kvar(self, argument):
        """Sets the kvar output for the active generator, kW is updated for current power factor."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def generators_read_pf(self):
        """Gets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def generators_write_pf(self, argument):
        """Sets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def generators_read_kvarated(self):
        """Gets the KVA rating of the generator."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def generators_write_kvarated(self, argument):
        """Sets the KVA rating of the generator."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def generators_read_vmaxpu(self):
        """Gets the Vmaxpu for Generator Model."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def generators_write_vmaxpu(self, argument):
        """Sets the Vmaxpu for Generator Model."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def generators_read_vminpu(self):
        """Gets the Vminpu for Generator Model."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def generators_write_vminpu(self, argument):
        """Sets the Vminpu for Generator Model."""
        result = float(self.dss_obj.GeneratorsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    # GeneratorsS (String)
    def generators_read_name(self):
        """Gets the name of the active Generator."""
        result = ctypes.c_char_p(self.dss_obj.GeneratorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def generators_write_name(self, argument):
        """Sets the name of the active Generator."""
        result = ctypes.c_char_p(self.dss_obj.GeneratorsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # GeneratorsV (Variant)
    def generators_allnames(self):
        """Gets the array of names of all Generator objects."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.GeneratorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def generators_registernames(self):
        """Gets the array of names of all generator Energy Meter registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.GeneratorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def generators_registervalues(self):
        """Gets the array of values in generator Energy Meter registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.GeneratorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
