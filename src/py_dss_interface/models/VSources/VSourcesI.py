# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class VSources(Base):
    """
    This interface implements the Vsources (IVSources) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    # VSourcesI (int)
    def vsources_count(self):
        """Returns the number of VSource objects currently defined in the active circuit."""
        result = self.dss_obj.VsourcesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def vsources_first(self):
        """Sets the first VSource to be active; returns 0 if none."""
        result = self.dss_obj.VsourcesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def vsources_next(self):
        """Sets the next VSource to be active; returns 0 if none."""
        result = self.dss_obj.VsourcesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def vsources_read_phases(self):
        """Gets the number of phases of the active VSource."""
        result = self.dss_obj.VsourcesI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def vsources_write_phases(self, argument):
        """Sets the number of phases of the active VSource."""
        result = self.dss_obj.VsourcesI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    # VSourcesF (Float)
    def vsources_read_basekv(self):
        """Gets the source voltage in kV."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def vsources_write_basekv(self, argument):
        """Sets the source voltage in kV."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def vsources_read_pu(self):
        """Gets the source voltage in pu."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def vsources_write_pu(self, argument):
        """Sets the source voltage in pu."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def vsources_read_angledeg(self):
        """Gets the source phase angle of first phase in degrees."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def vsources_write_angledeg(self, argument):
        """Sets the source phase angle of first phase in degrees."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def vsources_read_frequency(self):
        """Gets the source frequency in Hz."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def vsources_write_frequency(self, argument):
        """Sets the source frequency in Hz."""
        result = float(self.dss_obj.VsourcesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    # VSourcesS (String)
    def vsources_read_name(self):
        """Gets the name of the active VSource."""
        result = ctypes.c_char_p(self.dss_obj.VsourcesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def vsources_write_name(self, argument):
        """Sets the name of the active VSource."""
        result = ctypes.c_char_p(self.dss_obj.VsourcesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # VSourcesV (Variant)
    def vsources_allnames(self):
        """Gets the name of the active VSource."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.VsourcesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value
