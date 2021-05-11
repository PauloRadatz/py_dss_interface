# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class CktElementF(Base):

    def cktelement_read_norm_amps(self):
        """Deliver the normal ampere rating for the active PDElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def cktelement_write_norm_amps(self, argument):
        """Allows to fix the normal ampere rating for the active PDElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def cktelement_read_emerg_amps(self):
        """Deliver the Emergency ampere rating for the active PDElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def cktelement_write_emerg_amp(self, argument):
        """Allows to fix the Emergency ampere rating for the active PDElement. The new value must be defined in the
        variable ?Argument?."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def cktelement_variable_i(self):
        """Delivers get the value of a variable by index for the active PCElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result
