# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

import ctypes

from py_dss_interface.models.Base import Base


class LoadsS(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are strings.

    The structure of the interface is as follows:
        CStr DSSLoadsS(int32_t Parameter, CStr Argument);

    This interface returns a string, the variable “parameter” (Integer) is used to specify the property of the class
    to be used and the variable “argument” (string) can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.
    """

    def loads_read_name(self) -> str:
        """Allows to read the Name property of the active load. The parameter argument can be filled with an empty
        string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_name(self, argument) -> str:
        """allows to set the active load by specifying the Name load. The parameter argument must contain the Name of
        the load to activate. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_cvr_curve(self) -> str:
        """Allows to read the CVRCurve property of the active load. The parameter argument can be filled with an
        empty string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_cvr_curve(self, argument) -> str:
        """Allows to set the CVRCurve property for the active load. The parameter argument must contain the Name of
        the new CVRCurve to be linked to the active load. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_daily(self) -> str:
        """Allows to read the daily property of the active load. The parameter argument can be filled with an empty
        string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_daily(self, argument) -> str:
        """Allows to set the daily property for the active load. The parameter argument must contain the Name of the
        new daily to be linked to the active load. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_duty(self) -> str:
        """Allows to read the duty property of the active load. The parameter argument can be filled with an empty
        string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TODO include in test
    def loads_write_duty(self, argument) -> str:
        """Allows to set the dduty property for the active load. The parameter argument must contain the Name of the
        new duty to be linked to the active load. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_spectrum(self) -> str:
        """Allows to read the Spectrum property of the active load. The parameter argument can be filled with an
        empty string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_spectrum(self, argument) -> str:
        """Allows to set the Spectrum property for the active load. The parameter argument must contain the Name of
        the new Spectrum to be linked to the active load. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_yearly(self) -> str:
        """Allows to read the Yearly property of the active load. The parameter argument can be filled with an empty
        string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_yearly(self, argument) -> str:
        """Allows to set the Yearly property for the active load. The parameter argument must contain the Name of the
        new Yearly to be linked to the active load. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_growth(self) -> str:
        """Allows to read the Growth property of the active load. The parameter argument can be filled with an empty
        string. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(12), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_growth(self, argument) -> str:
        """Allows to set the Growth property for the active load. The parameter argument must contain the Name of the
        new Growth to be linked to the active load. The return value will be equal to empty. """
        result = ctypes.c_char_p(self.dss_obj.DSSLoadsS(ctypes.c_int32(13), argument.encode('ascii')))
        return result.value.decode('ascii')
