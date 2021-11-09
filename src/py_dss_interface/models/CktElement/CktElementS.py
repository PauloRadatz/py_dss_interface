# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CktElementS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr CktElementS(int32_t Parameter, CStr Argument);

    This interface returns a string (pAnsiChar) with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def cktelement_name(self) -> str:
        """Delivers the full name of the active circuit element."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(0, 0))
        return result.value.decode('ascii')

    def cktelement_read_display(self) -> str:
        """Displays the name of the active circuit element (not necessarily unique)."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_write_display(self, argument: str) -> str:
        """Allows to modify the name of the active circuit element (not necessarily unique)."""
        argument = Base.check_string_param(argument)
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    # TODO include in test
    def cktelement_guid(self) -> str:
        """Delivers the unique name for the active circuit element."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_energymeter(self) -> str:
        """Delivers the name of the EnergyMeter linked to the active circuit element."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_controller(self, argument: str) -> str:
        """Delivers the Full name of the i-th controller attached to the active circuit element.
        The i-th controller index must be specified in the argument arg. Ex: Str = Controller(2).
        See NumControls to determine valid index range."""
        # argument = Base.check_string_param(argument)
        # argument = ctypes.c_char_p(argument.encode('utf-8'))
        result = self.dss_obj.CktElementS(5, argument)
        return result.decode('ascii')

        # try:
        #     result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(5), ctypes.(argument)))
        #     result = result.value.decode('ascii')
        # except Exception as e:
        #     result = Base.warn_msg("Check if exist at least one *Controller* in your circuit", e)
        # return result
