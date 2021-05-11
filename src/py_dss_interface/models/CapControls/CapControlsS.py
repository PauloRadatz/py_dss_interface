# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapControlsS(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr CapControlsS(int32_t Parameter, CStr Argument);

    This interface returns a string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def cap_controls_read_name(self) -> str:
        """Gets the name of the active CapControl."""
        return self.get_string(0, 0)

    def cap_controls_write_name(self, argument):
        """Sets a CapControl active by name."""
        return self.get_string(1, argument.encode('ascii'))

    def cap_controls_read_capacitor(self) -> str:
        """Gets the name of the capacitor that is controlled."""
        return self.get_string(2, 0)

    def cap_controls_write_capacitor(self, argument):
        """Sets the name of the capacitor that is controlled."""
        return self.get_string(3, argument.encode('ascii'))

    def cap_controls_read_monitored_obj(self) -> str:
        """Gets the full name of the element that PT and CT are connected to."""
        return self.get_string(4, 0)

    def cap_controls_write_monitored_obj(self, argument):
        """Sets the full name of the element that PT and CT are connected to."""
        return self.get_string(5, argument.encode('ascii'))
