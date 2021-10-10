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

    def capcontrols_read_name(self) -> str:
        """Gets the name of the active CapControl."""
        return (self.dss_obj.CapControlsS(0, 0)).decode('ascii')

    def capcontrols_write_name(self, argument: str) -> str:
        """Sets a CapControl active by name."""
        return self.dss_obj.CapControlsS(1, argument.encode('ascii'))

    def capcontrols_read_capacitor(self) -> str:
        """Gets the name of the capacitor that is controlled."""
        return (self.dss_obj.CapControlsS(2, 0)).decode('ascii')

    def capcontrols_write_capacitor(self, argument: str) -> str:
        """Sets the name of the capacitor that is controlled."""
        try:
            return self.dss_obj.CapControlsS(3, argument.encode('ascii'))
        except Exception as e:
            print(f'capcontrols_write_capacitor: Check if exist at least one CapControl!. {e}')

    def capcontrols_read_monitored_obj(self) -> str:
        """Gets the full name of the element that PT and CT are connected to."""
        return (self.dss_obj.CapControlsS(4, 0)).decode('ascii')

    def capcontrols_write_monitored_obj(self, argument: str) -> str:
        """Sets the full name of the element that PT and CT are connected to."""
        try:
            return self.dss_obj.CapControlsS(5, argument.encode('ascii'))
        except Exception as e:
            print(f'capcontrols_write_monitored_obj: Check if exist at least one CapControl!. {e}')
