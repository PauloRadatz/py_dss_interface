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

    def _name(self) -> str:
        return (self._dss_obj.CapControlsS(0, 0)).decode('ascii')

    def _name_write(self, argument: str) -> str:
        return self._dss_obj.CapControlsS(1, argument.encode('ascii'))

    def _controlled_capacitor(self) -> str:
        return (self._dss_obj.CapControlsS(2, 0)).decode('ascii')

    def _controlled_capacitor_write(self, argument: str) -> str:
        try:
            return self._dss_obj.CapControlsS(3, argument.encode('ascii'))
        except Exception as e:
            print(f'capcontrols_write_capacitor: Check if exist at least one CapControl!. {e}')

    def _monitored_object(self) -> str:
        return (self._dss_obj.CapControlsS(4, 0)).decode('ascii')

    def _monitored_object_write(self, argument: str) -> str:
        try:
            return self._dss_obj.CapControlsS(5, argument.encode('ascii'))
        except Exception as e:
            print(f'capcontrols_write_monitored_obj: Check if exist at least one CapControl!. {e}')
