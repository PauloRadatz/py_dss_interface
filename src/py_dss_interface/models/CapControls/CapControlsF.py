# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CapControlsF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double CapControlsF(int32_t Parameter, double Argument);

    This interface returns a floating point number (64 bits) with the result of the query according to the value of
    the variable Parameter, which can be one of the following.
    """

    def _ct_ratio(self) -> float:
        return self._dss_obj.CapControlsF(0, 0)

    def _ct_ratio_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(1, argument)
        except Exception as e:
            print(f'capcontrols_write_ct_ratio: Check if exist at least one CapControl!. {e}')

    def _pt_ratio(self) -> float:
        return self._dss_obj.CapControlsF(2, 0)

    def _pt_ratio_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(3, argument)
        except Exception as e:
            print(f'capcontrols_write_pt_ratio: Check if exist at least one CapControl!. {e}')

    def _on_setting(self) -> float:
        return self._dss_obj.CapControlsF(4, 0)

    def _on_setting_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(5, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_on_setting: Check if exist at least one CapControl!. {e}')

    def _off_setting(self) -> float:
        return self._dss_obj.CapControlsF(6, 0)

    def _off_setting_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(7, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_off_setting: Check if exist at least one CapControl!. {e}')

    def _vmax(self) -> float:
        return self._dss_obj.CapControlsF(8, 0)

    def _vmax_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(9, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_vmax: Check if exist at least one CapControl!. {e}')

    def _vmin(self) -> float:
        return self._dss_obj.CapControlsF(10, 0)

    def _vmin_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(11, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_vmin: Check if exist at least one CapControl!. {e}')

    def _delay(self) -> float:
        return self._dss_obj.CapControlsF(12, 0)

    def _delay_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(13, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_delay: Check if exist at least one CapControl!. {e}')

    def _delay_off(self) -> float:
        return self._dss_obj.CapControlsF(14, 0)

    def _delay_off_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(15, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_delay_off: Check if exist at least one CapControl!. {e}')

    def _dead_time(self) -> float:
        return self._dss_obj.CapControlsF(16, 0)

    def _dead_time_write(self, argument: float) -> float:
        try:
            return self._dss_obj.CapControlsF(17, ctypes.c_double(argument))
        except Exception as e:
            print(f'capcontrols_write_dead_time: Check if exist at least one CapControl!. {e}')
