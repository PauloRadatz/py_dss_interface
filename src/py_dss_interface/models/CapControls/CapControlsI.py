# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base
from ... import DSSDLL

class CapControlsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t CapControlsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following
    """

    def cap_controls_first(self) -> int:
        """Sets the first CapControl active. Returns 0 if no more."""
        return self.dss_obj.CapControlsI(0, 0)

    def cap_controls_next(self) -> int:
        """Sets the next CapControl active. Returns 0 if no more."""
        return self.dss_obj.CapControlsI(1, 0)

    def cap_controls_read_mode(self) -> int:
        """Gets the type of automatic controller (see manual for details)."""
        # TODO: what is the return type?
        return self.dss_obj.CapControlsI(2, 0)

    def cap_controls_write_mode(self, argument: int) -> int:
        """Sets the type of automatic controller (see manual for details).
        0: elem.CapControlType := CURRENTCONTROL;
        1: elem.CapControlType := VOLTAGECONTROL;
        2: elem.CapControlType := KVARCONTROL;
        3: elem.CapControlType := TIMECONTROL;
        4: elem.CapControlType := PFCONTROL;
        """
        # TODO: what is the return type?
        return self.dss_obj.CapControlsI(3, argument)

    def cap_controls_read_monitored_term(self) -> int:
        """Gets the terminal number on the element that PT and CT are connected to."""
        return self.dss_obj.CapControlsI(4, 0)

    def cap_controls_write_monitored_term(self, dss: DSSDLL, argument: int) -> int:
        """Sets the terminal number on the element that PT and CT are connected to."""
        # TODO: what is the return type and values?
        # self.dss_obj.CapControlsI(5, argument)
        result = 0
        if not dss.cap_controls_count() == 0:
            result = dss.text(f'CapControls.{dss.cap_controls_read_name()} Terminal={argument}')
        return result

    def cap_controls_read_use_volt_override(self) -> int:
        """Gets if Vmin and Vmax are enabled to override the control Mode."""
        # TODO: what is the return type and values?
        return self.dss_obj.CapControlsI(6, 0)

    def cap_controls_write_use_volt_override(self, dss: DSSDLL, argument: int) -> int:
        """Sets if enables Vmin and Vmax to override the control Mode."""
        # TODO: what is the return type?
        # return self.dss_obj.CapControlsI(7, argument)
        result = 0
        if not dss.cap_controls_count() == 0:
            result = dss.text(f'CapControls.{dss.cap_controls_read_name()} VoltOverride={argument}')
        return result

    def cap_controls_count(self) -> int:
        """Gets the number of CapControls in Active Circuit."""
        return self.dss_obj.CapControlsI(8, 0)
