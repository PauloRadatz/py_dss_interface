# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapControlsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t CapControlsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following
    """

    def capcontrols_first(self) -> int:
        """Sets the first CapControl active. Returns 0 if no more."""
        return self.dss_obj.CapControlsI(0, 0)

    def capcontrols_next(self) -> int:
        """Sets the next CapControl active. Returns 0 if no more."""
        return self.dss_obj.CapControlsI(1, 0)

    def capcontrols_read_mode(self) -> int:
        """Gets the type of automatic controller (see manual for details).
        CURRENTCONTROL: Result := 0;
        VOLTAGECONTROL: Result := 1;
        VARCONTROL: Result := 2;
        TIMECONTROL: Result := 3;
        PFCONTROL: Result := 4;
        USERCONTROL: Result := 4;
        """
        return self.dss_obj.CapControlsI(2, 0)

    def capcontrols_write_mode(self, argument: int) -> int:
        """Sets the type of automatic controller (see manual for details).
        0: elem.CapControlType := CURRENTCONTROL;
        1: elem.CapControlType := VOLTAGECONTROL;
        2: elem.CapControlType := KVARCONTROL;
        3: elem.CapControlType := TIMECONTROL;
        4: elem.CapControlType := PFCONTROL;
        """
        return self.dss_obj.CapControlsI(3, argument)

    def capcontrols_read_monitored_term(self) -> int:
        """Gets the terminal number on the element that PT and CT are connected to."""
        return self.dss_obj.CapControlsI(4, 0)

    def capcontrols_write_monitored_term(self, dss, argument: int) -> int:
        """Sets the terminal number on the element that PT and CT are connected to. There is not a explicit return
        type in the oficial documentation, because of this we choose not put a explicit return too. """
        return (
            dss.text(
                f'edit CapControl.{dss.capcontrols_read_name()} Terminal={argument}'
            )
            if self.capcontrols_count() != 0
            else 0
        )

    # TODO
    def capcontrols_read_use_volt_override(self) -> int:
        """Gets if Vmin and Vmax are enabled to override the control Mode. There is not a explicit return type in the
        oficial documentation, because of this we choose not put a explicit return too."
        return self.dss_obj.CapControlsI(6, 0). """

    def capcontrols_write_use_volt_override(self, dss, argument: int) -> int:
        """Sets if enables Vmin and Vmax to override the control Mode. There is not a explicit return type in the
        oficial documentation, because of this we choose not put a explicit return too. """
        return (
            dss.text(
                f'edit CapControl.{dss.capcontrols_read_name()} VoltOverride={argument}'
            )
            if self.capcontrols_count() != 0
            else 0
        )

    def capcontrols_count(self) -> int:
        """Gets the number of CapControls in Active Circuit."""
        return self.dss_obj.CapControlsI(8, 0)
