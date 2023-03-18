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

    def _first(self) -> int:
        return self._dss_obj.CapControlsI(0, 0)

    def _next(self) -> int:
        return self._dss_obj.CapControlsI(1, 0)

    def _mode(self) -> int:
        return self._dss_obj.CapControlsI(2, 0)

    def _mode_write(self, argument: int) -> int:
        return self._dss_obj.CapControlsI(3, argument)

    def _monitored_term(self) -> int:
        return self._dss_obj.CapControlsI(4, 0)

    def _monitored_term_write(self, dss, argument: int) -> int:
        return (
            dss.text(
                f'edit CapControl.{dss.capcontrols.name} Terminal={argument}'
            )
            if self._count() != 0
            else 0
        )

    # TODO
    def _use_volt_override(self) -> int:
        """return self.dss_obj.CapControlsI(6, 0). """

    def _use_volt_override_write(self, dss, argument: int) -> int:
        return (
            dss.text(
                f'edit CapControl.{dss.capcontrols.name} VoltOverride={argument}'
            )
            if self._count() != 0
            else 0
        )

    def _count(self) -> int:
        return self._dss_obj.CapControlsI(8, 0)
