# -*- encoding: utf-8 -*-

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class ExportV(Base):
    """Access to the OpenDSS Export interface (ExportV).

    The ExportV DLL function returns, in-memory, the exact CSV text that the
    corresponding OpenDSS ``export`` command would otherwise write to a file.
    The structure of the interface is:

        void ExportV(int32_t mode, VARIANT *Argument);
    """

    def _voltages_ln(self) -> str:
        result = Bridge.pointer_read(self._dss_obj.ExportV, 0)
        return "\n".join(result) if result else ""

    def _voltages_ll(self) -> str:
        result = Bridge.pointer_read(self._dss_obj.ExportV, 1)
        return "\n".join(result) if result else ""

    def _elem_voltages(self) -> str:
        result = Bridge.pointer_read(self._dss_obj.ExportV, 2)
        return "\n".join(result) if result else ""
