# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class CktElementI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t CktElementI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def cktelement_num_terminals(self) -> int:
        """Deliver the number of terminals of the active DSS object."""
        return int(self.dss_obj.CktElementI(ctypes.c_int32(0), ctypes.c_int32(0)))

    def cktelement_num_conductors(self) -> int:
        """Deliver the number of conductors of the active DSS object."""
        return int(self.dss_obj.CktElementI(ctypes.c_int32(1), ctypes.c_int32(0)))

    def cktelement_num_phases(self) -> int:
        """Delivers the number of phases of the active DSS object."""
        return int(self.dss_obj.CktElementI(ctypes.c_int32(2), ctypes.c_int32(0)))

    def cktelement_open(self, argument: int) -> int:
        """Open the specified terminal (Argument) of the active DSS object."""
        return int(self.dss_obj.CktElementI(ctypes.c_int32(3), ctypes.c_int32(argument)))

    def cktelement_close(self, argument: int) -> int:
        """Close the specified terminal (Argument) of the active DSS object."""
        return self.dss_obj.CktElementI(ctypes.c_int32(4), ctypes.c_int32(argument))

    def cktelement_is_open(self) -> int:
        """Return a 1 if any terminal of the active DSS object is open, otherwise, it will return a 0."""
        return self.dss_obj.CktElementI(ctypes.c_int32(5), ctypes.c_int32(0))

    def cktelement_num_properties(self) -> int:
        """Return the number of properties of the active DSS object."""
        return self.dss_obj.CktElementI(ctypes.c_int32(6), ctypes.c_int32(0))

    def cktelement_has_switch_control(self) -> int:
        """Returns 1 if the active DSS object has a Switch Control linked; otherwise, it will return 0."""
        return self.dss_obj.CktElementI(ctypes.c_int32(7), ctypes.c_int32(0))

    def cktelement_has_volt_control(self) -> int:
        """Returns 1 if the active DSS object has a Volt Control linked; otherwise, it will return 0."""
        return self.dss_obj.CktElementI(ctypes.c_int32(8), ctypes.c_int32(0))

    def cktelement_num_controls(self) -> int:
        """Returns number of controls linked to the active DSS object."""
        return self.dss_obj.CktElementI(ctypes.c_int32(9), ctypes.c_int32(0))

    def cktelement_ocp_dev_index(self) -> int:
        """Returns the Index into Controller list of OCP Device controlling the active DSS object."""
        return self.dss_obj.CktElementI(ctypes.c_int32(10), ctypes.c_int32(0))

    def cktelement_ocp_dev_type(self) -> int:
        """Returns one of the following values: 0=none; 1=Fuse; 2=Recloser; 3=Relay according to the type of active
        control."""
        return self.dss_obj.CktElementI(ctypes.c_int32(11), ctypes.c_int32(0))

    def cktelement_read_enabled(self) -> int:
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
        enabled."""
        return self.dss_obj.CktElementI(ctypes.c_int32(12), ctypes.c_int32(0))

    def cktelement_write_enabled(self, argument: int) -> int:
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
        enabled."""
        return self.dss_obj.CktElementI(ctypes.c_int32(13), ctypes.c_int32(argument))
