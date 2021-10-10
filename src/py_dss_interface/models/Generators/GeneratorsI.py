# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class GeneratorsI(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t GeneratorsI(int32_t Parameter, int32_t Argument);

    This interface returns an integer with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def generators_first(self) -> int:
        """Sets first generator to be active. Returns 0 if None."""
        return self.dss_obj.GeneratorsI(ctypes.c_int32(0), ctypes.c_int32(0))

    def generators_next(self) -> int:
        """Sets next generator to be active. Returns 0 if None."""
        return self.dss_obj.GeneratorsI(ctypes.c_int32(1), ctypes.c_int32(0))

    def generators_read_forced_on(self) -> int:
        """Returns 1 if the generator is forced ON regardless of other dispatch criteria; otherwise, returns 0."""
        return self.dss_obj.GeneratorsI(ctypes.c_int32(2), ctypes.c_int32(0))

    def generators_write_forced_on(self, argument: int) -> int:
        """Allows to force ON regardless of other dispatch criteria. To force ON put 1 in the argument, otherwise put
        0. """
        return self.dss_obj.GeneratorsI(ctypes.c_int32(3), ctypes.c_int32(argument))

    def generators_read_phases(self) -> int:
        """Returns the number of phases of the active generator."""
        return self.dss_obj.GeneratorsI(ctypes.c_int32(4), ctypes.c_int32(0))

    def generators_write_phases(self, argument: int) -> int:
        """Sets the number of phases (argument) of the active generator."""
        argument = Base.check_int_param(argument, 1)  # Phase 1 as default
        return self.dss_obj.GeneratorsI(ctypes.c_int32(5), ctypes.c_int32(argument))

    def generators_count(self) -> int:
        """Returns the number of generators Objects in Active Circuit."""
        return self.dss_obj.GeneratorsI(ctypes.c_int32(6), ctypes.c_int32(0))

    def generators_read_idx(self) -> int:
        """Gets the active generator by Index into generators list. 1..Count."""
        return self.dss_obj.GeneratorsI(ctypes.c_int32(7), ctypes.c_int32(0))

    def generators_write_idx(self, argument: int) -> int:
        """Sets the active generator (argument) by Index into generators list. 1..Count."""
        argument = Base.check_int_param(argument, 1)
        return self.dss_obj.GeneratorsI(ctypes.c_int32(8), ctypes.c_int32(argument))

    def generators_read_model(self) -> int:
        """Gets the active generator Model (see Manual for details).
        1:Generator injects a constant kW at specified power factor.
        2:Generator is modeled as a constant admittance.
        3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.
        4:Const kW, Fixed Q (Q never varies)
        5:Const kW, Fixed Q(as a constant reactance)
        6:Compute load injection from User-written Model.(see usage of Xd, Xdp)
        7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter. See also Balanced.
        """
        return self.dss_obj.GeneratorsI(ctypes.c_int32(9), ctypes.c_int32(0))

    def generators_write_model(self, argument: int) -> int:
        """Sets the active generator Model (see Manual for details).
        1:Generator injects a constant kW at specified power factor.
        2:Generator is modeled as a constant admittance.
        3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.
        4:Const kW, Fixed Q (Q never varies)
        5:Const kW, Fixed Q(as a constant reactance)
        6:Compute load injection from User-written Model.(see usage of Xd, Xdp)
        7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter. See also Balanced.
        """
        return self.dss_obj.GeneratorsI(ctypes.c_int32(10), ctypes.c_int32(argument))
