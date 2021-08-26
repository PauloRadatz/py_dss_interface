# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Base import Base


class CapacitorsI(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are integers.

    The structure of the interface is as follows:
        int32_t CapacitorsI(int32_t Parameter, int32_t argument)

    This interface returns an integer (signed 32 bits), the variable “parameter” is used to specify the property of
    the class to be used and the variable “argument” can be used to modify the value of the property when necessary.
    Reading and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """

    def capacitors_read_num_steps(self) -> int:
        """Gets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        return self.dss_obj.CapacitorsI(0, 0)

    def capacitors_write_num_steps(self, argument: int) -> int:
        """Sets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        argument = Base.check_int_param(argument, default=1)
        return self.dss_obj.CapacitorsI(1, argument)

    def capacitors_read_is_delta(self) -> int:
        """Gets 1 if delta connection, otherwise will return 0 for distributing and switching the total kvar."""
        return self.dss_obj.CapacitorsI(2, 0)

    def capacitors_write_is_delta(self, argument: int = 1) -> int:
        """Sets (Argument) 1 if delta connection, otherwise will return 0 for distributing and switching the total
        kvar. """
        argument = Base.check_int_param(argument, default=1)
        return self.dss_obj.CapacitorsI(3, argument)

    def capacitors_first(self) -> int:
        """Sets the first capacitor active. Returns 0 if no more."""
        return self.dss_obj.CapacitorsI(4, 0)

    def capacitors_next(self) -> int:
        """Sets the next capacitor active. Returns 0 if no more."""
        result = self.dss_obj.CapacitorsI(5, 0)
        return result

    def capacitors_count(self) -> int:
        """Gets the number of capacitor objects in active circuit."""
        return self.dss_obj.CapacitorsI(6, 0)

    def capacitors_add_step(self) -> int:
        """Adds one step of the capacitor if available. If successful returns 1."""
        result = self.dss_obj.CapacitorsI(7, 0)
        Base.check_assertion_result(result, "Capacitor step problem detect!", "A problem occur when tried to adds "
                                                                              "step to a capacitor. Check capacitor/"
                                                                              "bank capacitor existence or available "
                                                                              "steps!", expected_value=1)
        return result

    def capacitors_subtract_step(self) -> int:
        """Subtracts one step of the capacitor if available. If no more steps, returns 0."""
        result = self.dss_obj.CapacitorsI(8, 0)
        Base.check_assertion_result(result, "Capacitor step problem detect!", "A problem occur when tried to subtract "
                                                                              "step to a capacitor. Check capacitor/"
                                                                              "bank capacitor existence or available "                                                                   "steps!", expected_value=1)
        return result

    def capacitors_available_steps(self) -> int:
        """Gets the number of steps available in cap bank to be switched ON."""
        return self.dss_obj.CapacitorsI(9, 0)

    def capacitors_open(self) -> int:
        """Opens all steps, all phases of the capacitor."""
        return self.dss_obj.CapacitorsI(10, 0)

    def capacitors_close(self) -> int:
        """Closes all steps, all phases of the capacitor."""
        return self.dss_obj.CapacitorsI(11, 0)
