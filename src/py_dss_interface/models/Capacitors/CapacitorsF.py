# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapacitorsF(Base):
    """
    This interface can be used to read/modify the properties of the Capacitors Class where the values are doubles.

    The structure of the interface is as follows:
        double CapacitorsF(int32_t Parameter, double argument)

    This interface returns a floating point number (64 bits), the first parameter is used to specify the property of
    the class to be used and second parameter can be used to modify the value of the property when necessary. Reading
    and writing properties are separated and require a different parameter number to be executed.

    The properties (parameter) are integer numbers and are described as follows.
    """
    def capacitors_read_kv(self) -> float:
        """Gets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        return self.get_float(0, 0)

    def capacitors_write_kv(self, argument):
        """Sets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        # TODO: what is the the return type?
        argument = Base.check_float_param(argument)
        return self.get_float(1, argument)

    def capacitors_read_kvar(self) -> float:
        """Gets the total bank kvar, distributed equally among phases and steps."""
        return self.get_float(2, 0)

    def capacitors_write_kvar(self, argument):
        """Sets the total bank kvar, distributed equally among phases and steps."""
        # TODO: what is the the return type?
        argument = Base.check_float_param(argument)
        return self.get_float(3, argument)
