# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from py_dss_interface.models.Base import Base


class BusF(Base):
    def bus_float(self, first, second):
        return float(self.dss_obj.BUSF(ctypes.c_int32(first), ctypes.c_double(second)))

    def kv_base(self):
        """Returns the base voltage at bus in kV."""
        return self.bus_float(0, 0)

    def read_x(self):
        """Returns the X coordinate for the bus."""
        return self.bus_float(1, 0)

    def write_x(self, argument):
        """Allows to write the X coordinate for the bus. Returns 0."""
        return self.bus_float(2, 0)

    def read_y(self):
        """Returns the X coordinate for the bus."""
        return self.bus_float(3, 0)

    def write_y(self, argument):
        """Allows to write the Y coordinate for the bus. Returns 0."""
        return self.bus_float(4, 0)

    def distance(self):
        """Returns the distance from the energymeter (if non-zero)."""
        return self.bus_float(5, 0)

    # TODO: rever nome do método
    def bus_lambda(self):
        """Returns the accumulated failure rate downstream from this bus; faults per year."""
        return self.bus_float(6, 0)

    def interruptions(self):
        """Returns the number of interruptions this bus per year."""
        return self.bus_float(7, 0)

    def interruption_duration(self):
        """Returns the average interruption duration in hours."""
        return self.bus_float(8, 0)

    def total_customers_interruptions(self):
        """Returns the annual number of customer interruptions from this bus."""
        return self.bus_float(9, 0)

    def customer_accum_duration(self):
        """Returns the accumulated customer outage durations."""
        return self.bus_float(10, 0)

    def total_miles(self):
        """Returns the total length of line downline from this bus, in miles. For recloser siting algorithm."""
        return self.bus_float(11, 0)
