# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class BusF(Base):

    def bus_kv_base(self):
        """Returns the base voltage at bus in kV."""
        return self.get_float(0, 0)

    def bus_read_x(self):
        """Returns the X coordinate for the bus."""
        return self.get_float(1, 0)

    def bus_write_x(self, argument):
        """Allows to write the X coordinate for the bus. Returns 0."""
        return self.get_float(2, 0)

    def bus_read_y(self):
        """Returns the X coordinate for the bus."""
        return self.get_float(3, 0)

    def bus_write_y(self, argument):
        """Allows to write the Y coordinate for the bus. Returns 0."""
        return self.get_float(4, 0)

    def bus_distance(self):
        """Returns the distance from the energymeter (if non-zero)."""
        return self.get_float(5, 0)

    # TODO: rever nome do método
    def bus_bus_lambda(self):
        """Returns the accumulated failure rate downstream from this bus; faults per year."""
        return self.get_float(6, 0)

    def bus_interruptions(self):
        """Returns the number of interruptions this bus per year."""
        return self.get_float(7, 0)

    def bus_interruption_duration(self):
        """Returns the average interruption duration in hours."""
        return self.get_float(8, 0)

    def bus_total_customers_interruptions(self):
        """Returns the annual number of customer interruptions from this bus."""
        return self.get_float(9, 0)

    def bus_customer_accum_duration(self):
        """Returns the accumulated customer outage durations."""
        return self.get_float(10, 0)

    def bus_total_miles(self):
        """Returns the total length of line downline from this bus, in miles. For recloser siting algorithm."""
        return self.get_float(11, 0)
