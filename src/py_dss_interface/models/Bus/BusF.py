# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models.Base import Base


class BusF(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double BUSF(int32_t Parameter, double Argument)

    This interface returns a floating point number (64 bits) according to the number sent in the variable
    “parameter”. The parameter can be one of the following.
    """

    def bus_kv_base(self) -> float:
        """Returns the base voltage at bus in kV."""
        return self.dss_obj.BUSF(0, 0)

    def bus_read_x(self) -> float:
        """Returns the X coordinate for the bus."""
        return self.dss_obj.BUSF(1, 0)

    def bus_write_x(self, param_coordinate) -> int:
        """Allows to write the X coordinate for the bus. Returns 0.
        :param param_coordinate: The X coordinate, if it's None, X will be 0.0
        """
        param_coordinate = Base.check_float_param(param_coordinate)
        result = self.dss_obj.BUSF(2, ctypes.c_double(param_coordinate))
        Base.check_assertion_result(result, "Write X coordinate failed!", "Something wrong when tried defined the X "
                                                                          "coordinate!")
        return result

    def bus_read_y(self) -> float:
        """Returns the X coordinate for the bus."""
        return self.dss_obj.BUSF(3, 0)

    def bus_write_y(self, param_coordinate: float) -> int:
        """Allows to write the Y coordinate for the bus. Returns 0.
        :param param_coordinate: The Y coordinate, if it's None, Y will be 0.0
        """
        param_coordinate = Base.check_float_param(param_coordinate)
        result = self.dss_obj.BUSF(4, ctypes.c_double(param_coordinate))
        Base.check_assertion_result(result, "Write Y coordinate failed!", "Something wrong when tried defined the Y "
                                                                          "coordinate!")
        return result

    def bus_distance(self) -> float:
        """Returns the distance from the energymeter (if non-zero)."""
        return self.dss_obj.BUSF(5, 0)

    def bus_lambda(self) -> float:
        """Returns the accumulated failure rate downstream from this bus, faults per year."""
        return self.dss_obj.BUSF(6, 0)

    def bus_interruptions_num(self) -> float:
        """Returns the number of interruptions this bus per year."""
        return self.dss_obj.BUSF(7, 0)

    def bus_interruptions_avg_duration(self) -> float:
        """Returns the average interruption duration in hours."""
        return self.dss_obj.BUSF(8, 0)

    def bus_interruptions_total_customers(self) -> float:
        """Returns the annual number of customer interruptions from this bus."""
        return self.dss_obj.BUSF(9, 0)

    def bus_outage_customer_accum_duration(self) -> float:
        """Returns the accumulated customer outage durations."""
        return self.dss_obj.BUSF(10, 0)

    def bus_line_total_miles(self) -> float:
        """Returns the total length of line downline from this bus, in miles. For recloser siting algorithm."""
        return self.dss_obj.BUSF(11, 0)

    def bus_read_latitude(self) -> float:
        """This parameter returns the GIS latitude assigned to the active bus (if any)."""
        return self.dss_obj.BUSF(12, 0)

    def bus_write_latitude(self, latitude_param: float) -> float:
        """This parameter sets the GIS latitude to the active bus using the value given at the argument.."""
        latitude_param = Base.check_float_param(latitude_param)
        return self.dss_obj.BUSF(13, ctypes.c_double(latitude_param))

    def bus_read_longitude(self) -> float:
        """This parameter returns the GIS longitude assigned to the active bus (if any)."""
        return self.dss_obj.BUSF(14, 0)

    def bus_write_longitude(self, longitude_param: float) -> float:
        """This parameter sets the GIS longitude to the active bus using the value given at the argument.."""
        longitude_param = Base.check_float_param(longitude_param)
        return self.dss_obj.BUSF(15, ctypes.c_double(longitude_param))
