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

    def _kv_base(self) -> float:
        """Returns the base voltage at bus in kV."""
        return self._dss_obj.BUSF(0, 0)

    def _x(self) -> float:
        """Returns the X coordinate for the bus."""
        return self._dss_obj.BUSF(1, 0)

    def _x_write(self, param_coordinate: float) -> int:
        """Allows to write the X coordinate for the bus. Returns 0."""
        param_coordinate = Base._check_float_param(param_coordinate)
        result = self._dss_obj.BUSF(2, ctypes.c_double(param_coordinate))
        Base._check_assertion_result(result, "Write X coordinate failed!", "Something wrong when tried defined the X "
                                                                          "coordinate!")
        return result

    def _y(self) -> float:
        """Returns the Y coordinate for the bus."""
        return self._dss_obj.BUSF(3, 0)

    def _y_write(self, param_coordinate: float) -> int:
        """Allows to write the Y coordinate for the bus. Returns 0.
        ::param param_coordinate: The Y coordinate, if it's None, Y will be 0.0
        """
        param_coordinate = Base._check_float_param(param_coordinate)
        result = self._dss_obj.BUSF(4, ctypes.c_double(param_coordinate))
        Base._check_assertion_result(result, "Write Y coordinate failed!", "Something wrong when tried defined the Y "
                                                                          "coordinate!")
        return result

    def _distance(self) -> float:
        """Returns the distance from the energymeter (if non-zero)."""
        return self._dss_obj.BUSF(5, 0)

    # TODO: need more studies
    def _bus_lambda(self) -> float:
        """Returns the accumulated failure rate downstream from this bus, faults per year."""
        return self._dss_obj.BUSF(6, 0)

    def _interruptions_num(self) -> float:
        """Returns the number of interruptions this bus per year."""
        return self._dss_obj.BUSF(7, 0)

    def _interruptions_avg_duration(self) -> float:
        """Returns the average interruption duration in hours."""
        return self._dss_obj.BUSF(8, 0)

    def _interruptions_total_customers(self) -> float:
        """Returns the annual number of customer interruptions from this bus."""
        return self._dss_obj.BUSF(9, 0)

    def _outage_customer_accum_duration(self) -> float:
        """Returns the accumulated customer outage durations."""
        return self._dss_obj.BUSF(10, 0)

    def _line_total_miles(self) -> float:
        """Returns the total length of line downline from this bus, in miles. For recloser siting algorithm."""
        return self._dss_obj.BUSF(11, 0)

    def _latitude(self) -> float:
        """Returns the GIS latitude assigned to the active bus (if any)."""
        return self._dss_obj.BUSF(12, 0)

    def _latitude_write(self, latitude_param: float) -> float:
        """Sets the GIS latitude to the active bus using the value given at the argument."""
        latitude_param = Base._check_float_param(latitude_param)
        return self._dss_obj.BUSF(13, ctypes.c_double(latitude_param))

    def _longitude(self) -> float:
        """Returns the GIS longitude assigned to the active bus (if any)."""
        return self._dss_obj.BUSF(14, 0)

    def _longitude_write(self, longitude_param: float) -> float:
        """Sets the GIS longitude to the active bus using the value given at the argument."""
        longitude_param = Base._check_float_param(longitude_param)
        return self._dss_obj.BUSF(15, ctypes.c_double(longitude_param))
