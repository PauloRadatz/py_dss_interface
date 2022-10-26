# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Text.Text import Text


class SensorsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SensorsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def _all_names(self):
        """Returns a variant array of sensor names."""
        return Bridge.var_array_function(self.dss_obj.SensorsV, ctypes.c_int(0), ctypes.c_int(0), None)

    def _currents(self):
        """Gets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        return Bridge.var_array_function(self.dss_obj.SensorsV, ctypes.c_int(1), ctypes.c_int(0), None)

    def _currents_write(self, argument):
        """Sets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        sen = self.dss_obj.sensors.name
        sen_name = sen.name
        return t.text(f'edit Sensor.{sen_name} currents = {argument}')

    def _kvars(self):
        """Gets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        return Bridge.var_array_function(self.dss_obj.SensorsV, ctypes.c_int(3), ctypes.c_int(0), None)

    def _kvars_write(self, argument):
        """Sets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        sen = self.dss_obj.sensors.name
        sen_name = sen.name
        return t.text(f'edit Sensor.{sen_name} kvars = {argument}')

    def _kws(self):
        """Gets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        return Bridge.var_array_function(self.dss_obj.SensorsV, ctypes.c_int(5), ctypes.c_int(0), None)

    def _kws_write(self, argument):
        """Sets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        sen = self.dss_obj.sensors.name
        sen_name = sen.name
        return t.text(f'edit Sensor.{sen_name} kws = {argument}')
