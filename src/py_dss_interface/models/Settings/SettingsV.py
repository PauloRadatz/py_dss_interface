# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Text.Text import Text


class SettingsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SettingsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    def settings_read_ue_regs(self):
        """Gets the array of Integers defining Energy Meter registers to use for computing UE."""
        return Bridge.var_array_function(self.dss_obj.SettingsV, 0, None, '')

    def settings_write_ue_regs(self, argument):
        """Sets the array of Integers defining Energy Meter registers to use for computing UE."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        return t.text(f'Set UEregs = {argument}')

    def settings_read_loss_regs(self):
        """Gets the array of Integers defining Energy Meter registers to use for computing Losses."""
        return Bridge.var_array_function(self.dss_obj.SettingsV, 2, None, '')

    def settings_write_loss_regs(self, argument):
        """Sets the array of Integers defining Energy Meter registers to use for computing Losses."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        return t.text(f'Set Lossregs = {argument}')

    def settings_read_voltage_bases(self):
        """Gets the array of doubles defining the legal voltage bases in kV L-L."""
        return Bridge.var_array_function(self.dss_obj.SettingsV, 4, None, '')

    def settings_write_voltage_bases(self, argument):
        """Sets the array of doubles defining the legal voltage bases in kV L-L."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        return t.text(f'Set Voltagebases = {argument}')
