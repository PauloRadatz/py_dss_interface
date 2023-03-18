# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Text.Text import Text
from typing import List
from py_dss_interface.utils.Error import Error


class SettingsV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void SettingsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """

    # TODO work here

    def _ue_regs_read(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.SettingsV, 0)

    def _ue_regs_write(self, argument: List[int]):
        Error.method_not_working("setter of dss.settings.ue_regs")
        # return Bridge.variant_pointer_write(self.dss_obj.SettingsV, 1, argument)
        # argument = Base.check_string_param(argument)
        # t = Text(self.dss_obj)
        # return t.text(f'Set UEregs = {argument}')

    def _loss_regs_read(self) -> List[int]:
        return Bridge.variant_pointer_read(self._dss_obj.SettingsV, 2)

    def _loss_regs_write(self, argument: List[int]):
        Error.method_not_working("setter of dss.settings.loss_regs")
        # argument = Base.check_string_param(argument)
        # t = Text(self.dss_obj)
        # return t.text(f'Set Lossregs = {argument}')

    def _voltage_bases_read(self) -> List[float]:
        return Bridge.variant_pointer_read(self._dss_obj.SettingsV, 4)

    def _voltage_bases_write(self, argument: List[float]):
        Error.method_not_working("setter of dss.settings.voltage_bases")
        # return Bridge.variant_pointer_write(self.dss_obj.SettingsV, 5, argument)
        # argument = Base.check_string_param(argument)
        # t = Text(self.dss_obj)
        # return t.text(f'Set Voltagebases = {argument}')
