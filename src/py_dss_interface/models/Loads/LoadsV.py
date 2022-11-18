# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.Loads import Loads
from py_dss_interface.models.Text.Text import Text


class LoadsV(Base):
    """
    This interface can be used to read/modify the properties of the Loads Class where the values are variants (the
    value can have different formats).

    The structure of the interface is as follows:
        void DSSLoadsV(int32_t Parameter, VARIANT *Argument);

    This interface returns a string, the variable “parameter” (Integer) is used to specify the property of the class
    to be used and the variable “argument” (Variant) is used to return the variant structure.
    """

    def _names(self):
        """Allows to read the names of all the loads present in the active circuit. The result is delivered as
        variant, however, the content of this variant is an array of strings. """
        return Bridge.variant_pointer_read(self.dss_obj.DSSLoadsV, 0)

    def _zipv_read(self):
        """Allows to read the array of 7 elements (doubles) for ZIP property of the active Load object."""
        return Bridge.variant_pointer_read(self.dss_obj.DSSLoadsV, 1)

    def _zipv_write(self, argument):
        """Allows to write the array of 7 elements (doubles) for ZIP property of the active Load object.
        :param argument: Array of 7 coefficients:
                    First 3 are ZIP weighting factors for real power (should sum to 1)
                    Next 3 are ZIP weighting factors for reactive power (should sum to 1)
                    Last 1 is cut-off voltage in p.u. of base kV; load is 0 below this cut-off
                    No defaults; all coefficients must be specified if using model=8.
        """

        return Bridge.variant_pointer_write(self.dss_obj.DSSLoadsV, 2, argument)
        # argument = Base.check_string_param(argument)
        # t = Text(self.dss_obj)
        # load = Loads.Loads(self.dss_obj)
        # load_name = load.name
        # return t.text(f'edit Load.{load_name} zipv = {argument}')
