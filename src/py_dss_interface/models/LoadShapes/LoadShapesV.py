# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base
from py_dss_interface.models.LoadShapes.LoadShapesS import LoadShapesS
from py_dss_interface.models.Text.Text import Text


class LoadShapesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void LoadShapeV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following. """

    def loadshapes_all_names(self):
        """Gets a variant array of strings containing names of all LoadShape objects currently defined."""
        return Bridge.var_array_function(self.dss_obj.LoadShapeV, 0, None, '')

    def loadshapes_read_p_mult(self):
        """Gets a variant array of doubles for the P multiplier in the LoadShape."""
        return Bridge.var_array_function(self.dss_obj.LoadShapeV, 1, None, '')

    def loadshapes_write_p_mult(self, argument):
        """Sets a variant array of doubles for the P multiplier in the LoadShape."""
        t = Text(self.dss_obj)
        ls = LoadShapesS(self.dss_obj)
        ls_name = ls.loadshapes_read_name()
        return t.text(f'edit LoadShape.{ls_name} pmult = {argument}')

    def loadshapes_read_q_mult(self):
        """Gets a variant array of doubles for the Q multiplier in the LoadShape."""
        return Bridge.var_array_function(self.dss_obj.LoadShapeV, 3, None, '')

    def loadshapes_write_q_mult(self, argument):
        """Sets a variant array of doubles for the Q multiplier in the LoadShape."""
        t = Text(self.dss_obj)
        ls = LoadShapesS(self.dss_obj)
        ls_name = ls.loadshapes_read_name()
        return t.text(f'edit LoadShape.{ls_name} qmult = {argument}')

    def loadshapes_read_time_array(self):
        """Gets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        return Bridge.var_array_function(self.dss_obj.LoadShapeV, ctypes.c_int(5), ctypes.c_int(0), None)

    def loadshapes_write_time_array(self, argument):
        """Sets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        t = Text(self.dss_obj)
        ls = LoadShapesS(self.dss_obj)
        ls_name = ls.loadshapes_read_name()
        return t.text(f'edit LoadShape.{ls_name} hour = {argument}')
