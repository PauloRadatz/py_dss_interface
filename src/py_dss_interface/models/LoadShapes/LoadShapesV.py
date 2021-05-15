# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes

from comtypes import automation

from py_dss_interface.models.LoadShapes.LoadShapesS import LoadShapesS
from py_dss_interface.models.Text.Text import Text
from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class LoadShapesV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void LoadShapeV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following. """

    def loadshapes_allnames(self):
        """Gets a variant array of strings containing names of all LoadShape objects currently defined."""
        return Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, 0, None, '')

    def loadshapes_read_pmult(self):
        """Gets a variant array of doubles for the P multiplier in the LoadShape."""
        return Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, 1, None, '')

    def loadshapes_write_pmult(self, argument):
        """Sets a variant array of doubles for the P multiplier in the LoadShape."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        ls = LoadShapesS(self.dss_obj)
        ls_name = ls.loadshapes_read_name()
        return t.text(f'edit LoadShape.{ls_name} pmult = {argument}')

    def loadshapes_read_qmult(self):
        """Gets a variant array of doubles for the Q multiplier in the LoadShape."""
        return Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, 3, None, '')

    def loadshapes_write_qmult(self, argument):
        """Sets a variant array of doubles for the Q multiplier in the LoadShape."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        ls = LoadShapesS(self.dss_obj)
        ls_name = ls.loadshapes_read_name()
        return t.text(f'edit LoadShape.{ls_name} qmult = {argument}')

    def loadshapes_read_timearray(self):
        """Gets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        return Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, ctypes.c_int(5), ctypes.c_int(0), None)

    def loadshapes_write_timearray(self, argument):
        """Sets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        argument = Base.check_string_param(argument)
        t = Text(self.dss_obj)
        ls = LoadShapesS(self.dss_obj)
        ls_name = ls.loadshapes_read_name()
        return t.text(f'edit LoadShape.{ls_name} npts = {argument}')
