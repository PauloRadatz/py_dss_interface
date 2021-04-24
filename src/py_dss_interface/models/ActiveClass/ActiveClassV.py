# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class ActiveClassV(Base):

    def active_class_all_names(self):
        """Gets a variant array of strings consisting of all element names in the active Class."""
        return self.get_variant(0)
