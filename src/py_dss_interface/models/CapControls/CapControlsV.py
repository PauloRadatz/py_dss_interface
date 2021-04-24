# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class CapControlsV(Base):

    def cap_controls_all_names(self):
        return self.get_variant(0)
