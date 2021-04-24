# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Base import Base


class ActiveClassS(Base):
    def active_class_get_name(self):
        """Gets the name of the active Element of the Active class."""
        return self.dss_obj.get_string(0, 0)

    def active_class_write_name(self, argument):
        """Sets the name of the active Element of the Active class."""
        return self.dss_obj.get_string(1, argument)

    def active_class_set_name(self):
        """Sets the name of the active Element of the Active class."""
        return self.dss_obj.get_string(2, 0)
