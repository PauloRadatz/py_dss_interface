# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.DSSProperties.DSSPropertiesS import DSSPropertiesS


class DSSProperties(DSSPropertiesS):
    """
    This interface implements the DSSproperties (IDSSProperties) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface.

    This interface can be used to read/write certain properties of DSS objects.

    The structure of the interface is as follows:
        CStr DSSProperties(int32_t Parameter, CStr Argument);

    This interface returns a string pointer (ANSI) with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """
    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def name_active_property(self, argument: str) -> str:
        """Delivers the name of the active property. The index of the property must be specified in the argument.
                The index minimum value is 1. This value must be entered as string."""
        return DSSPropertiesS._name_active_property(self, argument)

    def description_active_property(self, argument: str) -> str:
        """This parameter will deliver the description of the active property. This parameter will deliver the name of
                the active property. The index of the property must be specified in the argument. The index minimum value is
                1. This value must be entered as string.
                """
        return DSSPropertiesS._description_active_property(self, argument)

    # TODO Understand the value option
    def value_read(self, argument: str) -> str:
        """This parameter will deliver the value of the active property. This parameter will deliver the name of the
                active property. The index of the property must be specified in the argument. The index minimum value is 1.
                This value must be entered as string.
                """
        return DSSPropertiesS._value_read(self, argument)

    # TODO include in test
    def value_write(self, argument: str) -> str:
        """This parameter will allow to set the value of the active property. The new value must be specified in the
                variable “argument” as string. This parameter will deliver the name of the active property. The index of the
                property must be specified in the argument. The index minimum value is 1. This value must be entered as string.
                """
        return DSSPropertiesS._value_write(self, argument)
