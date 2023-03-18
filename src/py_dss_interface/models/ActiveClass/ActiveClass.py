# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from dataclasses import dataclass

from py_dss_interface.models.ActiveClass.ActiveClassI import ActiveClassI
from py_dss_interface.models.ActiveClass.ActiveClassS import ActiveClassS
from py_dss_interface.models.ActiveClass.ActiveClassV import ActiveClassV

from typing import List


class ActiveClass(ActiveClassS, ActiveClassI, ActiveClassV):
    """This class implements the ActiveClass interface of OpenDSS.

    The ActiveClass interface provides methods for accessing properties of DSS classes.
    This class defines the methods for accessing the different properties included in this interface:
    ActiveClassS, ActiveClassI, ActiveClassV.

    Args:
        obj_dss: The object that provides access to the OpenDSS engine.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def first(self) -> int:
        """Sets first element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element.
        Returns 0 if none."""
        return ActiveClassI._first(self)

    def next(self) -> int:
        """Sets next element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element. Returns 0 if none."""
        return ActiveClassI._next(self)

    @property
    def num_elements(self) -> int:
        """Gets the number of elements in this class. Same as Count Property."""
        return ActiveClassI._num_elements(self)

    @property
    def count(self) -> int:
        """Gets the number of elements in this class. Same as NumElements Property."""
        return ActiveClassI._count(self)

    @property
    def name(self) -> str:
        """Gets the name of the active Element of the Active class.
        Sets the name of the active Element of the Active class."""
        return ActiveClassS._name(self)

    @name.setter
    def name(self, argument: str):
        ActiveClassS._name_write(self, argument)

    @property
    def class_name(self) -> str:
        """Gets the name of the active Element's class."""
        return ActiveClassS._class_name(self)

    @property
    def parent_class_name(self) -> str:
        """Gets the name of the Parent Element of the Active class."""
        return ActiveClassS._parent_class_name(self)

    @property
    def names(self) -> List[str]:
        """Gets a variant array of strings consisting of all element names in the active Class."""
        return ActiveClassV._names(self)

