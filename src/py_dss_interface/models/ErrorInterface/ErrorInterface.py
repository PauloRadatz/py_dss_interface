# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/05/2021
"""
from py_dss_interface.models.ErrorInterface.ErrorCode import ErrorCode
from py_dss_interface.models.ErrorInterface.ErrorDesc import ErrorDesc


class ErrorOpenDSS(ErrorCode, ErrorDesc):
    """
    This interface implements the ErrorOpenDSS interface of OpenDSS by declaring 2 procedures for accessing the
    different properties included in this interface.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def error_code(self):
        """This interface returns an integer with the latest error code delivered by OpenDSS."""
        return ErrorCode._error_code(self)

    @property
    def error_desc(self):
        """This interface returns a string with description of the latest error code delivered by OpenDSS.
                """
        return ErrorDesc._error_desc(self)
