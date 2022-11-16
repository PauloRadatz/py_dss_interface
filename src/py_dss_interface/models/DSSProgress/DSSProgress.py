# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.DSSProgress.DSSProgressI import DSSProgressI
from py_dss_interface.models.DSSProgress.DSSProgressS import DSSProgressS


class DSSProgress(DSSProgressI, DSSProgressS):
    """
    This interface implements the DSSProgress (IDSSProgress) interface of OpenDSS by declaring 2 procedures for
    accessing the different properties included in this interface: DSSProgressI, DSSProgressS
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    def pct_progress(self, arg: float) -> int:
        return DSSProgressI._pct_progress(self, arg)

    def show(self) -> int:
        return DSSProgressI._show(self)

    def close(self) -> int:
        return DSSProgressI._close(self)

    def caption(self, arg: str) -> str:
        return DSSProgressS._caption(self, arg)


