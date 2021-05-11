# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.ISources.ISourcesF import ISourcesF
from py_dss_interface.models.ISources.ISourcesI import ISourcesI
from py_dss_interface.models.ISources.ISourcesS import ISourcesS
from py_dss_interface.models.ISources.ISourcesV import ISourcesV


class ISources(ISourcesI, ISourcesF, ISourcesS, ISourcesV):
    """
    This interface implements the ISources (IIsources) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: ISourcesI, ISourcesF, ISourcesS, ISourcesV.
    """
    pass
