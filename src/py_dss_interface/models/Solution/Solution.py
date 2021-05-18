# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Solution.SolutionF import SolutionF
from py_dss_interface.models.Solution.SolutionI import SolutionI
from py_dss_interface.models.Solution.SolutionS import SolutionS
from py_dss_interface.models.Solution.SolutionV import SolutionV


class Solution(SolutionI, SolutionF, SolutionS, SolutionV):
    """
    This interface implements the Solution (ISolution) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: SolutionI, SolutionF, SolutionS, SolutionV.
    """
    pass
