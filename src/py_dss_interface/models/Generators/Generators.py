# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Generators.GeneratorsF import GeneratorsF
from py_dss_interface.models.Generators.GeneratorsI import GeneratorsI
from py_dss_interface.models.Generators.GeneratorsS import GeneratorsS
from py_dss_interface.models.Generators.GeneratorsV import GeneratorsV


class Generators(GeneratorsI, GeneratorsF, GeneratorsS, GeneratorsV):
    """
    This interface implements the Generators (IGenerators) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: GeneratorsI, GeneratorsF, GeneratorsS, GeneratorsV.
    """
    pass
