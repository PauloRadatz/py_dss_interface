# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Transformers.TransformersF import TransformersF
from py_dss_interface.models.Transformers.TransformersI import TransformersI
from py_dss_interface.models.Transformers.TransformersS import TransformersS
from py_dss_interface.models.Transformers.TransformersV import TransformersV


class Transformers(TransformersV, TransformersF, TransformersI, TransformersS):
    """
    This interface implements the Transformers (ITransformer) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: TransformersV, TransformersF, TransformersI, TransformersS.
    """
    pass
