# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models.Monitors.MonitorsI import MonitorsI
from py_dss_interface.models.Monitors.MonitorsS import MonitorsS
from py_dss_interface.models.Monitors.MonitorsV import MonitorsV
from typing import List


class Monitors(MonitorsI, MonitorsV, MonitorsS):
    """
    This interface implements the Monitors (IMonitors) interface of OpenDSS by declaring 3 procedures for accessing
    the different properties included in this interface: MonitorsI._, MonitorsV, MonitorsS.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    def first(self) -> int:
        return MonitorsI._first(self)

    def next(self) -> int:
        return MonitorsI._next(self)

    def reset(self) -> int:
        return MonitorsI._reset(self)

    def reset_all(self) -> int:
        return MonitorsI._reset_all(self)

    def sample(self) -> int:
        return MonitorsI._sample(self)

    def save(self) -> int:
        return MonitorsI._save(self)

    def show(self) -> int:
        return MonitorsI._show(self)

    @property
    def mode(self) -> int:
        return MonitorsI._mode(self)

    @mode.setter
    def mode(self, argument):
        MonitorsI._mode_write(self, argument)

    @property
    def sample_count(self) -> int:
        return MonitorsI._sample_count(self)

    def sample_all(self) -> int:
        return MonitorsI._sample_all(self)

    def save_all(self) -> int:
        return MonitorsI._save_all(self)

    @property
    def count(self) -> int:
        return MonitorsI._count(self)

    def process(self) -> int:
        return MonitorsI._process(self)

    def process_all(self) -> int:
        return MonitorsI._process_all(self)

    @property
    def file_version(self) -> int:
        return MonitorsI._file_version(self)

    @property
    def record_size(self) -> int:
        return MonitorsI._record_size(self)

    @property
    def num_channels(self) -> int:
        return MonitorsI._num_channels(self)

    @property
    def terminal(self) -> int:
        return MonitorsI._terminal(self)

    @terminal.setter
    def terminal(self, argument: int):
        MonitorsI._terminal_write(self, argument)

    @property
    def file_name(self) -> str:
        return MonitorsS._file_name(self)

    @property
    def name(self) -> str:
        return MonitorsS._name_read(self)

    @name.setter
    def name(self, argument: str):
        MonitorsS._name_write(self, argument)

    @property
    def element(self) -> str:
        return MonitorsS._element_read(self)

    @element.setter
    def element(self, argument: str):
        MonitorsS._element_write(self, argument)

    @property
    def names(self) -> List[str]:
        return MonitorsV._names(self)

    @property
    def byte_stream(self) -> List[int]:
        return MonitorsV._byte_stream(self)

    @property
    def header(self) -> List[str]:
        return MonitorsV._header(self)

    @property
    def dbl_hour(self) -> List[float]:
        return MonitorsV._dbl_hour(self)

    @property
    def dbl_freq(self) -> List[float]:
        return MonitorsV._dbl_freq(self)

    def channel(self, argument) -> List[float]:
        return MonitorsV._channel(self, argument)
