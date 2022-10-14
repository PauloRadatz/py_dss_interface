# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.CktElement.CktElementF import CktElementF
from py_dss_interface.models.CktElement.CktElementI import CktElementI
from py_dss_interface.models.CktElement.CktElementS import CktElementS
from py_dss_interface.models.CktElement.CktElementV import CktElementV


class CktElement(CktElementI, CktElementS, CktElementF, CktElementV):
    """
    This interface implements the CktElement interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: CktElementI, CktElementS, CktElementF, CktElementV
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def norm_amps(self):
        return CktElementF._norm_amps(self)

    @norm_amps.setter
    def norm_amps(self, argument: float):
        CktElementF._norm_amps_write(self, argument)

    @property
    def emerg_amps(self) -> float:
        return CktElementF._emerg_amps(self)

    @emerg_amps.setter
    def emerg_amps(self, argument: float):
        CktElementF._emerg_amps_write(self, argument)

    @property
    def num_terminals(self) -> int:
        return CktElementI._num_terminals(self)

    @property
    def num_conductors(self) -> int:
        return CktElementI._num_conductors(self)

    @property
    def num_phases(self) -> int:
        return CktElementI._num_phases(self)

    @property
    def is_terminal_open(self) -> int:
        return CktElementI._is_terminal_open(self)

    @property
    def num_properties(self) -> int:
        return CktElementI._num_properties(self)

    @property
    def has_switch_control(self) -> int:
        return CktElementI._has_switch_control(self)

    @property
    def has_volt_control(self) -> int:
        return CktElementI._has_volt_control(self)

    @property
    def num_controls(self) -> int:
        return CktElementI._num_controls(self)

    @property
    def ocp_dev_index(self) -> int:
        return CktElementI._ocp_dev_index(self)

    @property
    def ocp_dev_type(self) -> int:
        return CktElementI._ocp_dev_type(self)

    @property
    def is_enabled(self) -> int:
        return CktElementI._is_enabled(self)

    @property
    def name(self) -> str:
        return CktElementS._name(self)

    @property
    def display(self) -> str:
        return CktElementS._display(self)

    @display.setter
    def display(self, argument: str):
        CktElementS._display_write(self, argument)

    @property
    def guid(self) -> str:
        return CktElementS._guid(self)

    @property
    def energymeter(self) -> str:
        return CktElementS._energymeter(self)

    @property
    def bus_names(self) -> List[str]:
        return CktElementV._bus_names(self)

    @bus_names.setter
    def bus_names(self, values):
        dss, argument = values
        CktElementV._bus_names_write(self, dss, argument)

    @property
    def voltages(self) -> List[float]:
        return CktElementV._voltages(self)

    @property
    def currents(self) -> List[float]:
        return CktElementV._currents(self)

    @property
    def powers(self) -> List[float]:
        return CktElementV._powers(self)

    @property
    def losses(self) -> List[float]:
        return CktElementV._losses(self)

    @property
    def phase_losses(self) -> List[float]:
        return CktElementV._phase_losses(self)

    @property
    def seq_voltages(self) -> List[float]:
        return CktElementV._seq_voltages(self)

    @property
    def seq_currents(self) -> List[float]:
        return CktElementV._seq_currents(self)

    @property
    def seq_powers(self) -> List[float]:
        return CktElementV._seq_powers(self)

    @property
    def property_names(self) -> List[str]:
        return CktElementV._property_names(self)

    @property
    def residuals_currents(self) -> List[float]:
        return CktElementV._residuals_currents(self)

    @property
    def y_prim(self) -> List[float]:
        return CktElementV._y_prim(self)

    @property
    def cplx_seq_voltages(self) -> List[float]:
        return CktElementV._cplx_seq_voltages(self)

    @property
    def cplx_seq_currents(self) -> List[float]:
        return CktElementV._cplx_seq_currents(self)

    @property
    def variables_names(self) -> List[str]:
        return CktElementV._variables_names(self)

    @property
    def variables_values(self) -> List[float]:
        return CktElementV._variables_values(self)

    @property
    def node_order(self) -> List[int]:
        return CktElementV._node_order(self)

    @property
    def currents_mag_ang(self) -> List[float]:
        return CktElementV._currents_mag_ang(self)

    @property
    def voltages_mag_ang(self) -> List[float]:
        return CktElementV._voltages_mag_ang(self)

    def controller(self, argument: str) -> str:
        return CktElementS._controller(self, argument)

    def enabled(self, argument: int) -> int:
        return CktElementI._enabled(self, argument)

    def variable_i(self, argument: float) -> float:
        return CktElementF._variable_i(self, argument)

    def open_terminal(self, argument: int) -> int:
        return CktElementI._open_terminal(self, argument)

    def close_terminal(self, argument: int) -> int:
        return CktElementI._close_terminal(self, argument)
