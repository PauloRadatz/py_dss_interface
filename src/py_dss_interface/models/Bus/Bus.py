# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Bus.BusF import BusF
from py_dss_interface.models.Bus.BusI import BusI
from py_dss_interface.models.Bus.BusS import BusS
from py_dss_interface.models.Bus.BusV import BusV


class Bus(BusS, BusI, BusV, BusF):
    """
    This interface implements the Bus (IBus) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: BusS, BusI, BusV, BusF.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def name(self) -> str:
        return BusS._name(self)

    @property
    def num_nodes(self) -> int:
        return BusI._num_nodes(self)

    @property
    def zsc_refresh(self) -> int:
        return BusI._zsc_refresh(self)

    @property
    def coord_defined(self) -> int:
        return BusI._coord_defined(self)

    @property
    def unique_node_number(self) -> int:
        return BusI._unique_node_number(self, 1)

    @unique_node_number.setter
    def unique_node_number(self, start_number: int):
        BusI._unique_node_number(self, start_number)

    @property
    def total_customers(self) -> int:
        return BusI._total_customers(self)

    @property
    def section_id(self) -> int:
        return BusI._section_id(self)

    @property
    def voltages(self):
        return BusV._voltages(self)

    @property
    def seq_voltages(self):
        return BusV._seq_voltages(self)

    @property
    def nodes(self) -> List[int]:
        return BusV._nodes(self)

    @property
    def voc(self):
        return BusV._voc(self)

    @property
    def isc(self):
        return BusV._isc(self)

    @property
    def pu_voltages(self):
        return BusV._pu_voltages(self)

    @property
    def zsc_matrix(self):
        return BusV._zsc_matrix(self)

    @property
    def zsc1(self):
        return BusV._zsc1(self)

    @property
    def zsc0(self):
        return BusV._zsc0(self)

    @property
    def ysc_matrix(self):
        return BusV._ysc_matrix(self)

    @property
    def cplx_sequence_voltages(self):
        return BusV._cplx_sequence_voltages(self)

    @property
    def vll(self):
        return BusV._vll(self)

    @property
    def pu_vll(self):
        return BusV._pu_vll(self)

    @property
    def vmag_angle(self):
        return BusV._vmag_angle(self)

    @property
    def vmag_angle_pu(self):
        return BusV._vmag_angle_pu(self)

    @property
    def line_list(self):
        return BusV._line_list(self)

    @property
    def load_list(self):
        return BusV._load_list(self)

    @property
    def axc_012_matrix(self):
        return BusV._axc_012_matrix(self)

    @property
    def all_pce_active_bus(self) -> List[str]:
        return BusV._all_pce_active_bus(self)

    @property
    def all_pde_active_bus(self) -> List[str]:
        return BusV._all_pde_active_bus(self)

    @property
    def kv_base(self) -> float:
        return BusF._kv_base(self)

    @property
    def x(self) -> float:
        return BusF._x(self)

    @x.setter
    def x(self, value: float):
        BusF._x_write(self, value)

    @property
    def y(self) -> float:
        return BusF._y(self)

    @y.setter
    def y(self, value: float):
        BusF._y_write(self, value)

    @property
    def distance(self) -> float:
        return BusF._distance(self)

    @property
    def bus_lambda(self) -> float:
        return BusF._bus_lambda(self)

    @property
    def interruptions_num(self) -> float:
        return BusF._interruptions_num(self)

    @property
    def interruptions_avg_duration(self) -> float:
        return BusF._interruptions_avg_duration(self)

    @property
    def interruptions_total_customers(self) -> float:
        return BusF._interruptions_total_customers(self)

    @property
    def outage_customer_accum_duration(self) -> float:
        return BusF._outage_customer_accum_duration(self)

    @property
    def line_total_miles(self) -> float:
        return BusF._line_total_miles(self)

    @property
    def latitude(self) -> float:
        return BusF._latitude(self)

    @latitude.setter
    def latitude(self, value: float):
        BusF._latitude_write(self, value)

    @property
    def longitude(self) -> float:
        return BusF._longitude(self)

    @longitude.setter
    def longitude(self, value: float):
        BusF._longitude_write(self, value)
