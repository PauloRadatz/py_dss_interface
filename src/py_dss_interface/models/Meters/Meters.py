# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Meters.MetersF import MetersF
from py_dss_interface.models.Meters.MetersI import MetersI
from py_dss_interface.models.Meters.MetersS import MetersS
from py_dss_interface.models.Meters.MetersV import MetersV


class Meters(MetersV, MetersS, MetersF, MetersI):
    """
    This interface implements the Meters (IMeters) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: MetersV, MetersS, MetersF, MetersI.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def saifi(self) -> float:
        return MetersF._saifi(self)

    @property
    def saifi_kw(self) -> float:
        return MetersF._saifi_kw(self)

    @property
    def saidi(self) -> float:
        return MetersF._saidi(self)

    @property
    def cust_interrupts(self) -> float:
        return MetersF._cust_interrupts(self)

    @property
    def avg_repair_time(self) -> float:
        return MetersF._avg_repair_time(self)

    @property
    def fault_rate_x_repair_hrs(self) -> float:
        return MetersF._fault_rate_x_repair_hrs(self)

    @property
    def sum_branch_flt_rates(self) -> float:
        return MetersF._sum_branch_flt_rates(self)

    def first(self) -> int:
        return MetersI._first(self)

    def next(self) -> int:
        return MetersI._next(self)

    def reset(self) -> int:
        return MetersI._reset(self)

    def reset_all(self) -> int:
        return MetersI._reset_all(self)

    def sample(self) -> int:
        return MetersI._sample(self)

    def sample_all(self) -> int:
        return MetersI._sample_all(self)

    def save(self) -> int:
        return MetersI._save(self)

    def save_all(self) -> int:
        return MetersI._save_all(self)

    @property
    def metered_terminal(self) -> int:
        return MetersI._metered_terminal_read(self)

    @metered_terminal.setter
    def metered_terminal(self, value: int):
        MetersI._metered_terminal_write(self, value)

    @property
    def di_files_are_open(self) -> int:
        return MetersI._di_files_are_open(self)

    def open_all_di_files(self) -> int:
        return MetersI._open_all_di_files(self)

    def close_all_di_files(self) -> int:
        return MetersI._close_all_di_files(self)

    @property
    def count_end_elements(self) -> int:
        return MetersI._count_end_elements(self)

    @property
    def count(self) -> int:
        return MetersI._count(self)

    @property
    def count_branches(self) -> int:
        return MetersI._count_branches(self)

    @property
    def sequence_index(self) -> int:
        return MetersI._sequence_index_read(self)

    @sequence_index.setter
    def sequence_index(self, value: int):
        MetersI._sequence_index_write(self, value)

    def do_reliability_calc(self) -> int:
        return MetersI._do_reliability_calc(self)

    @property
    def seq_list_size(self) -> int:
        return MetersI._seq_list_size(self)

    @property
    def total_customers(self) -> int:
        return MetersI._total_customers(self)

    @property
    def num_sections(self) -> int:
        return MetersI._num_sections_read(self)

    def set_active_section(self, value: int):
        MetersI._set_active_section_write(self, value)

    @property
    def ocp_device_type(self) -> int:
        return MetersI._ocp_device_type(self)

    @property
    def num_section_customers(self) -> int:
        return MetersI._num_section_customers(self)

    @property
    def num_section_branches(self) -> int:
        return MetersI._num_section_branches(self)

    @property
    def sect_seq_idx(self) -> int:
        return MetersI._sect_seq_idx(self)

    @property
    def sect_total_cust(self) -> int:
        return MetersI._sect_total_cust(self)

    @property
    def name(self) -> str:
        return MetersS._name_read(self)

    @name.setter
    def name(self, value: str):
        MetersS._name_write(self, value)

    @property
    def metered_element(self) -> str:
        return MetersS._metered_element_read(self)

    @metered_element.setter
    def metered_element(self, value: str):
        MetersS._metered_element_write(self, value)

    # TODO add types
    @property
    def names(self):
        return MetersV._names(self)

    @property
    def register_names(self):
        return MetersV._register_names(self)

    @property
    def register_values(self):
        return MetersV._register_values(self)

    @property
    def totals(self):
        return MetersV._totals(self)

    @property
    def peak_current(self):
        return MetersV._peak_current_read(self)

    @peak_current.setter
    def peak_current(self, value):
        MetersV._peak_current_write(self, value)

    @property
    def calc_current(self):
        return MetersV._calc_current_read(self)

    @calc_current.setter
    def calc_current(self, value):
        MetersV._calc_current_write(self, value)

    @property
    def alloc_factors(self):
        return MetersV._alloc_factors_read(self)

    @alloc_factors.setter
    def alloc_factors(self, value):
        MetersV._alloc_factors_write(self, value)

    @property
    def all_end_elements(self):
        return MetersV._all_end_elements(self)

    @property
    def all_branches_in_zone(self):
        return MetersV._all_branches_in_zone(self)

    @property
    def all_pce_in_zone(self):
        return MetersV._all_pce_in_zone(self)

