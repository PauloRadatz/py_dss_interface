# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""

from py_dss_interface.models.Meters.MetersF import MetersF
from py_dss_interface.models.Meters.MetersI import MetersI
from py_dss_interface.models.Meters.MetersS import MetersS
from py_dss_interface.models.Meters.MetersV import MetersV
from typing import List


class Meters(MetersV, MetersS, MetersF, MetersI):
    """
    This interface implements the Meters (IMeters) interface of OpenDSS by declaring 4 procedures for accessing the
    different properties included in this interface: MetersV, MetersS, MetersF, MetersI.
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def saifi(self) -> float:
        """Returns SAIFI for this meter's zone. Execute reliability calc method first."""
        return MetersF._saifi(self)

    @property
    def saifi_kw(self) -> float:
        """Returns the SAIFI based on kW rather than number of customers. Get after reliability calcs."""
        return MetersF._saifi_kw(self)

    @property
    def saidi(self) -> float:
        """Returns the SAIDI for this meter zone. Execute DoreliabilityCalc first."""
        return MetersF._saidi(self)

    @property
    def cust_interrupts(self) -> float:
        """Returns the total customer interruptions for this meter zone based on reliability calcs."""
        return MetersF._cust_interrupts(self)

    @property
    def avg_repair_time(self) -> float:
        """Returns the average Repair Time in this Section of the meter zone."""
        return MetersF._avg_repair_time(self)

    @property
    def fault_rate_x_repair_hrs(self) -> float:
        """Returns the sum of Fault Rate Time Repair Hours in this section of the meter zone."""
        return MetersF._fault_rate_x_repair_hrs(self)

    @property
    def sum_branch_flt_rates(self) -> float:
        """Returns the sum of the branch fault rates in this section of the meter's zone."""
        return MetersF._sum_branch_flt_rates(self)

    def first(self) -> int:
        """Sets the first Energy Meter active. Returns 0 if no monitors."""
        return MetersI._first(self)

    def next(self) -> int:
        """Sets the next energy Meter Active. Returns 0 if no more."""
        return MetersI._next(self)

    def reset(self) -> int:
        """Resets the active Meter object."""
        return MetersI._reset(self)

    def reset_all(self) -> int:
        """Resets all Meter object."""
        return MetersI._reset_all(self)

    def sample(self) -> int:
        """Causes active meter to take a sample."""
        return MetersI._sample(self)

    def sample_all(self) -> int:
        """Causes all Energy Meters to take a sample of the present state. Returns 0."""
        return MetersI._sample_all(self)

    def save(self) -> int:
        """Causes active meter to save its current sample buffer to its meter stream. Then you can access the
                Bytestream or channel data. Most standard solution modes do this automatically. """
        return MetersI._save(self)

    def save_all(self) -> int:
        """Save all Energy Meter buffers to their respective file streams. Returns 0."""
        return MetersI._save_all(self)

    @property
    def metered_terminal(self) -> int:
        """Returns the number of metered terminal by the active Energy Meter.

        Sets the number of metered terminal by the active Energy Meter."""
        return MetersI._metered_terminal_read(self)

    @metered_terminal.setter
    def metered_terminal(self, arg: int):
        MetersI._metered_terminal_write(self, arg)

    @property
    def di_files_are_open(self) -> int:
        """Returns a global flag (1=true, 0=false) to indicate if Demand Interval (DI) files have been properly
                opened. """
        return MetersI._di_files_are_open(self)

    def open_all_di_files(self) -> int:
        """Opens Demand Interval (DI) files. Returns 0."""
        return MetersI._open_all_di_files(self)

    def close_all_di_files(self) -> int:
        """Closes all Demand Interval (DI) files. Necessary at the end of a run."""
        return MetersI._close_all_di_files(self)

    @property
    def count_end_elements(self) -> int:
        """Returns the number of zone end elements in the active meter zone."""
        return MetersI._count_end_elements(self)

    @property
    def count(self) -> int:
        """Returns the number of Energy Meters in the Active Circuit."""
        return MetersI._count(self)

    @property
    def count_branches(self) -> int:
        """Returns the number of branches in active Energy Meter zone (same as sequencelist size)."""
        return MetersI._count_branches(self)

    @property
    def sequence_index(self) -> int:
        """Returns the index into meter's SequenceList that contains branch pointers in lexical order. Earlier index
                guaranteed to be up line from later index. Sets PDElement active.

        Sets the index into meter's SequenceList that contains branch pointers in lexical order. Earlier index
        guaranteed to be up line from later index. Sets PDElement active. """
        return MetersI._sequence_index_read(self)

    @sequence_index.setter
    def sequence_index(self, arg: int):
        MetersI._sequence_index_write(self, arg)

    def do_reliability_calc(self) -> int:
        return MetersI._do_reliability_calc(self)

    @property
    def seq_list_size(self) -> int:
        """Calculates SAIFI, etc. if the Argument is equal to 1 this parameter will assume restoration, otherwise it
                will not. """
        return MetersI._seq_list_size(self)

    @property
    def total_customers(self) -> int:
        """Returns the total number of customers in this zone (down line from the Energy Meter)."""
        return MetersI._total_customers(self)

    @property
    def num_sections(self) -> int:
        """Returns the number of feeder sections in this meter's zone."""
        return MetersI._num_sections_read(self)

    def set_active_section(self, arg: int):
        """Sets the designated section (argument) if the index is valid."""
        MetersI._set_active_section_write(self, arg)

    @property
    def ocp_device_type(self) -> int:
        """Returns the type of OCP device: {1=fuse | 2+ recloser | 3= relay}."""
        return MetersI._ocp_device_type(self)

    @property
    def num_section_customers(self) -> int:
        """Returns the number of customers in the active section."""
        return MetersI._num_section_customers(self)

    @property
    def num_section_branches(self) -> int:
        """Returns the number of branches (lines) in the active section."""
        return MetersI._num_section_branches(self)

    @property
    def sect_seq_idx(self) -> int:
        """Returns the Sequence Index of the branch at the head of this section."""
        return MetersI._sect_seq_idx(self)

    @property
    def sect_total_cust(self) -> int:
        """Returns the total customers down line from this section."""
        return MetersI._sect_total_cust(self)

    @property
    def name(self) -> str:
        """Returns the active Energy Meter's name.

        Sets the active Energy Meter's name."""
        return MetersS._name_read(self)

    @name.setter
    def name(self, arg: str):
        MetersS._name_write(self, arg)

    @property
    def metered_element(self) -> str:
        """Returns the name of the metered element (considering the active Energy Meter).

        Sets the name of the metered element (considering the active Energy Meter)."""
        return MetersS._metered_element_read(self)

    @metered_element.setter
    def metered_element(self, arg: str):
        MetersS._metered_element_write(self, arg)

    @property
    def names(self) -> List[str]:
        """Returns an array of all Energy Meter names."""
        return MetersV._names(self)

    @property
    def register_names(self) -> List[str]:
        """Returns an array of strings containing the names of the registers."""
        return MetersV._register_names(self)

    @property
    def register_values(self) -> List[float]:
        """Returns an array of values contained in the Meter registers for the active Meter."""
        return MetersV._register_values(self)

    @property
    def totals(self) -> List[float]:
        """Returns the totals for all registers of all Meters."""
        return MetersV._totals(self)

    @property
    def peak_current(self) -> List[float]:
        """Returns an array of doubles with the Peak Current Property.

        Receives an array of doubles to set values of Peak Current Property."""
        return MetersV._peak_current_read(self)

    @peak_current.setter
    def peak_current(self, arg: List[float]):
        MetersV._peak_current_write(self, arg)

    @property
    def calc_current(self) -> List[float]:
        """Returns the magnitude of the real part of the Calculated Current (normally determined by solution)
                for the meter to force some behavior on Load Allocation.

        Sets the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        return MetersV._calc_current_read(self)

    @calc_current.setter
    def calc_current(self, arg: List[float]):
        MetersV._calc_current_write(self, arg)

    @property
    def alloc_factors(self) -> List[float]:
        """Returns an array of doubles: allocation factors for the active Meter.

        Receives an array of doubles to set the phase allocation factors for the active Meter."""
        return MetersV._alloc_factors_read(self)

    @alloc_factors.setter
    def alloc_factors(self, arg: List[float]):
        MetersV._alloc_factors_write(self, arg)

    @property
    def all_end_elements(self) -> List[str]:
        """Returns a variant array of names of all zone end elements."""
        return MetersV._all_end_elements(self)

    @property
    def all_branches_in_zone(self) -> List[str]:
        """Returns a wide string list of all branches in zone of the active Energy Meter object."""
        return MetersV._all_branches_in_zone(self)

    @property
    def all_pce_in_zone(self) -> List[str]:
        """This parameter returns a wide string list of all the PCE in zone of the active Energy Meter object."""
        return MetersV._all_pce_in_zone(self)

