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
        """Returns the name of the active bus."""
        return BusS._name(self)

    @property
    def num_nodes(self) -> int:
        """Returns the number of nodes of this bus."""
        return BusI._num_nodes(self)

    @property
    def zsc_refresh(self) -> int:
        """Recomputes Zsc for active bus for present circuit configuration. Return 1 if the procedure was successful."""
        return BusI._zsc_refresh(self)

    @property
    def coord_defined(self) -> int:
        """Returns 1 if a coordinate has been defined for this bus; otherwise, it will return 0."""
        return BusI._coord_defined(self)

    @property
    def unique_node_number(self) -> int:
        """Returns a unique node number at the active bus to avoid node collisions and adds it to the node list for
                the bus. The start number can be specified in the second parameter."""
        return BusI._unique_node_number(self, 1)

    @unique_node_number.setter
    def unique_node_number(self, start_number: int):
        BusI._unique_node_number(self, start_number)

    @property
    def total_customers(self) -> int:
        """Returns returns the total number of customers served down line from this bus."""
        return BusI._total_customers(self)

    @property
    def section_id(self) -> int:
        """Returns the integer ID of the feeder section in which this bus is located."""
        return BusI._section_id(self)

    @property
    def voltages(self) -> List[float]:
        """Returns a complex array of voltages at this bus."""
        return BusV._voltages(self)

    @property
    def seq_voltages(self) -> List[float]:
        """Returns a complex array of Sequence voltages at this bus."""
        return BusV._seq_voltages(self)

    @property
    def nodes(self) -> List[int]:
        """Returns an integer array of node numbers defined at the bus in same order as the voltages."""
        return BusV._nodes(self)

    @property
    def voc(self) -> List[float]:
        """Returns the open circuit voltage as complex array."""
        return BusV._voc(self)

    @property
    def isc(self) -> List[float]:
        """Returns the short circuit current as complex array."""
        return BusV._isc(self)

    @property
    def pu_voltages(self) -> List[float]:
        """Returns the voltages in per unit at bus as complex array."""
        return BusV._pu_voltages(self)

    @property
    def zsc_matrix(self) -> List[float]:
        """Returns the complex array of Zsc matrix at bus, column by column."""
        return BusV._zsc_matrix(self)

    @property
    def zsc1(self) -> List[float]:
        """Returns the complex positive-sequence short circuit impedance at bus."""
        return BusV._zsc1(self)

    @property
    def zsc0(self) -> List[float]:
        """Returns the complex zero-sequence short circuit impedance at bus."""
        return BusV._zsc0(self)

    @property
    def ysc_matrix(self) -> List[float]:
        """Returns the complex array of Ysc matrix at bus, column by column."""
        return BusV._ysc_matrix(self)

    @property
    def cplx_sequence_voltages(self) -> List[float]:
        """Returns the complex double array of sequence voltages (0, 1, 2) at this bus."""
        return BusV._cplx_sequence_voltages(self)

    @property
    def vll(self) -> List[float]:
        """For 2 and 3 phase buses, returns a variant array of complex numbers representing L-L voltages in volts.
                Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3."""
        return BusV._vll(self)

    @property
    def pu_vll(self) -> List[float]:
        """Returns a variant array of complex numbers representing L-L voltages in per unit. Returns -1.0 for 1-phase
                bus. If more than 3 phases, returns only first 3. """
        return BusV._pu_vll(self)

    @property
    def vmag_angle(self) -> List[float]:
        """Returns a variant array of doubles containing voltages in magnitude (VLN), angle (deg)."""
        return BusV._vmag_angle(self)

    @property
    def vmag_angle_pu(self) -> List[float]:
        """Returns a variant array of doubles containing voltages in per unit and angles in degrees."""
        return BusV._vmag_angle_pu(self)

    @property
    def line_list(self) -> List[str]:
        """Returns a variant array of strings containing the names of the lines connected to the
                active bus. The names of the lines include the class name 'Line.' """
        return BusV._line_list(self)

    @property
    def load_list(self) -> List[str]:
        """This parameter returns a variant array of strings containing the names of the loads connected to the
                active bus. The names of the lines include the class name 'Load.'. """
        return BusV._load_list(self)

    @property
    def axc_012_matrix(self) -> List[float]:
        """Variant array of doubles (complex) containing the complete 012 Zsc matrix."""
        return BusV._axc_012_matrix(self)

    @property
    def all_pce_active_bus(self) -> List[str]:
        """Returns an array with the names of all PCE connected to the active bus."""
        return BusV._all_pce_active_bus(self)

    @property
    def all_pde_active_bus(self) -> List[str]:
        """Returns an array with the names of all PDE connected to the active bus."""
        return BusV._all_pde_active_bus(self)

    @property
    def kv_base(self) -> float:
        """Returns the base voltage at bus in kV."""
        return BusF._kv_base(self)

    @property
    def x(self) -> float:
        """Returns the X coordinate for the bus.
        Allows to write the X coordinate for the bus. Returns 0."""
        return BusF._x(self)

    @x.setter
    def x(self, value: float):
        BusF._x_write(self, value)

    @property
    def y(self) -> float:
        """Returns the Y coordinate for the bus.
        Allows to write the Y coordinate for the bus. Returns 0."""
        return BusF._y(self)

    @y.setter
    def y(self, value: float):
        BusF._y_write(self, value)

    @property
    def distance(self) -> float:
        """Returns the distance from the energymeter (if non-zero)."""
        return BusF._distance(self)

    @property
    def bus_lambda(self) -> float:
        """Returns the accumulated failure rate downstream from this bus, faults per year."""
        return BusF._bus_lambda(self)

    @property
    def interruptions_num(self) -> float:
        """Returns the number of interruptions this bus per year."""
        return BusF._interruptions_num(self)

    @property
    def interruptions_avg_duration(self) -> float:
        """Returns the average interruption duration in hours."""
        return BusF._interruptions_avg_duration(self)

    @property
    def interruptions_total_customers(self) -> float:
        """Returns the annual number of customer interruptions from this bus."""
        return BusF._interruptions_total_customers(self)

    @property
    def outage_customer_accum_duration(self) -> float:
        """Returns the accumulated customer outage durations."""
        return BusF._outage_customer_accum_duration(self)

    @property
    def line_total_miles(self) -> float:
        """Returns the total length of line downline from this bus, in miles. For recloser siting algorithm."""
        return BusF._line_total_miles(self)

    @property
    def latitude(self) -> float:
        """Returns the GIS latitude assigned to the active bus (if any).
        Sets the GIS latitude to the active bus using the value given at the argument."""
        return BusF._latitude(self)

    @latitude.setter
    def latitude(self, value: float):
        BusF._latitude_write(self, value)

    @property
    def longitude(self) -> float:
        """Returns the GIS longitude assigned to the active bus (if any).
        Sets the GIS longitude to the active bus using the value given at the argument."""
        return BusF._longitude(self)

    @longitude.setter
    def longitude(self, value: float):
        BusF._longitude_write(self, value)
