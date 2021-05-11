# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Base import Base


class BusV(Base):
    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void BUSV(int32_t Parameter, VARIANT *Argument)

    This interface returns a variant according to the number sent in the variable “parameter”. The parameter can be
    one of the following.
    """

    def bus_voltages(self):
        """Returns a complex array of voltages at this bus."""
        return self.get_variant(0)

    # TODO: Checar duplicidade com o método desta classe nomeado sequence_voltages
    def bus_seqvoltages(self):
        """Returns a complex array of Sequence voltages at this bus."""
        return self.get_variant(1)

    def bus_nodes(self) -> List[int]:
        """Returns an integer array of node numbers defined at the bus in same order as the voltages."""
        return self.get_variant(2)

    def bus_voc(self):
        """Returns the open circuit voltage as complex array."""
        return self.get_variant(3)

    def bus_isc(self):
        """Returns the short circuit current as complex array."""
        return self.get_variant(4)

    def bus_pu_voltages(self):
        """Returns the voltages in per unit at bus as complex array."""
        return self.get_variant(5)

    def bus_zsc_matrix(self):
        """Returns the complex array of Zsc matrix at bus, column by column."""
        return self.get_variant(6)

    def bus_zsc1(self):
        """Returns the complex array of Zsc matrix at bus, column by column."""
        return self.get_variant(7)

    def bus_zsc0(self):
        """Returns the complex zero-sequence short circuit impedance at bus."""
        return self.get_variant(8)

    def bus_ysc_matrix(self):
        """Returns the complex array of Ysc matrix at bus, column by column."""
        return self.get_variant(9)

    def bus_sequence_voltages(self):
        """Returns the complex double array of sequence voltages (0, 1, 2) at this bus."""
        return self.get_variant(10)

    def bus_vll(self):
        """For 2 and 3 phase buses, returns a variant array of complex numbers representing L-L voltages in volts.
        Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3."""
        return self.get_variant(11)

    def bus_pu_vll(self):
        """Returns a variant array of complex numbers representing L-L voltages in per unit. Returns -1.0 for 1-phase bus.
        If more than 3 phases, returns only first 3.."""
        return self.get_variant(12)

    def bus_vmag_angle(self):
        """Returns a variant array of doubles containing voltages in magnitude (VLN), angle (deg)."""
        return self.get_variant(13)

    def bus_pu_vmag_angle(self):
        """Returns a variant array of doubles containing voltages in per unit and angles in degrees."""
        return self.get_variant(14)

    def bus_line_list(self):
        """This parameter returns a variant array of strings containing the names of the lines connected to the
        active bus. The names of the lines include the class name 'Line.' """
        return self.get_variant(15)

    def bus_load_list(self):
        """This parameter returns a variant array of strings containing the names of the loads connected to the
        active bus. The names of the lines include the class name 'Load.'. """
        return self.get_variant(16)

    def bus_axc_012_matrix(self):
        """Variant array of doubles (complex) containing the complete 012 Zsc matrix."""
        return self.get_variant(17)

    def bus_all_pace_active_bus(self) -> List[str]:
        """Returns an array with the names of all PCE connected to the active bus."""
        return self.get_variant(18)

    def bus_all_pde_active_bus(self) -> List[str]:
        """Returns an array with the names of all PDE connected to the active bus."""
        return self.get_variant(19)
