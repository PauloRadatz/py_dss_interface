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

    def controller(self, argument: str) -> str:
        """Delivers the Full name of the i-th controller attached to the active circuit element.
                The i-th controller index must be specified in the argument arg. Ex: Str = Controller(2).
                See NumControls to determine valid index range."""
        return CktElementS._controller(self, argument)

    def enabled(self, argument: int) -> int:
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
                enabled."""
        return CktElementI._enabled(self, argument)

    def variable_i(self, argument: float) -> float:
        """Delivers get the value of a variable by index for the active PCElement."""
        return CktElementF._variable_i(self, argument)

    def open_terminal(self, argument: int) -> int:
        """Open the specified terminal (Argument) of the active DSS object."""
        return CktElementI._open_terminal(self, argument)

    def close_terminal(self, argument: int) -> int:
        """Close the specified terminal (Argument) of the active DSS object."""
        return CktElementI._close_terminal(self, argument)

    @property
    def norm_amps(self):
        """Deliver the normal ampere rating for the active PDElement.
                Allows to fix the normal ampere rating for the active PDElement."""
        return CktElementF._norm_amps(self)

    @norm_amps.setter
    def norm_amps(self, argument: float):
        CktElementF._norm_amps_write(self, argument)

    @property
    def emerg_amps(self) -> float:
        """Deliver the Emergency ampere rating for the active PDElement.

        Allows to fix the Emergency ampere rating for the active PDElement. The new value must be defined in the
                variable ?Argument?."""
        return CktElementF._emerg_amps(self)

    @emerg_amps.setter
    def emerg_amps(self, argument: float):
        CktElementF._emerg_amps_write(self, argument)

    @property
    def num_terminals(self) -> int:
        """Deliver the number of terminals of the active DSS object."""
        return CktElementI._num_terminals(self)

    @property
    def num_conductors(self) -> int:
        """Deliver the number of conductors of the active DSS object."""
        return CktElementI._num_conductors(self)

    @property
    def num_phases(self) -> int:
        """Delivers the number of phases of the active DSS object."""
        return CktElementI._num_phases(self)

    @property
    def is_terminal_open(self) -> int:
        """Return a 1 if any terminal of the active DSS object is open, otherwise, it will return a 0."""
        return CktElementI._is_terminal_open(self)

    @property
    def num_properties(self) -> int:
        """Return the number of properties of the active DSS object."""
        return CktElementI._num_properties(self)

    @property
    def has_switch_control(self) -> int:
        """Returns 1 if the active DSS object has a Switch Control linked; otherwise, it will return 0."""
        return CktElementI._has_switch_control(self)

    @property
    def has_volt_control(self) -> int:
        """Returns 1 if the active DSS object has a Volt Control linked; otherwise, it will return 0."""
        return CktElementI._has_volt_control(self)

    @property
    def num_controls(self) -> int:
        """Returns number of controls linked to the active DSS object."""
        return CktElementI._num_controls(self)

    @property
    def ocp_dev_index(self) -> int:
        """Returns the Index into Controller list of OCP Device controlling the active DSS object."""
        return CktElementI._ocp_dev_index(self)

    @property
    def ocp_dev_type(self) -> int:
        """Returns one of the following values: 0=none; 1=Fuse; 2=Recloser; 3=Relay according to the type of active
                control."""
        return CktElementI._ocp_dev_type(self)

    @property
    def is_enabled(self) -> int:
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
                enabled."""
        return CktElementI._is_enabled(self)

    @property
    def name(self) -> str:
        """Delivers the full name of the active circuit element."""
        return CktElementS._name(self)

    @property
    def display(self) -> str:
        """Displays the name of the active circuit element (not necessarily unique).

        Allows to modify the name of the active circuit element (not necessarily unique)."""
        return CktElementS._display(self)

    @display.setter
    def display(self, argument: str):
        CktElementS._display_write(self, argument)

    @property
    def guid(self) -> str:
        """Delivers the unique name for the active circuit element."""
        return CktElementS._guid(self)

    @property
    def energymeter(self) -> str:
        """Delivers the name of the EnergyMeter linked to the active circuit element."""
        return CktElementS._energymeter(self)

    @property
    def bus_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the buses connected to the active circuit element.

        Allows to fix an array of strings with the names of all the buses connected to the active circuit element."""
        return CktElementV._bus_names(self)

    @bus_names.setter
    def bus_names(self, argument):
        CktElementV._bus_names_write(self, argument)

    @property
    def voltages(self) -> List[float]:
        """Delivers an array of doubles with the voltages at terminals of the active circuit element."""
        return CktElementV._voltages(self)

    @property
    def currents(self) -> List[float]:
        """Delivers an array of doubles with the currents at terminals of the active circuit element."""
        return CktElementV._currents(self)

    @property
    def powers(self) -> List[float]:
        """Delivers an array of doubles with the powers at terminals of the active circuit element."""
        return CktElementV._powers(self)

    @property
    def losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses at terminals of the active circuit element."""
        return CktElementV._losses(self)

    @property
    def phase_losses(self) -> List[float]:
        """Delivers an array of doubles with the Losses per phase at the terminals of the active circuit element."""
        return CktElementV._phase_losses(self)

    @property
    def seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component voltages per phase at the terminals of the active
                 circuit element."""
        return CktElementV._seq_voltages(self)

    @property
    def seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component Currents per phase at the terminals of the active
                 circuit element."""
        return CktElementV._seq_currents(self)

    @property
    def seq_powers(self) -> List[float]:
        """Delivers an array of doubles with the symmetrical component powers per phase at the terminals of the active
                circuit element."""
        return CktElementV._seq_powers(self)

    @property
    def property_names(self) -> List[str]:
        """Delivers an array of strings with the names of all the properties of the active circuit element."""
        return CktElementV._property_names(self)

    @property
    def residuals_currents(self) -> List[float]:
        """Delivers an array of doubles with the residual currents (magnitude, angle) in all the nodes of the active
                circuit element."""
        return CktElementV._residuals_currents(self)

    @property
    def y_prim(self) -> List[float]:
        """Delivers an array of doubles with the Y primitive matrix (complex) of the active circuit element."""
        return CktElementV._y_prim(self)

    @property
    def cplx_seq_voltages(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence voltages for all terminals of the active circuit
                element."""
        return CktElementV._cplx_seq_voltages(self)

    @property
    def cplx_seq_currents(self) -> List[float]:
        """Delivers an array of doubles with the complex of sequence currents for all terminals of the active circuit
                element."""
        return CktElementV._cplx_seq_currents(self)

    @property
    def variables_names(self) -> List[str]:
        """Delivers a Variant array of strings listing all the published state variable names, if the active circuit
                element is a PCElement. Otherwise, null string."""
        return CktElementV._variables_names(self)

    @property
    def variables_values(self) -> List[float]:
        """Delivers a Variant array of doubles listing all the values of the state variables, if the active circuit
                element is a PCElement. Otherwise, null string."""
        return CktElementV._variables_values(self)

    @property
    def node_order(self) -> List[int]:
        """Delivers a Variant array integers variant array of integer containing the node numbers (representing phases,
                for Example) for each conductor of each terminal."""
        return CktElementV._node_order(self)

    @property
    def currents_mag_ang(self) -> List[float]:
        """Delivers the currents in magnitude, angle format as a variant array of doubles of the active circuit
                element. """
        return CktElementV._currents_mag_ang(self)

    @property
    def voltages_mag_ang(self) -> List[float]:
        """Delivers the voltages in magnitude, angle format as a variant array of doubles of the active circuit
                element. """
        return CktElementV._voltages_mag_ang(self)
