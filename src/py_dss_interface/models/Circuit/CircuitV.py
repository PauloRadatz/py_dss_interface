# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from py_dss_interface.models import Bridge
import ctypes

from comtypes import automation

from py_dss_interface.models.Base import Base


class CircuitV(Base):

    # CircuitV (Variant)
    def circuit_losses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total losses of the
        active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(0), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_line_losses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total Line losses of
        the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(1), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_substation_losses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total transformer
        losses of the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(2), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_total_power(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total power in watts
        delivered to the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(3), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

        # aqui = Bridge.VarArrayFunction(self.dss_obj.CircuitV, ctypes.c_int32(3), ctypes.c_int32(0), None)
        # print('Resultado: {0}'.format(aqui))
        # return aqui

    def circuit_all_bus_volts(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the node voltages from
        the most recent solution. Argument2 must be 0."""

        teste = ctypes.pointer(automation.VARIANT())

        self.dss_obj.CircuitV(ctypes.c_int32(4), teste)

        return teste.contents.value

    def circuit_all_bus_vmag(self):
        """Returns an array of doubles (magnitude) with the node voltages from the most recent solution.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(5), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_element_names(self):
        """Returns an array of strings with the names of all the elements of the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(6), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_bus_names(self):
        """Returns an array of strings with the names of all the Buses of the active circuit (See AllNodeNames).
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(7), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_element_losses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the losses in each element
        of the active circuit.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(8), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_bus_vmag_pu(self):
        """Returns an array of doubles with the voltages in per unit of the most recent solution of the active circuit.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(9), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_node_names(self):
        """Returns an array of strings containing full name of each node in system in same order as returned by
        AllBusVolts, etc.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(10), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_system_y(self):
        """Returns an array of doubles (two doubles for representing a complex number) containing the Y Bus Matrix of
        the system (after a solution has been performed).
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(11), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_bus_distances(self):
        """Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(12), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_node_distances(self):
        """Returns distance from each Node to parent EnergyMeter. Corresponds to sequence in AllBusVmag.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(13), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_all_node_vmag_by_phase(self, argument):
        """Returns array of doubles representing the voltage magnitudes for nodes on the specified phase.
        The phase must be specified in the Argument2."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(14), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_all_node_vmag_pu_by_phase(self, argument):
        """Returns array of doubles representing the voltage magnitudes (in per unit) for nodes on the specified phase.
        The phase must be specified in the Argument2."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(15), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_all_node_distances_by_phase(self, argument):
        """Returns array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to
        other node ByPhase properties. Argument2 must contain the number of the phase to return."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(16), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_all_node_names_by_phase(self, argument):
        """Returns array of strings of the node names by Phase criteria. Sequence corresponds to other ByPhase properties.
        Argument2 must contain the number of the phase to return."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(17), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_y_node_varray(self):
        """Returns a complex array of actual node voltages in same order as SystemY Matrix. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(18), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_y_node_order(self):
        """Returns a variant array of strings containing the names of the nodes in the same order as the Y Matrix.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(19), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_y_currents(self):
        """Returns a variant array of doubles containing complex injection currents for the present solution.
        It is the "I" vector of I=YV. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CircuitV(ctypes.c_int32(20), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value
