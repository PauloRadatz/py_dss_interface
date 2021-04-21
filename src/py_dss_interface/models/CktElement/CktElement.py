# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class CktElement(Base):

    # CktElementI (int)
    def cktelement_num_terminals(self):
        """Deliver the number of terminals of the active DSS object."""
        result = int(self.dss_obj.CktElementI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def cktelement_num_conductors(self):
        """Deliver the number of conductors of the active DSS object."""
        result = int(self.dss_obj.CktElementI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def cktelement_num_phases(self):
        """Delivers the number of phases of the active DSS object."""
        result = int(self.dss_obj.CktElementI(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result

    def cktelement_open(self):
        """Open the specified terminal (Argument) of the active DSS object."""
        result = int(self.dss_obj.CktElementI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def cktelement_close(self):
        """Close the specified terminal (Argument) of the active DSS object."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def cktelement_is_open(self):
        """Return a 1 if any terminal of the active DSS object is open, otherwise, it will return a 0."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def cktelement_num_properties(self):
        """Return the number of properties of the active DSS object."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def cktelement_has_switch_control(self):
        """Returns 1 if the active DSS object has a Switch Control linked; otherwise, it will return 0."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def cktelement_has_volt_control(self):
        """Returns 1 if the active DSS object has a Volt Control linked; otherwise, it will return 0."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def cktelement_num_controls(self):
        """Returns number of controls linked to the active DSS object."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def cktelement_ocp_dev_index(self):
        """Returns the Index into Controller list of OCP Device controlling the active DSS object."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def cktelement_ocp_dev_type(self):
        """Returns one of the following values: 0=none; 1=Fuse; 2=Recloser; 3=Relay according to the type of active
        control."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def cktelement_read_enabled(self):
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
        enabled."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def cktelement_write_enabled(self, argument):
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
        enabled."""
        result = self.dss_obj.CktElementI(ctypes.c_int32(13), ctypes.c_int32(argument))
        return result

    # CktElementF (Float)
    def cktelement_read_norm_amps(self):
        """Deliver the normal ampere rating for the active PDElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def cktelement_write_norm_amps(self, argument):
        """Allows to fix the normal ampere rating for the active PDElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def cktelement_read_emerg_amps(self):
        """Deliver the Emergency ampere rating for the active PDElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def cktelement_write_emerg_amp(self, argument):
        """Allows to fix the Emergency ampere rating for the active PDElement. The new value must be defined in the
        variable ?Argument?."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def cktelement_variable_i(self):
        """Delivers get the value of a variable by index for the active PCElement."""
        result = float(self.dss_obj.CktElementF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    # CktElementS (String)
    def cktelement_name(self):
        """Delivers the full name of the active circuit element."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_read_display(self):
        """Displays the name of the active circuit element (not necessarily unique)."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_write_display(self, argument):
        """Allows to modify the name of the active circuit element (not necessarily unique)."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def cktelement_guid(self):
        """Delivers the unique name for the active circuit element."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_energymeter(self):
        """Delivers the name of the EnergyMeter linked to the active circuit element."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_controller(self):
        """Delivers the Full name of the i-th controller attached to the active circuit element.
        The i-th controller index must be specified in the argument arg. Ex: Str = Controller(2).
        See NumControls to determine valid index range."""
        result = ctypes.c_char_p(self.dss_obj.CktElementS(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # CktElementV (Variant)
    def cktelement_read_bus_names(self):
        """Delivers an array of strings with the names of all the buses connected to the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_write_bus_names(self, argument):
        """Allows to fix an array of strings with the names of all the buses connected to the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.CktElementV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_voltages(self):
        """Delivers an array of doubles with the voltages at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_currents(self):
        """Delivers an array of doubles with the currents at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_powers(self):
        """Delivers an array of doubles with the powers at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_losses(self):
        """Delivers an array of doubles with the Losses at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_phase_losses(self):
        """Delivers an array of doubles with the Losses per phase at the terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seq_voltages(self):
        """Delivers an array of doubles with the symmetrical component voltages per phase at the terminals of the active
         circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seq_currents(self):
        """Delivers an array of doubles with the symmetrical component Currents per phase at the terminals of the active
         circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seq_powers(self):
        """Delivers an array of doubles with the symmetrical component powers per phase at the terminals of the active
        circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(9), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_all_property_names(self):
        """Delivers an array of strings with the names of all the properties of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(10), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_residuals(self):
        """Delivers an array of doubles with the residual currents (magnitude, angle) in all the nodes of the active
        circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(11), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_y_prim(self):
        """Delivers an array of doubles with the Y primitive matrix (complex) of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(12), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_cplx_seq_voltages(self):
        """Delivers an array of doubles with the complex of sequence voltages for all terminals of the active circuit
        element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(13), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_cplx_seq_currents(self):
        """Delivers an array of doubles with the complex of sequence currents for all terminals of the active circuit
        element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(14), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_all_variables_names(self):
        """Delivers a Variant array of strings listing all the published state variable names, if the active circuit
        element is a PCElement. Otherwise, null string."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(15), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_all_variables_values(self):
        """Delivers a Variant array of doubles listing all the values of the state variables, if the active circuit
        element is a PCElement. Otherwise, null string."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(16), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_node_order(self):
        """Delivers a Variant array integers variant array of integer containing the node numbers (representing phases,
        for example) for each conductor of each terminal."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(17), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_currents_mag_ang(self):
        """Delivers the currents in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(18), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_voltages_mag_ang(self):
        """Delivers the voltages in magnitude, angle format as a variant array of doubles of the active circuit
        element. """
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.CktElementV(ctypes.c_int(19), variant_pointer)
        return variant_pointer.contents.value
