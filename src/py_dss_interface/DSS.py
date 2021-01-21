# -*- coding: iso-8859-15 -*-

import sys
import ctypes
from comtypes import automation
import os
import platform
import pathlib


class DSSDLL:

    def __init__(self, dll_folder=None, dll_name="OpenDSSDirect.dll"):
        """
        Class to create an OpenDSS object
        :param dll_folder: None will use the OpenDSS available within the package. The DDLL path allows to use a different OpenDSS
        """

        if dll_folder == None:
            script_path = os.path.dirname(os.path.abspath(__file__))
            dll_folder = os.path.join(pathlib.Path(script_path), "DDLL")

        self.opendss_started = False

        if platform.architecture()[0] == "64bit":
            dll64_path = os.path.join(dll_folder, "x64")
            if not dll64_path in os.environ['PATH']:
                os.environ['PATH'] = dll64_path + os.pathsep + os.environ['PATH']
            try:
                os.chdir(os.path.join(dll_folder, "x64"))
                self.dssObj = ctypes.WinDLL(dll_name)
                self.opendss_started = True
            except:
                os.chdir(os.path.join(dll_folder, "x64"))
                self.dssObj = ctypes.WinDLL(os.path.join(dll_folder, "x64", dll_name))
                self.opendss_started = True
        elif platform.architecture()[0] == "32bit":
            dll86_path = os.path.join(dll_folder, "x86")
            if not dll86_path in os.environ['PATH']:
                os.environ['PATH'] = dll86_path + os.pathsep + os.environ['PATH']
            os.chdir(os.path.join(dll_folder, "x86"))
            self.dssObj = ctypes.CDLL(os.path.join(dll_folder, "x86", dll_name))
            self.opendss_started = True
        else:
            print("Make sure you are using the OpenDSS DLL and Python with the same bits")

        self._allocate_memory()

        if int(self.dssObj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0))) == 1:
            self.dss_version = ctypes.c_char_p(self.dssObj.DSSS(ctypes.c_int32(1), "".encode('ascii')))
            print("OpenDSS Started successfully! \nOpenDSS {}\n\n".format(self.dss_version.value.decode('ascii')))
        else:
            self.dss_version = None
            print("OpenDSS Failed to Start")

    def _allocate_memory(self):
        self.dssObj.DSSPut_Command.restype = ctypes.c_char_p

        self.dssObj.ActiveClassS.restype = ctypes.c_char_p

        self.dssObj.BUSF.restype = ctypes.c_double
        self.dssObj.BUSS.restype = ctypes.c_char_p

        self.dssObj.CapacitorsF.restype = ctypes.c_double
        self.dssObj.CapacitorsS.restype = ctypes.c_char_p

        self.dssObj.CircuitF.restype = ctypes.c_double
        self.dssObj.CircuitS.restype = ctypes.c_char_p

        self.dssObj.CapControlsF.restype = ctypes.c_double
        self.dssObj.CapControlsS.restype = ctypes.c_char_p

        self.dssObj.CktElementF.restype = ctypes.c_double
        self.dssObj.CktElementS.restype = ctypes.c_char_p

        self.dssObj.DSSS.restype = ctypes.c_char_p

        self.dssObj.DSSElementS.restype = ctypes.c_char_p

        self.dssObj.DSSProgressS.restype = ctypes.c_char_p

        self.dssObj.DSSExecutiveS.restype = ctypes.c_char_p

        self.dssObj.DSSProperties.restype = ctypes.c_char_p

        self.dssObj.FusesF.restype = ctypes.c_double
        self.dssObj.FusesS.restype = ctypes.c_char_p

        self.dssObj.GeneratorsF.restype = ctypes.c_double
        self.dssObj.GeneratorsS.restype = ctypes.c_char_p

        self.dssObj.IsourceF.restype = ctypes.c_double
        self.dssObj.IsourceS.restype = ctypes.c_char_p

        self.dssObj.LinesF.restype = ctypes.c_double
        self.dssObj.LinesS.restype = ctypes.c_char_p

        self.dssObj.LineCodesF.restype = ctypes.c_double
        self.dssObj.LineCodesS.restype = ctypes.c_char_p

        self.dssObj.DSSLoadsF.restype = ctypes.c_double
        self.dssObj.DSSLoadsS.restype = ctypes.c_char_p

        self.dssObj.LoadShapeF.restype = ctypes.c_double
        self.dssObj.LoadShapeS.restype = ctypes.c_char_p

        self.dssObj.MetersF.restype = ctypes.c_double
        self.dssObj.MetersS.restype = ctypes.c_char_p

        self.dssObj.MonitorsS.restype = ctypes.c_char_p

        self.dssObj.ParserF.restype = ctypes.c_double
        self.dssObj.ParserS.restype = ctypes.c_char_p

        self.dssObj.PDElementsF.restype = ctypes.c_double
        self.dssObj.PDElementsS.restype = ctypes.c_char_p

        self.dssObj.PVsystemsF.restype = ctypes.c_double
        self.dssObj.PVsystemsS.restype = ctypes.c_char_p

        self.dssObj.ReclosersF.restype = ctypes.c_double
        self.dssObj.ReclosersS.restype = ctypes.c_char_p

        self.dssObj.RegControlsF.restype = ctypes.c_double
        self.dssObj.RegControlsS.restype = ctypes.c_char_p

        self.dssObj.RelaysS.restype = ctypes.c_char_p

        self.dssObj.SensorsF.restype = ctypes.c_double
        self.dssObj.SensorsS.restype = ctypes.c_char_p

        self.dssObj.SettingsF.restype = ctypes.c_double
        self.dssObj.SettingsS.restype = ctypes.c_char_p

        self.dssObj.SolutionF.restype = ctypes.c_double
        self.dssObj.SolutionS.restype = ctypes.c_char_p

        self.dssObj.SwtControlsF.restype = ctypes.c_double
        self.dssObj.SwtControlsS.restype = ctypes.c_char_p

        self.dssObj.TopologyS.restype = ctypes.c_char_p

        self.dssObj.TransformersF.restype = ctypes.c_double
        self.dssObj.TransformersS.restype = ctypes.c_char_p

        self.dssObj.VsourcesF.restype = ctypes.c_double
        self.dssObj.VsourcesS.restype = ctypes.c_char_p

        self.dssObj.XYCurvesF.restype = ctypes.c_double
        self.dssObj.XYCurvesS.restype = ctypes.c_char_p


# ActiveClass Interface

    # ActiveClassI (int)
    def activeclass_first(self):
        """Sets first element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element. Returns 0 if none."""
        result = self.dssObj.ActiveClassI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def activeclass_next(self):
        """Sets next element in the active class to be the active DSS object.
        If object is a CktElement, ActiveCktElement also points to this element. Returns 0 if none."""
        result = self.dssObj.ActiveClassI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def activeclass_numelements(self):
        """Gets the number of elements in this class. Same as Count Property."""
        result = self.dssObj.ActiveClassI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def activeclass_count(self):
        """Gets the number of elements in this class. Same as NumElements Property."""
        result = self.dssObj.ActiveClassI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    # ActiveClassS (String)
    def activeclass_read_name(self):
        """Gets the name of the active Element of the Active class."""
        result = ctypes.c_char_p(self.dssObj.ActiveClassS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def activeclass_write_name(self, argument):
        """Sets the name of the active Element of the Active class."""
        result = ctypes.c_char_p(self.dssObj.ActiveClassS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def activeclass_activeclassname(self):
        """Sets the name of the active Element of the Active class."""
        result = ctypes.c_char_p(self.dssObj.ActiveClassS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # ActiveClassV (Variant)
    def activeclass_allnames(self):
        """Gets a variant array of strings consisting of all element names in the active Class."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ActiveClassV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# Bus Interface
    # BusI (int)
    def bus_numnodes(self):
        """Returns the number of nodes of this bus."""
        result = int(self.dssObj.BUSI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def bus_zscrefresh(self):
        """Recomputes Zsc for active bus for present circuit configuration.
        Return 1 if the procedure was successful."""
        result = int(self.dssObj.BUSI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def bus_coorddefined(self):
        """Returns 1 if a coordinate has been defined for this bus; otherwise, it will return 0."""
        result = int(self.dssObj.BUSI(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result

    def bus_getuniquenodenumber(self):
        """Returns a unique node number at the active bus to avoid node collisions and adds it to the node list for the bus.
        The start number can be specified in the argument."""
        result = int(self.dssObj.BUSI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def bus_n_customers(self):
        """Returns returns the total number of customers served down line from this bus."""
        result = int(self.dssObj.BUSI(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result

    def bus_sectionid(self):
        """Returns the integer ID of the feeder section in which this bus is located."""
        result = int(self.dssObj.BUSI(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result

    # BusF (Float)
    def bus_kVbase(self):
        """Returns the base voltage at bus in kV."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def bus_read_x(self):
        """Returns the X coordinate for the bus."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    def bus_write_x(self, argument):
        """Allows to write the X coordinate for the bus. Returns 0."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(2), ctypes.c_double(argument)))
        return result

    def bus_read_y(self):
        """Returns the X coordinate for the bus."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def bus_write_y(self, argument):
        """Allows to write the Y coordinate for the bus. Returns 0."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(4), ctypes.c_double(argument)))
        return result

    def bus_distance(self):
        """Returns the distance from the energymeter (if non-zero)."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def bus_lambda(self):
        """Returns the accumulated failure rate downstream from this bus; faults per year."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def bus_ninterrupts(self):
        """Returns the number of interruptions this bus per year."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    def bus_intduration(self):
        """Returns the average interruption duration in hours."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def bus_custinterrupts(self):
        """Returns the annual number of customer interruptions from this bus."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(9), ctypes.c_double(0)))
        return result

    def bus_custduration(self):
        """Returns the accumulated customer outage durations."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def bus_totalmiles(self):
        """Returns the total length of line downline from this bus, in miles. For recloser siting algorithm."""
        result = float(self.dssObj.BUSF(ctypes.c_int32(11), ctypes.c_double(0)))
        return result

    # BusS (String)
    def bus_name(self):
        """Returns the name of the active bus."""
        result = ctypes.c_char_p(self.dssObj.BUSS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # BusV (Variant)
    def bus_voltages(self):
        """Returns a complex array of voltages at this bus."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value

    def bus_seqvoltages(self):
        """Returns a complex array of Sequence voltages at this bus."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(1), variant_pointer)
        return variant_pointer.contents.value

    def bus_nodes(self):
        """Returns an integer array of node numbers defined at the bus in same order as the voltages."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(2), variant_pointer)
        return variant_pointer.contents.value

    def bus_voc(self):
        """Returns the open circuit voltage as complex array."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(3), variant_pointer)
        return variant_pointer.contents.value

    def bus_isc(self):
        """Returns the short circuit current as complex array."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(4), variant_pointer)
        return variant_pointer.contents.value

    def bus_puvoltages(self):
        """Returns the voltages in per unit at bus as complex array."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(5), variant_pointer)
        return variant_pointer.contents.value

    def bus_zscmatrix(self):
        """Returns the complex array of Zsc matrix at bus, column by column."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(6), variant_pointer)
        return variant_pointer.contents.value

    def bus_zsc1(self):
        """Returns the complex array of Zsc matrix at bus, column by column."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(7), variant_pointer)
        return variant_pointer.contents.value

    def bus_zsc0(self):
        """Returns the complex zero-sequence short circuit impedance at bus."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(8), variant_pointer)
        return variant_pointer.contents.value

    def bus_yscmatrix(self):
        """Returns the complex array of Ysc matrix at bus, column by column."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(9), variant_pointer)
        return variant_pointer.contents.value

    def bus_cplxseqvoltages(self):
        """Returns the complex double array of sequence voltages (0, 1, 2) at this bus."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(10), variant_pointer)
        return variant_pointer.contents.value

    def bus_vll(self):
        """For 2 and 3 phase buses, returns a variant array of complex numbers representing L-L voltages in volts.
        Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(11), variant_pointer)
        return variant_pointer.contents.value

    def bus_puvll(self):
        """Returns a variant array of complex numbers representing L-L voltages in per unit. Returns -1.0 for 1-phase bus.
        If more than 3 phases, returns only first 3.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(12), variant_pointer)
        return variant_pointer.contents.value

    def bus_vmagangle(self):
        """Returns a variant array of doubles containing voltages in magnitude (VLN), angle (deg)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(13), variant_pointer)
        return variant_pointer.contents.value

    def bus_puvmagangle(self):
        """Returns a variant array of doubles containing voltages in per unit and angles in degrees."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.BUSV(ctypes.c_int32(14), variant_pointer)
        return variant_pointer.contents.value

# Capacitors Interface

    # CapacitorsI (int)
    def capacitors_read_numsteps(self):
        """Gets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def capacitors_write_numsteps(self, argument):
        """Sets the number of steps (defaults 1) for distributing and switching the total bank kvar."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def capacitors_read_isdelta(self):
        """Gets 1 if delta connection, otherwise will return 0 for distributing and switching the total kvar."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def capacitors_write_isdelta(self, argument):
        """Sets (Argument) 1 if delta connection, otherwise will return 0 for distributing and switching the total kvar."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def capacitors_first(self):
        """Sets the first capacitor active. Returns 0 if no more."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def capacitors_next(self):
        """Sets the next capacitor active. Returns 0 if no more."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def capacitors_count(self):
        """Gets the number of capacitor objects in active circuit."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def capacitors_addstep(self):
        """Adds one step of the capacitor if available. If successful returns 1."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def capacitors_subtractstep(self):
        """Subtracts one step of the capacitor if available. If no more steps, returns 0."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def capacitors_availablesteps(self):
        """Gets the number of steps available in cap bank to be switched ON."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def capacitors_open(self):
        """Opens all steps, all phases of the capacitor."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def capacitors_close(self):
        """Closes all steps, all phases of the capacitor."""
        result = self.dssObj.CapacitorsI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    # CapacitorsF (Float)
    def capacitors_read_kv(self):
        """Gets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        result = float(self.dssObj.CapacitorsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def capacitors_write_kv(self, argument):
        """Sets the bank rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase."""
        result = float(self.dssObj.CapacitorsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def capacitors_read_kvar(self):
        """Gets the total bank kvar, distributed equally among phases and steps."""
        result = float(self.dssObj.CapacitorsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def capacitors_write_kvar(self, argument):
        """Sets the total bank kvar, distributed equally among phases and steps."""
        result = float(self.dssObj.CapacitorsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    # CapacitorsS (String)
    def capacitors_read_name(self):
        """Gets the name of the active Capacitor element."""
        result = ctypes.c_char_p(self.dssObj.CapacitorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def capacitors_write_name(self, argument):
        """Sets the name of the Capacitor element to set it active."""
        result = ctypes.c_char_p(self.dssObj.CapacitorsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # CapacitorsV (Variant)
    def capacitors_allnames(self):
        """Gets a variant array of strings with all Capacitor names in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CapacitorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def capacitors_read_states(self):
        """Gets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CapacitorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def capacitors_write_states(self, argument):
        """Sets a variant array of integers [0..numsteps-1] indicating the state of each step.
        If value is -1 and error has occurred."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.CapacitorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# CapControls Interface

    # CapControlsI (int)
    def capcontrols_first(self):
        """Sets the first CapControl active. Returns 0 if no more."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def capcontrols_next(self):
        """Sets the next CapControl active. Returns 0 if no more."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def capcontrols_read_mode(self):
        """Gets the type of automatic controller (see manual for details)."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def capcontrols_write_mode(self, argument):
        """Sets the type of automatic controller (see manual for details)."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def capcontrols_read_monitoredterm(self):
        """Gets the terminal number on the element that PT and CT are connected to."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def capcontrols_write_monitoredterm(self, argument):
        """Sets the terminal number on the element that PT and CT are connected to."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def capcontrols_read_usevoltoverride(self):
        """Gets if Vmin and Vmax are enabled to override the control Mode."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def capcontrols_write_usevoltoverride(self, argument):
        """Sets if enables Vmin and Vmax to override the control Mode."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def capcontrols_count(self):
        """Gets the number of CapControls in Active Circuit."""
        result = self.dssObj.CapControlsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    # CapControlsF (Float)
    def capcontrols_read_ctratio(self):
        """Gets the transducer ratio current to control current."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def capcontrols_write_ctratio(self, argument):
        """Sets the transducer ratio current to control current."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def capcontrols_read_ptratio(self):
        """Gets the transducer ratio from primary feeder to control voltage."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def capcontrols_write_ptratio(self, argument):
        """Sets the transducer ratio from primary feeder to control voltage."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def capcontrols_read_onsetting(self):
        """Gets the threshold to arm or switch on a step. See Mode for Units."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def capcontrols_write_onsetting(self, argument):
        """Sets the threshold to arm or switch on a step. See Mode for Units."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def capcontrols_read_offsetting(self):
        """Gets the threshold to switch off a step. See Mode for Units."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def capcontrols_write_offsetting(self, argument):
        """Sets the threshold to switch off a step. See Mode for Units."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def capcontrols_read_vmax(self):
        """Gets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def capcontrols_write_vmax(self, argument):
        """Sets the Vmax, this reference with VoltOverride, switch off whenever PT voltage exceeds this level."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def capcontrols_read_vmin(self):
        """Gets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def capcontrols_write_vmin(self, argument):
        """Sets the Vmin, this reference with VoltOverride, switch ON whenever PT voltage drops below this level."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def capcontrols_read_delay(self):
        """Gets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def capcontrols_write_delay(self, argument):
        """Sets the time delay [s] to switch on after arming. Control may reset before actually switching."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def capcontrols_read_delayoff(self):
        """Gets the time delay [s] before switching off a step. Control may reset before actually switching."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def capcontrols_write_delayoff(self, argument):
        """Sets the time delay [s] before switching off a step. Control may reset before actually switching."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def capcontrols_read_deadtime(self):
        """Gets the time delay [s] after switching off a step. Control may reset before actually switching."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def capcontrols_write_deadtime(self, argument):
        """Sets the time delay [s] after switching off a step. Control may reset before actually switching.."""
        result = float(self.dssObj.CapControlsF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

     # CapControlsS (String)
    def capcontrols_read_name(self):
        """Gets the name of the active CapControl."""
        result = ctypes.c_char_p(self.dssObj.CapControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def capcontrols_write_name(self, argument):
        """Sets a CapControl active by name."""
        result = ctypes.c_char_p(self.dssObj.CapControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def capcontrols_read_capacitor(self):
        """Gets the name of the capacitor that is controlled."""
        result = ctypes.c_char_p(self.dssObj.CapControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def capcontrols_write_capacitor(self, argument):
        """Sets the name of the capacitor that is controlled."""
        result = ctypes.c_char_p(self.dssObj.CapControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def capcontrols_read_monitoredobj(self):
        """Gets the full name of the element that PT and CT are connected to."""
        result = ctypes.c_char_p(self.dssObj.CapControlsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def capcontrols_write_monitoredobj(self, argument):
        """Sets the full name of the element that PT and CT are connected to."""
        result = ctypes.c_char_p(self.dssObj.CapControlsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # CapControlsV (Variant)
    def capcontrols_allnames(self):
        """Gets a variant array of string with all CapControl names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CapControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# Circuit Interface

    # CircuitI (int)
    def circuit_numcktelements(self):
        """Will deliver the number of CktElements included in the active circuit."""
        result = self.dssObj.CircuitI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def circuit_numbuses(self):
        """Will deliver the number of buses included in the active circuit."""
        result = self.dssObj.CircuitI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def circuit_numnodes(self):
        """Will deliver the number of nodes included in the active circuit."""
        result = self.dssObj.CircuitI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def circuit_firstpcelement(self):
        """Sets the first PCElement to be the active PCElement, as a result,
        this parameter will deliver the index of the active PCElement (ideally 1)."""
        result = self.dssObj.CircuitI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def circuit_nextpcelement(self):
        """Sets the next PCElement to be the active PCElement, as a result,
        this parameter will deliver the index of the active PCElement (if there is no more it will return a 0)."""
        result = self.dssObj.CircuitI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def circuit_firstpdelement(self):
        """Sets the first PDElement to be the active PDElement, as a result,
        this parameter will deliver the index of the active PDElement (ideally 1)."""
        result = self.dssObj.CircuitI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def circuit_nextpdelement(self):
        """Sets the next PDElement to be the active PDElement, as a result, this parameter will deliver the index of the
         active PDElement (if there is no more it will return a 0)."""
        result = self.dssObj.CircuitI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def circuit_sample(self):
        """Forces all meters and monitors to take a sample, returns 0."""
        result = self.dssObj.CircuitI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def circuit_savesample(self):
        """Forces all meters and monitors to save their sample buffers, returns 0."""
        result = self.dssObj.CircuitI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def circuit_setactivebusi(self, i):
        """Sets active the bus specified by index, which is compatible with the index delivered by AllBusNames,
        returns 0 it everything ok."""
        result = self.dssObj.CircuitI(ctypes.c_int32(9), ctypes.c_int32(i))
        return result

    def circuit_firstelement(self):
        """Sets the first Element of the active class to be the active Element, as a result,
        this parameter will deliver the index of the active Element (0 if none)."""
        result = self.dssObj.CircuitI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def circuit_nextelement(self):
        """Sets the next Element of the active class to be the active Element, as a result,
        this parameter will deliver the index of the active Element (0 if none)."""
        result = self.dssObj.CircuitI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def circuit_updatestoraget(self):
        """Forces all storage classes to update. Typically done after a solution."""
        result = self.dssObj.CircuitI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def circuit_parentpdelement(self):
        """Sets parent PD Element, if any, to be the active circuit element and returns
        index > 0 if it fails or not applicable."""
        result = self.dssObj.CircuitI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def circuit_endoftimestepupdate(self):
        """Calls end of time step cleanup routine in solutionalgs.pas. Returns 0."""
        result = self.dssObj.CircuitI(ctypes.c_int32(14), ctypes.c_int32(0))
        return result

    # CircuitF (Float)
    def circuit_capacity(self):
        """Returns the total capacity of the active circuit.
        Or this parameter it is necessary to specify the start and increment of the capacity in the arguments argument1
        and argument2 respectively."""
        result = float(self.dssObj.CircuitF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    # CircuitS (String)
    def circuit_name(self):
        """Returns the name of the active circuit."""
        result = ctypes.c_char_p(self.dssObj.CircuitS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def circuit_disable(self):
        """Allows to disable an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver the string ?Ok?."""
        result = ctypes.c_char_p(self.dssObj.CircuitS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def circuit_enable(self):
        """Allows to enable an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver the string ?Ok?."""
        result = ctypes.c_char_p(self.dssObj.CircuitS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def circuit_setactiveelement(self, argument):
        """Allows to activate an element of the active circuit, the element must be specified by name.
        As a result, this parameter will deliver a string with the index of the active element."""
        result = ctypes.c_char_p(self.dssObj.CircuitS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def circuit_setactivebus(self, argument):
        """Allows to activate a bus of the active circuit, the bus must be specified by name.
        As a result, this parameter will deliver a string with the index of the active Bus."""
        result = ctypes.c_char_p(self.dssObj.CircuitS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def circuit_setactiveclass(self, argument):
        """Allows tto activate a Class of the active circuit, the Class must be specified by name.
        As a result, this parameter will deliver a string with the index of the active Class."""
        result = ctypes.c_char_p(self.dssObj.CircuitS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # CircuitV (Variant)
    def circuit_losses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total losses of the
        active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(0), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_linelosses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total Line losses of
        the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(1), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_substationlosses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total transformer
        losses of the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(2), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_totalpower(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the total power in watts
        delivered to the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(3), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allbusvolts(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the node voltages from
        the most recent solution. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(4), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allbusvmag(self):
        """Returns an array of doubles (magnitude) with the node voltages from the most recent solution.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(5), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allelementnames(self):
        """Returns an array of strings with the names of all the elements of the active circuit. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(6), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allbusnames(self):
        """Returns an array of strings with the names of all the Buses of the active circuit (See AllNodeNames).
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(7), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allelementlosses(self):
        """Returns an array of doubles (two doubles for representing a complex number) with the losses in each element
        of the active circuit.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(8), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allbusvmagpu(self):
        """Returns an array of doubles with the voltages in per unit of the most recent solution of the active circuit.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(9), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allnodenames(self):
        """Returns an array of strings containing full name of each node in system in same order as returned by
        AllBusVolts, etc.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(10), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_systemy(self):
        """Returns an array of doubles (two doubles for representing a complex number) containing the Y Bus Matrix of
        the system (after a solution has been performed).
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(11), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allbusdistances(self):
        """Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(12), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allnodedistances(self):
        """Returns distance from each Node to parent EnergyMeter. Corresponds to sequence in AllBusVmag.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(13), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_allnodevmagbyphase(self, argument):
        """Returns array of doubles representing the voltage magnitudes for nodes on the specified phase.
        The phase must be specified in the Argument2."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(14), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_allnodevmagpubyphase(self, argument):
        """Returns array of doubles representing the voltage magnitudes (in per unit) for nodes on the specified phase.
        The phase must be specified in the Argument2."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(15), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_allnodedistancesbyphase(self, argument):
        """Returns array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to
        other node ByPhase properties. Argument2 must contain the number of the phase to return."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(16), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_allnodenamesbyphase(self, argument):
        """Returns array of strings of the node names by Phase criteria. Sequence corresponds to other ByPhase properties.
        Argument2 must contain the number of the phase to return."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(17), variant_pointer, ctypes.c_int32(argument))
        return variant_pointer.contents.value

    def circuit_ynodevarray(self):
        """Returns a complex array of actual node voltages in same order as SystemY Matrix. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(18), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_ynodeorder(self):
        """Returns a variant array of strings containing the names of the nodes in the same order as the Y Matrix.
        Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(19), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

    def circuit_ycurrents(self):
        """Returns a variant array of doubles containing complex injection currents for the present solution.
        It is the "I" vector of I=YV. Argument2 must be 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CircuitV(ctypes.c_int32(20), variant_pointer, ctypes.c_int32(0))
        return variant_pointer.contents.value

# CktElement Interface

    # CktElementI (int)
    def cktelement_numterminals(self):
        """Deliver the number of terminals of the active DSS object."""
        result = int(self.dssObj.CktElementI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def cktelement_numconductors(self):
        """Deliver the number of conductors of the active DSS object."""
        result = int(self.dssObj.CktElementI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def cktelement_numphases(self):
        """Delivers the number of phases of the active DSS object."""
        result = int(self.dssObj.CktElementI(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result

    def cktelement_open(self):
        """Open the specified terminal (Argument) of the active DSS object."""
        result = int(self.dssObj.CktElementI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def cktelement_close(self):
        """Close the specified terminal (Argument) of the active DSS object."""
        result = self.dssObj.CktElementI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def cktelement_isopen(self):
        """Return a 1 if any terminal of the active DSS object is open, otherwise, it will return a 0."""
        result = self.dssObj.CktElementI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def cktelement_numproperties(self):
        """Return the number of properties of the active DSS object."""
        result = self.dssObj.CktElementI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def cktelement_hasswitchcontrol(self):
        """Returns 1 if the active DSS object has a Switch Control linked; otherwise, it will return 0."""
        result = self.dssObj.CktElementI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def cktelement_hasvoltcontrol(self):
        """Returns 1 if the active DSS object has a Volt Control linked; otherwise, it will return 0."""
        result = self.dssObj.CktElementI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def cktelement_numcontrols(self):
        """Returns number of controls linked to the active DSS object."""
        result = self.dssObj.CktElementI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def cktelement_ocpdevindex(self):
        """Returns the Index into Controller list of OCP Device controlling the active DSS object."""
        result = self.dssObj.CktElementI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def cktelement_ocpdevtype(self):
        """Returns one of the following values: 0=none; 1=Fuse; 2=Recloser; 3=Relay according to the type of active
        control."""
        result = self.dssObj.CktElementI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def cktelement_read_enabled(self):
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
        enabled."""
        result = self.dssObj.CktElementI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def cktelement_write_enabled(self, argument):
        """Returns one of the following values: 0 if the active element is disabled or 1 if the active element is
        enabled."""
        result = self.dssObj.CktElementI(ctypes.c_int32(13), ctypes.c_int32(argument))
        return result

    # CktElementF (Float)
    def cktelement_read_normamps(self):
        """Deliver the normal ampere rating for the active PDElement."""
        result = float(self.dssObj.CktElementF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def cktelement_write_normamps(self, argument):
        """Allows to fix the normal ampere rating for the active PDElement."""
        result = float(self.dssObj.CktElementF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def cktelement_read_emergamps(self):
        """Deliver the Emergency ampere rating for the active PDElement."""
        result = float(self.dssObj.CktElementF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def cktelement_write_emergamp(self, argument):
        """Allows to fix the Emergency ampere rating for the active PDElement. The new value must be defined in the
        variable ?Argument?."""
        result = float(self.dssObj.CktElementF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def cktelement_variablei(self):
        """Delivers get the value of a variable by index for the active PCElement."""
        result = float(self.dssObj.CktElementF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    # CktElementS (String)
    def cktelement_name(self):
        """Delivers the full name of the active circuit element."""
        result = ctypes.c_char_p(self.dssObj.CktElementS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_read_display(self):
        """Displays the name of the active circuit element (not necessarily unique)."""
        result = ctypes.c_char_p(self.dssObj.CktElementS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_write_display(self, argument):
        """Allows to modify the name of the active circuit element (not necessarily unique)."""
        result = ctypes.c_char_p(self.dssObj.CktElementS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def cktelement_guid(self):
        """Delivers the unique name for the active circuit element."""
        result = ctypes.c_char_p(self.dssObj.CktElementS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_energymeter(self):
        """Delivers the name of the EnergyMeter linked to the active circuit element."""
        result = ctypes.c_char_p(self.dssObj.CktElementS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def cktelement_controller(self):
        """Delivers the Full name of the i-th controller attached to the active circuit element.
        The i-th controller index must be specified in the argument arg. Ex: Str = Controller(2).
        See NumControls to determine valid index range."""
        result = ctypes.c_char_p(self.dssObj.CktElementS(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # CktElementV (Variant)
    def cktelement_read_busnames(self):
        """Delivers an array of strings with the names of all the buses connected to the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_write_busnames(self, argument):
        """Allows to fix an array of strings with the names of all the buses connected to the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.CktElementV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_voltages(self):
        """Delivers an array of doubles with the voltages at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_currents(self):
        """Delivers an array of doubles with the currents at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_powers(self):
        """Delivers an array of doubles with the powers at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_losses(self):
        """Delivers an array of doubles with the Losses at terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_phaselosses(self):
        """Delivers an array of doubles with the Losses per phase at the terminals of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seqvoltages(self):
        """Delivers an array of doubles with the symmetrical component voltages per phase at the terminals of the active
         circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seqcurrents(self):
        """Delivers an array of doubles with the symmetrical component Currents per phase at the terminals of the active
         circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_seqpowers(self):
        """Delivers an array of doubles with the symmetrical component powers per phase at the terminals of the active
        circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(9), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_allpropertynames(self):
        """Delivers an array of strings with the names of all the properties of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(10), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_residuals(self):
        """Delivers an array of doubles with the residual currents (magnitude, angle) in all the nodes of the active
        circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(11), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_yprim(self):
        """Delivers an array of doubles with the Y primitive matrix (complex) of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(12), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_cplxseqvoltages(self):
        """Delivers an array of doubles with the complex of sequence voltages for all terminals of the active circuit
        element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(13), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_cplxseqcurrents(self):
        """Delivers an array of doubles with the complex of sequence currents for all terminals of the active circuit
        element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(14), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_allvariablesnames(self):
        """Delivers a Variant array of strings listing all the published state variable names, if the active circuit
        element is a PCElement. Otherwise, null string."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(15), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_allvariablesvalues(self):
        """Delivers a Variant array of doubles listing all the values of the state variables, if the active circuit
        element is a PCElement. Otherwise, null string."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(16), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_nodeorder(self):
        """Delivers a Variant array integers variant array of integer containing the node numbers (representing phases,
        for example) for each conductor of each terminal."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(17), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_currentsmagang(self):
        """Delivers the currents in magnitude, angle format as a variant array of doubles of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(18), variant_pointer)
        return variant_pointer.contents.value

    def cktelement_voltagesmagang(self):
        """Delivers the voltages in magnitude, angle format as a variant array of doubles of the active circuit element."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CktElementV(ctypes.c_int(19), variant_pointer)
        return variant_pointer.contents.value

# CmathLib Interface

    # CmathLibF (Float)
    def cmathlib_cabs(self):
        """Returns the absolute value of complex number given in real (Argument1) and imaginary (Argument2) doubles."""
        result = float(self.dssObj.CmathLibF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def cmathlib_cdang(self):
        """Returns the angle, in degrees, of a complex number specified as two doubles: Real part (Argument1) and
        imaginary part (Argument2)."""
        result = float(self.dssObj.CmathLibF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    # CmathLibV (Variant)
    def cmathlib_cmplx(self):
        """Convert real (Argument1) and imaginary (Argument1) doubles to variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CmathLibV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def cmathlib_ctopolardeg(self):
        """Convert complex number (Argument1 and Argument2) to magnitude and angle, degrees. Returns variant array of
         two doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CmathLibV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def cmathlib_pdegtocomplex(self):
        """Convert magnitude, angle in degrees (Argument1 and Argument2) to a complex number. Returns variant array of
         two doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CmathLibV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# CtrlQueue Interface

    # CtrlQueueI(int)
    def ctrlqueue_clearqueue(self):
        """Clears the control queue."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def ctrlqueue_delete(self):
        """Deletes a control action from the DSS control queue by referencing the handle of the action (Argument)."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def ctrlqueue_numactions(self):
        """Gets the number of actions on the current action list (that have been popped off the control queue by
        CheckControlActions)."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def ctrlqueue_action(self):
        """Sets the active action by index (argument)."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def ctrlqueue_actioncode(self):
        """Gets the code for the active action. Long integer code to tell the control device what to do."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def ctrlqueue_devicehandle(self):
        """Gets the handle (user defined) to device that must act on the pending action."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def ctrlqueue_push(self):
        """Pushes a control action onto the DSS control queue by time, action code, and device handle.
        Returns Control Queue handle."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def ctrlqueue_show(self):
        """Shows the entire control queue in CSV format."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def ctrlqueue_clearactions(self):
        """Clears the action list."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def ctrlqueue_popaction(self):
        """Pops next action off the action list and makes it the active action. Returns zero if none."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def ctrlqueue_queuesize(self):
        """Delivers the size of the current control queue. Returns zero if none."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def ctrlqueue_doallqueue(self):
        """Forces the execution of all control actions stored at the control queue. Returns 0."""
        result = self.dssObj.CtrlQueueI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    # CtrlQueueV (Variant)
    def ctrlqueue_ctrlqueue(self):
        """Delivers the control actions contained in the CtrlQueue after the latest solve command."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.CtrlQueueV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# DSS Interface

    # DSSI (int)
    def dss_numcircuits(self):
        """Gets the number of circuits currently defined."""
        result = int(self.dssObj.DSSI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def dss_clearall(self):
        """Clears all circuit definitions."""
        self.dssObj.DSSI(ctypes.c_int32(1), ctypes.c_int32(0))

    def dss_showpanel(self):
        """Shows non-MDI child form of the Main DSS Edit form."""
        self.dssObj.DSSI(ctypes.c_int32(2), ctypes.c_int32(0))

    def dss_start(self):
        """Validates the user and starts the DSS. Returns TRUE (1) if successful."""
        result = int(self.dssObj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def dss_numclasses(self):
        """Gets the number of DSS intrinsic classes."""
        result = int(self.dssObj.DSSI(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result

    def dss_numuserclasses(self):
        """Gets the number of user-defined classes."""
        result = int(self.dssObj.DSSI(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result

    def dss_reset(self):
        """Resets DSS initialization for restarts, etc. from applets."""
        self.dssObj.DSSI(ctypes.c_int32(6), ctypes.c_int32(0))

    def dss_read_allow_forms(self):
        """Gets if the DSS allows forms (1) or not (0), default (1)."""
        self.dssObj.DSSI(ctypes.c_int32(7), ctypes.c_int32(0))

    def dss_write_allowforms(self, argument):
        """Sets if the DSS allows forms (1) or not (0), default (1)."""
        self.dssObj.DSSI(ctypes.c_int32(8), ctypes.c_int32(argument))

    # DSSS (String)
    def dss_newcircuit(self, argument):
        """Makes a new circuit, the name of the circuit must be specified in the Argument."""
        result = ctypes.c_char_p(self.dssObj.DSSS(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dss_version(self):
        """Gets the version string for the DSS."""
        result = ctypes.c_char_p(self.dssObj.DSSS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def dss_read_datapath(self):
        """Gets the Data File Path. Default for reports, etc. from DSS."""
        result = ctypes.c_char_p(self.dssObj.DSSS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def dss_write_datapath(self, argument):
        """Gets the Data File Path. Default for reports, etc. from DSS."""
        result = ctypes.c_char_p(self.dssObj.DSSS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dss_default_editor(self):
        """Gets the path name for the default text editor."""
        result = ctypes.c_char_p(self.dssObj.DSSS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # DSSV (Variant)
    def dss_classes(self):
        """Gets the list of DSS intrinsic classes (names of the classes)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.DSSV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value

    def dss_user_classes(self):
        """Gets list of user-defined classes (names of the classes)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.DSSV(ctypes.c_int32(1), variant_pointer)
        return variant_pointer.contents.value

# DSSElement Interface

    # DSSElementI (int)
    def dsselement_numproperties(self):
        """Gets the number of properties for the active DSS object."""
        result = int(self.dssObj.DSSElementI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    # DSSElementS (String)
    def dsselement_name(self):
        """Gets the full name of the active DSS object (general element or circuit element)."""
        result = ctypes.c_char_p(self.dssObj.DSSElementS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # DSSElementV (Variant)
    def dsselement_allpropertynames(self):
        """Gets a variant array of strings containing the names of all properties for the active DSS object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.DSSElementV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    # DSSProgress Interface

    # DSSElementI (int)
    def dssprogress_pctprogress(self):
        """Sets the percent progress to indicate [0..100]."""
        result = int(self.dssObj.DSSProgressI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def dssprogress_show(self):
        """Shows progress form with null caption and progress set to zero."""
        result = int(self.dssObj.DSSProgressI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def dssprogress_close(self):
        """Closes (hides) DSS Progress form."""
        result = int(self.dssObj.DSSProgressI(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result

    # DSSProgressS (String)
    def dssprogress_caption(self):
        """Sets the caption to appear on the bottom of the DSS Progress form."""
        result = ctypes.c_char_p(self.dssObj.DSSProgressS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

# DSSProperties

    # DSSProperties
    def dssproperties_name(self, argument):
        """Delivers the name of the active property. The index of the property must be specified in the argument.
        The index minimum value is 1. This value must be entered as string."""
        result = ctypes.c_char_p(self.dssObj.DSSProperties(ctypes.c_int32(0), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_description(self, argument):
        result = ctypes.c_char_p(self.dssObj.DSSProperties(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_read_value(self, argument):
        result = ctypes.c_char_p(self.dssObj.DSSProperties(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def dssproperties_write_value(self, argument):
        result = ctypes.c_char_p(self.dssObj.DSSProperties(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

# DSS_Executive Interface

    # DSS_ExecutiveI (int)
    def executive_numcommands(self):
        """Gets the number of DSS Executive Commands."""
        result = int(self.dssObj.DSSExecutiveI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def executive_numoptions(self):
        """Gets the number of DSS Executive Options."""
        result = int(self.dssObj.DSSExecutiveI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    # DSSExecutiveS (String)
    def executive_command(self):
        """Gets i-th command (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dssObj.DSSExecutiveS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_option(self):
        """Gets i-th option (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dssObj.DSSExecutiveS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_commandhelp(self):
        """Gets help string for i-th command (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dssObj.DSSExecutiveS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_optionhelp(self):
        """Gets help string for i-th option (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dssObj.DSSExecutiveS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def executive_optionvalue(self):
        """Gets present value for i-th option (specified in the argument as string)."""
        result = ctypes.c_char_p(self.dssObj.DSSExecutiveS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

# Fuses Interface

    # FusesI (int)
    def fuses_count(self):
        """Returns the number of Fuses objects currently defined in the active circuit."""
        result = self.dssObj.FusesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def fuses_first(self):
        """Sets the first Fuse to be the active Fuse. Returns 0 if none."""
        result = self.dssObj.FusesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def fuses_next(self):
        """Sets the next Fuse to be the active Fuse. Returns 0 if none."""
        result = self.dssObj.FusesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def fuses_read_monitoredterm(self):
        """Gets the terminal number to switch the fuse is connected."""
        result = self.dssObj.FusesI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def fuses_write_monitoredterm(self, argument):
        """Sets the terminal number to switch the fuse is connected."""
        result = self.dssObj.FusesI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def fuses_read_switchedterm(self):
        """Gets the terminal number of the terminal containing the switch controlled by the fuse."""
        result = self.dssObj.FusesI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def fuses_write_switchedterm(self, argument):
        """Sets the terminal number of the terminal containing the switch controlled by the fuse."""
        result = self.dssObj.FusesI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def fuses_open(self):
        """Opening of fuse."""
        result = self.dssObj.FusesI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def fuses_close(self):
        """Closing of fuse."""
        result = self.dssObj.FusesI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def fuses_isblown(self):
        """Returns the current state of the fuses. TRUE (1) if any on any phase is blown. Else FALSE (0)."""
        result = self.dssObj.FusesI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def fuses_read_idx(self):
        """Gets the active fuse by index into the list of fuses. 1 based: 1..count."""
        result = self.dssObj.FusesI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def fuses_write_idx(self, argument):
        """Sets the active fuse by index into the list of fuses. 1 based: 1..count."""
        result = self.dssObj.FusesI(ctypes.c_int32(11), ctypes.c_int32(argument))
        return result

    def fuses_numphases(self):
        """Gets the number of phases of the active fuse."""
        result = self.dssObj.FusesI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    # FusesF (Float)
    def fuses_read_ratedcurrent(self):
        """Gets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0,
        Multiply current values of TCC curve by this to get actual amps."""
        result = float(self.dssObj.FusesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def fuses_write_ratedcurrent(self, argument):
        """Sets the multiplier or actual amps for the TCCcurve object. Defaults to 1.0,
        Multiply current values of TCC curve by this to get actual amps."""
        result = float(self.dssObj.FusesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def fuses_read_delay(self):
        """Gets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0."""
        result = float(self.dssObj.FusesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def fuses_write_delay(self, argument):
        """Sets the fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0."""
        result = float(self.dssObj.FusesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    # FusesS (String)
    def fuses_read_name(self):
        """Gets the name of the active fuse."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_name(self, argument):
        """Sets the name of the active fuse."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def fuses_read_monitoredobj(self):
        """Gets the name of the Monitored Object by the active fuse."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_monitoredobj(self, argument):
        """Sets the name of the Monitored Object by the active fuse."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def fuses_read_switchedobj(self):
        """Gets the full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_switchedobj(self, argument):
        """Sets the full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def fuses_read_tcccurve(self):
        """Gets the name of the TCCcurve object that determines fuse blowing."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def fuses_write_tcccurve(self, argument):
        """Sets the name of the TCCcurve object that determines fuse blowing."""
        result = ctypes.c_char_p(self.dssObj.FusesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    # FusesV (Variant)
    def fuses_allnames(self):
        """Gets the variant array of string containing names of all fuses in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.FusesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# Generators Interface

    # GeneratorsI (int)
    def generators_first(self):
        """Sets first generator to be active. Returns 0 if None."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def generators_next(self):
        """Sets next generator to be active. Returns 0 if None."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def generators_read_forcedon(self):
        """Returns 1 if the generator is forced ON regardless of other dispatch criteria; otherwise, returns 0."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def generators_write_forcedon(self, argument):
        """Allows to force ON regardless of other dispatch criteria. To force ON put 1 in the argument, otherwise put 0."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def generators_read_phases(self):
        """Returns the number of phases of the active generator."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def generators_write_phases(self, argument):
        """Sets the number of phases (argument) of the active generator."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def generators_count(self):
        """Returns the number of generators Objects in Active Circuit."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def generators_read_idx(self):
        """Gets the active generator by Index into generators list. 1..Count."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def generators_write_idx(self, argument):
        """Sets the active generator (argument) by Index into generators list. 1..Count."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def generators_read_model(self):
        """Gets the active generator Model (see Manual for details)."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def generators_write_model(self, argument):
        """Sets the active generator Model (see Manual for details)."""
        result = self.dssObj.GeneratorsI(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    # GeneratorsF (Float)
    def generators_read_kv(self):
        """Gets the voltage base for the active generator, kV."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def generators_write_kv(self, argument):
        """Sets the voltage base for the active generator, kV."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def generators_read_kw(self):
        """Gets the kW output for the active generator, kvar is updated for current power factor."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def generators_write_kw(self, argument):
        """Sets the kW output for the active generator, kvar is updated for current power factor."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def generators_read_kvar(self):
        """Gets the kvar output for the active generator, kW is updated for current power factor."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def generators_write_kvar(self, argument):
        """Sets the kvar output for the active generator, kW is updated for current power factor."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def generators_read_pf(self):
        """Gets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def generators_write_pf(self, argument):
        """Sets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def generators_read_kvarated(self):
        """Gets the KVA rating of the generator."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def generators_write_kvarated(self, argument):
        """Sets the KVA rating of the generator."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def generators_read_vmaxpu(self):
        """Gets the Vmaxpu for Generator Model."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def generators_write_vmaxpu(self, argument):
        """Sets the Vmaxpu for Generator Model."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def generators_read_vminpu(self):
        """Gets the Vminpu for Generator Model."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def generators_write_vminpu(self, argument):
        """Sets the Vminpu for Generator Model."""
        result = float(self.dssObj.GeneratorsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    # GeneratorsS (String)
    def generators_read_name(self):
        """Gets the name of the active Generator."""
        result = ctypes.c_char_p(self.dssObj.GeneratorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def generators_write_name(self, argument):
        """Sets the name of the active Generator."""
        result = ctypes.c_char_p(self.dssObj.GeneratorsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # GeneratorsV (Variant)
    def generators_allnames(self):
        """Gets the array of names of all Generator objects."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.GeneratorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def generators_registernames(self):
        """Gets the array of names of all generator Energy Meter registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.GeneratorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def generators_registervalues(self):
        """Gets the array of values in generator Energy Meter registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.GeneratorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# Isource Interface

    # IsourcesI (int)
    def isources_count(self):
        """Returns the number of Isource objects currently defined in the active circuit."""
        result = self.dssObj.IsourceI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def isources_first(self):
        """Sets the first ISource to be active; returns 0 if none."""
        result = self.dssObj.IsourceI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def isources_next(self):
        """Sets the next ISource to be active; returns 0 if none."""
        result = self.dssObj.IsourceI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    # IsourcesF (Float)
    def isources_read_amps(self):
        """Gets the magnitude of the Isource in Amps."""
        result = float(self.dssObj.IsourceF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def isources_write_amps(self, argument):
        """Sets the magnitude of the Isource in Amps."""
        result = float(self.dssObj.IsourceF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def isources_read_angledeg(self):
        """Gets the phase angle of the Isource in degrees."""
        result = float(self.dssObj.IsourceF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def isources_write_angledeg(self, argument):
        """Sets the phase angle of the Isource in degrees."""
        result = float(self.dssObj.IsourceF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def isources_read_frequency(self):
        """Gets the frequency of the Isource in Hz."""
        result = float(self.dssObj.IsourceF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def isources_write_frequency(self, argument):
        """Sets the frequency of the Isource in Hz."""
        result = float(self.dssObj.IsourcesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    # IsourcesS (String)
    def isources_read_name(self):
        """Gets the name of the active Isource object."""
        result = ctypes.c_char_p(self.dssObj.IsourceS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def isources_write_name(self, argument):
        """Sets the name of the active Isource object."""
        result = ctypes.c_char_p(self.dssObj.IsourceS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # IsourcesV (Variant)
    def isources_allnames(self):
        """Gets the variant array of string containing names of all ISources in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.IsourceV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    # Lines Interface

    # LinesI (int)
    def lines_first(self):
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dssObj.LinesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def lines_next(self):
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dssObj.LinesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def lines_read_phases(self):
        """Gets the number of phases of the active line object."""
        result = self.dssObj.LinesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def lines_write_phases(self, argument):
        """Sets the number of phases of the active line object."""
        result = self.dssObj.LinesI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def lines_numcust(self):
        """Gets the number of customers on this line section."""
        result = self.dssObj.LinesI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def lines_parent(self):
        """Gets the parents of the active Line to be the active Line. Return 0 if no parent or action fails."""
        result = self.dssObj.LinesI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def lines_count(self):
        """Gets the number of Line Objects in Active Circuit."""
        result = self.dssObj.LinesI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def lines_read_units(self):
        """Gets the units of the line (distance, check manual for details)."""
        result = self.dssObj.LinesI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def lines_write_units(self, argument):
        """Sets the units of the line (distance, check manual for details)."""
        result = self.dssObj.LinesI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    # LinesF (Float)
    def lines_read_length(self):
        """Gets the length of line section in units compatible with the LineCode definition."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def lines_write_length(self, argument):
        """Sets the length of line section in units compatible with the LineCode definition."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def lines_read_r1(self):
        """Gets the positive sequence resistance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def lines_write_r1(self, argument):
        """Sets the positive sequence resistance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def lines_read_x1(self):
        """Gets the positive sequence reactance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def lines_write_x1(self, argument):
        """Sets the positive sequence reactance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def lines_read_r0(self):
        """Gets the zero sequence resistance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def lines_write_r0(self, argument):
        """Sets the zero sequence resistance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def lines_read_x0(self):
        """Gets the zero sequence reactance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def lines_write_x0(self, argument):
        """Sets the zero sequence reactance, ohm per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def lines_read_c1(self):
        """Gets the positive sequence capacitance, nanofarads per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def lines_write_c1(self, argument):
        """Sets the positive sequence capacitance, nanofarads per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def lines_read_c0(self):
        """Gets the zero sequence capacitance, nanofarads per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def lines_write_c0(self, argument):
        """Sets the zero sequence capacitance, nanofarads per unit length."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def lines_read_normamps(self):
        """Gets the normal ampere rating of line section."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def lines_write_normamps(self, argument):
        """Sets the normal ampere rating of Line."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def lines_read_emergamps(self):
        """Gets the emergency (maximum) ampere rating of Line."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def lines_write_emergamps(self, argument):
        """Sets the emergency (maximum) ampere rating of Line."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def lines_read_rg(self):
        """Gets the earth return value used to compute line impedances at power frequency."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def lines_write_rg(self, argument):
        """Sets the earth return value used to compute line impedances at power frequency."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def lines_read_xg(self):
        """Gets the earth return reactance value used to compute line impedances at power frequency."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def lines_write_xg(self, argument):
        """Sets the earth return reactance value used to compute line impedances at power frequency."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def lines_read_rho(self):
        """Gets the earth resistivity, m-ohms."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def lines_write_rho(self, argument):
        """Sets the earth resistivity, m-ohms."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(23), ctypes.c_double(argument)))
        return result

    def lines_read_seasonrating(self):
        """Returns the rating for the current season (in Amps) if the SeasonalRatings option is active."""
        result = float(self.dssObj.LinesF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    # LinesS (String)
    def lines_read_name(self):
        """Gets the name of the active Line element."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_name(self, argument):
        """Sets the name of the Line element to set it active."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_bus1(self):
        """Gets the name of bus for terminal 1."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_bus1(self, argument):
        """Sets the name of bus for terminal 1."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_bus2(self):
        """Gets the name of bus for terminal 2."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_bus2(self, argument):
        """Sets the name of bus for terminal 2."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_linecode(self):
        """Gets the name of LineCode object that defines the impedances."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_linecode(self, argument):
        """Sets the name of LineCode object that defines the impedances."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_geometry(self):
        """Gets the name of the Line geometry code."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_geometry(self, argument):
        """Sets the name of the Line geometry code."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def lines_read_spacing(self):
        """Gets the name of the Line spacing code."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def lines_write_spacing(self, argument):
        """Sets the name of the Line spacing code."""
        result = ctypes.c_char_p(self.dssObj.LinesS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    # LinesV (Variant)
    def lines_allnames(self):
        """Gets the name of all Line Objects."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LinesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_rmatrix(self):
        """Gets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LinesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_rmatrix(self, argument):
        """Sets the resistance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LinesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_xmatrix(self):
        """Gets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LinesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_xmatrix(self, argument):
        """Sets the reactance matrix (full), ohms per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LinesV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_cmatrix(self):
        """Gets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LinesV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_cmatrix(self, argument):
        """Sets the capacitance matrix (full), nanofarads per unit length. Variant array of doubles."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LinesV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def lines_read_yprim(self):
        """Gets the YPrimitive of the active Line."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LinesV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def lines_write_yprim(self, argument):
        """Does nothing at present."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LinesV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

# LineCodes Interface

    # LineCodesI (int)
    def linecodes_count(self):
        """Gets the number of Line Objects in Active Circuit."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def linecodes_first(self):
        """Sets the first element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def linecodes_next(self):
        """Sets the next element active. Returns 0 if no lines. Otherwise, index of the line element."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def linecodes_read_units(self):
        """Delivers the units of the active LineCode as an integer."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def linecodes_write_units(self, argument):
        """Sets the units of the active LineCode. The units must be specified as an integer in the argument.
        Please refer to the OpenDSS User manual for more information."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def linecodes_read_phasess(self):
        """Delivers the number of phases of the active LineCode as an integer."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def linecodes_write_phasess(self, argument):
        """Sets the number of phases of the active LineCode. The units must be specified as an integer in the argument."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def linecodes_isz1z0(self):
        """Gets the flag (Boolean 1/0) denoting whether the impedance data were entered in symmetrical components."""
        result = self.dssObj.LineCodesI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    # LineCodesF (Float)
    def linecodes_read_r1(self):
        """Gets the Positive-sequence resistance in ohms per unit length for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def linecodes_write_r1(self, argument):
        """Sets the Positive-sequence resistance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def linecodes_read_x1(self):
        """Gets the Positive-sequence reactance in ohms per unit length for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def linecodes_write_x1(self, argument):
        """Sets the Positive-sequence reactance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def linecodes_read_r0(self):
        """Gets the Zero-sequence resistance in ohms per unit length for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def linecodes_write_r0(self, argument):
        """Sets the Zero-sequence resistance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def linecodes_read_x0(self):
        """Gets the Zero-sequence reactance in ohms per unit length for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def linecodes_write_x0(self, argument):
        """Sets the Zero-sequence reactance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def linecodes_read_c1(self):
        """Gets the Positive-sequence capacitance in ohms per unit length for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def linecodes_write_c1(self, argument):
        """Sets the Positive-sequence capacitance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def linecodes_read_c0(self):
        """Gets the Zero-sequence capacitance in ohms per unit length for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def linecodes_write_c0(self, argument):
        """Sets the Zero-sequence capacitance in ohms per unit length for the active LineCode.
        This value must be specified in the argument as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def linecodes_read_normamps(self):
        """Gets the normal ampere rating for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def linecodes_write_normamps(self, argument):
        """Sets the normal ampere rating for the active LineCode. This value must be specified in the argument
        as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def linecodes_read_emergamps(self):
        """Gets the Emergency ampere rating for the active LineCode."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def linecodes_write_emergamps(self, argument):
        """Sets the Emergency ampere rating for the active LineCode. This value must be specified in the argument
        as a double."""
        result = float(self.dssObj.LineCodesF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    # LineCodesS (String)
    def linecodes_read_name(self):
        """Gets the name of the active LineCode element."""
        result = ctypes.c_char_p(self.dssObj.LineCodesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def linecodes_write_name(self, argument):
        """Sets the name of the active LineCode element. The new value must be specified in the argument as a string."""
        result = ctypes.c_char_p(self.dssObj.LineCodesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # LineCodesV (Variant)
    def linecodes_read_rmatrix(self):
        """Gets the resistance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LineCodesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_rmatrix(self, argument):
        """Sets the resistance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LineCodesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_read_xmatrix(self):
        """Gets the reactance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LineCodesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_xmatrix(self, argument):
        """Sets the reactance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LineCodesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_read_cmatrix(self):
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LineCodesV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_write_cmatrix(self, argument):
        """Sets the capacitance matrix in ohms per unit length of the active LineCode. The new values must be entered as
         a vector of doubles using the argument."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LineCodesV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def linecodes_allnames(self):
        """Gets the capacitance matrix in ohms per unit length of the active LineCode."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LineCodesV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

# Loads Interface

    # Loads int (int)
    def loads_first(self):
        """Allows to set the active load into the first load registered in the active circuit.
        As a result, this property will return the number 1. The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def loads_next(self):
        """Sets the active load into the next load registered in the active circuit.
        As a result, this property will deliver the index of the active load. The parameter argument can be filled
        with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def loads_read_idx(self):
        """Allows to read the index of the active load. The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def loads_write_idx(self, argument):
        """Allows to write the index of the active load. The parameter argument must contain the index of the desired
        active load.
        The return value will be equal to 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def loads_count(self):
        """Returns the number of load elements within the active circuit.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def loads_read_class(self):
        """Allows to read the code number used to separate loads by class or group.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def loads_write_class(self, argument):
        """Allows to read the code number used to separate loads by class or group.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def loads_read_model(self):
        """Allows to read the model of the active load.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def loads_write_model(self, argument):
        """Allows to write the model of the active load using the parameter argument.
        This parameter will return a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def loads_read_numcust(self):
        """Allows to read the number of customer of the active load.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def loads_write_numcust(self, argument):
        """Allows to write the number of customers of the active load using the parameter argument.
        This parameter will return a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    def loads_read_status(self):
        """Allows to read Response to load multipliers:
        Fixed (growth only - 1), Exempt (no LD curve - 2), Variable (all - 0), of the active load.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def loads_write_status(self, argument):
        """Allows to read Response to load multipliers:
        Fixed (growth only - 1), Exempt (no LD curve - 2), Variable (all - 0), of the active load.
        This parameter will return a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(12), ctypes.c_int32(argument))
        return result

    def loads_read_isdelta(self):
        """Allows to read if the active load is connected in delta,
        if the answer is positive, this function will deliver a 1; otherwise, the answer will be 0.
        The parameter argument can be filled with a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def loads_write_isdelta(self, argument):
        """Allows to read if the active load is connected in delta,
        if the answer is positive, this function will deliver a 1; otherwise, the answer will be 0.
        This parameter will return a 0."""
        result = self.dssObj.DSSLoads(ctypes.c_int32(14), ctypes.c_int32(argument))
        return result

    # Loads F (Float)
    def loads_read_kw(self):
        """Allows to read the kW property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def loads_write_kw(self, argument):
        """Allows to write the kW property of the active load.
        The parameter argument must contain the new value in kW for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def loads_read_kv(self):
        """Allows to read the kV property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def loads_write_kv(self, argument):
        """Allows to write the kV property of the active load.
        The parameter argument must contain the new value in kV for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def loads_read_kvar(self):
        """Allows to read the kvar property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def loads_write_kvar(self, argument):
        """Allows to write the kvar property of the active load.
        The parameter argument must contain the new value in kvar for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def loads_read_pf(self):
        """Allows to read the pf property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def loads_write_pf(self, argument):
        """Allows to write the pf property of the active load.
        The parameter argument must contain the new value in pf for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def loads_read_pctmean(self):
        """Allows to read the PctMean property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def loads_write_pctmean(self, argument):
        """Allows to write the PctMean property of the active load.
        The parameter argument must contain the new value in PctMean for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def loads_read_pctstddev(self):
        """Allows to read the PctStdDev property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def loads_write_pctstddev(self, argument):
        """Allows to write the PctStdDev property of the active load.
        The parameter argument must contain the new value in PctStdDev for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def loads_read_allocationfactor(self):
        """Allows to read the AllocationFactor property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def loads_write_allocationfactor(self, argument):
        """Allows to write the AllocationFactor property of the active load.
        The parameter argument must contain the new value in AllocationFactor for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def loads_read_cfactor(self):
        """Allows to read the CFactor property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def loads_write_cfactor(self, argument):
        """Allows to write the CFactor property of the active load.
        The parameter argument must contain the new value in CFactor for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def loads_read_cvrwatts(self):
        """Allows to read the CVRWatts property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def loads_write_cvrwatts(self, argument):
        """Allows to write the CVRWatts property of the active load.
        The parameter argument must contain the new value in CVRWatts for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def loads_read_cvrvars(self):
        """Allows to read the CVRvars property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def loads_write_cvrvars(self, argument):
        """Allows to write the CVRvars property of the active load.
        The parameter argument must contain the new value in CVRWatts for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def loads_read_kva(self):
        """Allows to read the kva property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def loads_write_kva(self, argument):
        """Allows to write the kva property of the active load.
        The parameter argument must contain the new value in kva for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def loads_read_kwh(self):
        """Allows to read the kWh property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def loads_write_kwh(self, argument):
        """Allows to write the kWh property of the active load.
        The parameter argument must contain the new value in kWh for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(23), ctypes.c_double(argument)))
        return result

    def loads_read_kwhdays(self):
        """Allows to read the kWhdays property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    def loads_write_kwhdays(self, argument):
        """Allows to write the kWhdays property of the active load.
        The parameter argument must contain the new value in kWhdays for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(25), ctypes.c_double(argument)))
        return result

    def loads_read_rneut(self):
        """Allows to read the RNeut (neutral resistance for wye connected loads) property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(26), ctypes.c_double(0)))
        return result

    def loads_write_rneut(self, argument):
        """Allows to write the RNeut (neutral resistance for wye connected loads) property of the active load.
        The parameter argument must contain the new value in RNeut for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(27), ctypes.c_double(argument)))
        return result

    def loads_read_vmaxpu(self):
        """Allows to read the VMaxpu property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(28), ctypes.c_double(0)))
        return result

    def loads_write_vmaxpu(self, argument):
        """Allows to write the VMaxpu property of the active load.
        The parameter argument must contain the new value in VMaxpu for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(29), ctypes.c_double(argument)))
        return result

    def loads_read_vminemerg(self):
        """Allows to read the VMinemerg property of the active load. The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(30), ctypes.c_double(0)))
        return result

    def loads_write_vminemerg(self, argument):
        """Allows to write the VMinemerg property of the active load.
        The parameter argument must contain the new value in VMinemerg for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(31), ctypes.c_double(argument)))
        return result

    def loads_read_vminnorm(self):
        """Allows to read the VMinnorm property of the active load. The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(32), ctypes.c_double(0)))
        return result

    def loads_write_vminnorm(self, argument):
        """Allows to write the VMinnorm property of the active load.
        The parameter argument must contain the new value in VMinnorm for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(33), ctypes.c_double(argument)))
        return result

    def loads_read_vminpu(self):
        """Allows to read the VMinpu property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(34), ctypes.c_double(0)))
        return result

    def loads_write_vminpu(self, argument):
        """Allows to write the VMinpu property of the active load.
        The parameter argument must contain the new value in VMinpu for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(35), ctypes.c_double(argument)))
        return result

    def loads_read_xfkva(self):
        """Allows to read the xfKVA (Rated service transformer KVA for load allocation, using Allocationfactor.
        Affects kW, kvar and pf.) property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(36), ctypes.c_double(0)))
        return result

    def loads_write_xfkva(self, argument):
        """Allows to write the xfKVA (Rated service transformer KVA for load allocation, using Allocationfactor.
        Affects kW, kvar and pf.) property of the active load.
        The parameter argument must contain the new value in xfKVA for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(37), ctypes.c_double(argument)))
        return result

    def loads_read_xneut(self):
        """Allows to read the Xneut property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(38), ctypes.c_double(0)))
        return result

    def loads_write_xneut(self, argument):
        """Allows to write the Xneut property of the active load.
        The parameter argument must contain the new value in Xneut for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(39), ctypes.c_double(argument)))
        return result

    def loads_read_pctseriesrl(self):
        """allows to read the PctSeriesRL (Percent of Load that is modeled as series R-L for harmonic studies)
        property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(40), ctypes.c_double(0)))
        return result

    def loads_write_pctseriesrl(self, argument):
        """allows to write the PctSeriesRL (Percent of Load that is modeled as series R-L for harmonic studies)
        property of the active load.
        The parameter argument must contain the new value in PctSeriesRL for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(41), ctypes.c_double(argument)))
        return result

    def loads_read_relweight(self):
        """Allows to read the RelWeight (relative weighting factor) property of the active load.
        The parameter argument can be filled with a 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(42), ctypes.c_double(0)))
        return result

    def loads_write_relweight(self, argument):
        """Allows to write the RelWeight (relative weighting factor) property of the active load.
        The parameter argument must contain the new value in RelWeight for the desired active load.
        The return value will be equal to 0."""
        result = float(self.dssObj.DSSLoadsF(ctypes.c_int32(43), ctypes.c_double(argument)))
        return result

    # LoadsS (String)
    def loads_read_name(self):
        """Allows to read the Name property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_name(self, argument):
        """allows to set the active load by specifying the Name load.
        The parameter argument must contain the Name of the load to activate.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_cvrcurve(self):
        """Allows to read the CVRCurve property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_cvrcurve(self, argument):
        """Allows to set the CVRCurve property for the active load.
        The parameter argument must contain the Name of the new CVRCurve to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_daily(self):
        """Allows to read the daily property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_daily(self, argument):
        """Allows to set the daily property for the active load.
        The parameter argument must contain the Name of the new daily to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_duty(self):
        """Allows to read the duty property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_duty(self, argument):
        """Allows to set the dduty property for the active load.
        The parameter argument must contain the Name of the new duty to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_spectrum(self):
        """Allows to read the Spectrum property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_spectrum(self, argument):
        """Allows to set the Spectrum property for the active load.
        The parameter argument must contain the Name of the new Spectrum to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_yearly(self):
        """Allows to read the Yearly property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_yearly(self, argument):
        """Allows to set the Yearly property for the active load.
        The parameter argument must contain the Name of the new Yearly to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    def loads_read_growth(self):
        """Allows to read the Growth property of the active load.
        The parameter argument can be filled with an empty string."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(12), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loads_write_growth(self, argument):
        """Allows to set the Growth property for the active load.
        The parameter argument must contain the Name of the new Growth to be linked to the active load.
        The return value will be equal to empty."""
        result = ctypes.c_char_p(self.dssObj.DSSLoadsS(ctypes.c_int32(13), argument.encode('ascii')))
        return result.value.decode('ascii')

    # LoadsV (Variant)
    def loads_allnames(self):
        """Allows to read the names of all the loads present in the active circuit.
        The result is delivered as variant, however, the content of this variant is an array of strings."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.DSSLoadsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def loads_read_zipv(self):
        """Allows to read the array of 7 elements (doubles) for ZIP property of the active Load object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.DSSLoadsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def loads_write_zipv(self, argument):
        """Allows to write the array of 7 elements (doubles) for ZIP property of the active Load object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.DSSLoadsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# LoadShapes Interface

    # LoadShapeI
    def loadshapes_count(self):
        """Returns the number of LoadShape objects currently defined in LoadShape collection."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def loadshapes_first(self):
        """sets the first loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def loadshapes_next(self):
        """Sets the next loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def loadshapes_read_npts(self):
        """Gets the number of points in active LoadShape."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def loadshapes_write_npts(self, argument):
        """Sets the number of points in active LoadShape."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def loadshapes_normalize(self):
        """Normalizes the P and Q curves based on either Pbase, Qbase or simply the peak value of the curve."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def loadshapes_read_useactual(self):
        """Gets a TRUE/FALSE (1/0) to let Loads know to use the actual value in the curve rather than use the value as
         a multiplier."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def loadshapes_write_useactual(self, argument):
        """Sets a TRUE/FALSE (1/0 - Argument) to let Loads know to use the actual value in the curve rather than use
         the value as a multiplier."""
        result = self.dssObj.LoadShapeI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

# LoadShapeF (Float)
    def loadshapes_read_hrinterval(self):
        """Gets the fixed interval time value, hours."""
        result = float(self.dssObj.LoadShapeF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def loadshapes_write_hrinterval(self, argument):
        """Sets the fixed interval time value, hours."""
        result = float(self.dssObj.LoadShapeI(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def loadshapes_read_mininterval(self):
        """Gets the fixed interval time value, in minutes."""
        result = float(self.dssObj.LoadShapeF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def loadshapes_write_mininterval(self, argument):
        """Sets the fixed interval time value, in minutes."""
        result = float(self.dssObj.LoadShapeI(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def loadshapes_read_pbase(self):
        """Gets the base for normalizing P curve. If left at zero, the peak value is used."""
        result = float(self.dssObj.LoadShapeF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def loadshapes_write_pbase(self, argument):
        """Sets the base for normalizing P curve. If left at zero, the peak value is used."""
        result = float(self.dssObj.LoadShapeI(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def loadshapes_read_qbase(self):
        """Gets the base for normalizing Q curve. If left at zero, the peak value is used."""
        result = float(self.dssObj.LoadShapeF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def loadshapes_write_qbase(self, argument):
        """Sets the base for normalizing Q curve. If left at zero, the peak value is used."""
        result = float(self.dssObj.LoadShapeI(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def loadshapes_read_sinterval(self):
        """Gets the fixed interval data time interval, seconds."""
        result = float(self.dssObj.LoadShapeF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def loadshapes_write_sinterval(self, argument):
        """Sets the fixed interval data time interval, seconds."""
        result = float(self.dssObj.LoadShapeI(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    # LoadShapeS (String)
    def loadshapes_read_name(self):
        """Gets the name of the active LoadShape object."""
        result = ctypes.c_char_p(self.dssObj.LoadShapeS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loadshapes_write_name(self, argument):
        """Sets the name of the active LoadShape object."""
        result = ctypes.c_char_p(self.dssObj.LoadShapeS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

     # LoadShapeV (Variant)
    def loadshapes_allnames(self):
        """Gets a variant array of strings containing names of all LoadShape objects currently defined."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LoadShapeV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_read_pmult(self):
        """Gets a variant array of doubles for the P multiplier in the LoadShape."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LoadShapeV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_write_pmult(self, argument):
        """Sets a variant array of doubles for the P multiplier in the LoadShape."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LoadShapeV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_read_qmult(self):
        """Gets a variant array of doubles for the Q multiplier in the LoadShape."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LoadShapeV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_write_qmult(self, argument):
        """Sets a variant array of doubles for the Q multiplier in the LoadShape."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LoadShapeV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_read_timearray(self):
        """Gets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.LoadShapeV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_write_timearray(self, argument):
        """Sets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.LoadShapeV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

# Meters Interface

    # MetersI (int)
    def meters_first(self):
        """Sets the first Energy Meter active. Returns 0 if no monitors."""
        result = self.dssObj.MetersI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def meters_next(self):
        """Sets the next energy Meter Active. Returns 0 if no more."""
        result = self.dssObj.MetersI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def meters_reset(self):
        """Resets the active Meter object."""
        result = self.dssObj.MetersI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def meters_resetall(self):
        """Resets all Meter object."""
        result = self.dssObj.MetersI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def meters_sample(self):
        """Causes active meter to take a sample."""
        result = self.dssObj.MetersI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def meters_save(self):
        """Causes active meter to save its current sample buffer to its meter stream.
        Then you can access the Bytestream or channel data. Most standard solution modes do this automatically."""
        result = self.dssObj.MetersI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def meters_read_meteredterminal(self):
        """Returns the number of metered terminal by the active Energy Meter."""
        result = self.dssObj.MetersI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def meters_write_meteredterminal(self, argument):
        """Sets the number of metered terminal by the active Energy Meter."""
        result = self.dssObj.MetersI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def meters_difilesareopen(self):
        """Returns a global flag (1=true, 0=false) to indicate if Demand Interval (DI)
        files have been properly opened."""
        result = self.dssObj.MetersI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def meters_sampleall(self):
        """Causes all Energy Meters to take a sample of the present state. Returns 0."""
        result = self.dssObj.MetersI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def meters_saveall(self):
        """Save all Energy Meter buffers to their respective file streams. Returns 0."""
        result = self.dssObj.MetersI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def meters_openalldifiles(self):
        """Opens Demand Interval (DI) files. Returns 0."""
        result = self.dssObj.MetersI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def meters_closealldifiles(self):
        """Closes all Demand Interval (DI) files. Necessary at the end of a run."""
        result = self.dssObj.MetersI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def meters_countendelements(self):
        """Returns the number of zone end elements in the active meter zone."""
        result = self.dssObj.MetersI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def meters_count(self):
        """Returns the number of Energy Meters in the Active Circuit."""
        result = self.dssObj.MetersI(ctypes.c_int32(14), ctypes.c_int32(0))
        return result

    def meters_countbranches(self):
        """Returns the number of branches in active Energy Meter zone (same as sequencelist size)."""
        result = self.dssObj.MetersI(ctypes.c_int32(15), ctypes.c_int32(0))
        return result

    def meters_read_sequenceindex(self):
        """Returns the index into meter's SequenceList that contains branch pointers in lexical order.
        Earlier index guaranteed to be up line from later index. Sets PDElement active."""
        result = self.dssObj.MetersI(ctypes.c_int32(16), ctypes.c_int32(0))
        return result

    def meters_write_sequenceindex(self, argument):
        """Sets the index into meter's SequenceList that contains branch pointers in lexical order.
        Earlier index guaranteed to be up line from later index. Sets PDElement active."""
        result = self.dssObj.MetersI(ctypes.c_int32(17), ctypes.c_int32(argument))
        return result

    def meters_doreliabilitycalc(self):
        """Calculates SAIFI, etc. if the Argument is equal to 1 this parameter will assume restoration,
        otherwise it will not."""
        result = self.dssObj.MetersI(ctypes.c_int32(18), ctypes.c_int32(0))
        return result

    def meters_seqlistsize(self):
        """Returns the size of Sequence List."""
        result = self.dssObj.MetersI(ctypes.c_int32(19), ctypes.c_int32(0))
        return result

    def meters_totalcustomers(self):
        """Returns the total number of customers in this zone (down line from the Energy Meter)."""
        result = self.dssObj.MetersI(ctypes.c_int32(20), ctypes.c_int32(0))
        return result

    def meters_numsections(self):
        """Returns the number of feeder sections in this meter's zone."""
        result = self.dssObj.MetersI(ctypes.c_int32(21), ctypes.c_int32(0))
        return result

    def meters_setactivesection(self):
        """Sets the designated section (argument) if the index is valid."""
        result = self.dssObj.MetersI(ctypes.c_int32(22), ctypes.c_int32(0))
        return result

    def meters_ocpdevicetype(self):
        """Returns the type of OCP device: {1=fuse | 2+ recloser | 3= relay}."""
        result = self.dssObj.MetersI(ctypes.c_int32(23), ctypes.c_int32(0))
        return result

    def meters_numsectioncustomers(self):
        """Returns the number of customers in the active section."""
        result = self.dssObj.MetersI(ctypes.c_int32(24), ctypes.c_int32(0))
        return result

    def meters_numsectionbranches(self):
        """Returns the number of branches (lines) in the active section."""
        result = self.dssObj.MetersI(ctypes.c_int32(25), ctypes.c_int32(0))
        return result

    def meters_sectseqidx(self):
        """Returns the Sequence Index of the branch at the head of this section."""
        result = self.dssObj.MetersI(ctypes.c_int32(26), ctypes.c_int32(0))
        return result

    def meters_secttotalcust(self):
        """Returns the total customers down line from this section."""
        result = self.dssObj.MetersI(ctypes.c_int32(27), ctypes.c_int32(0))
        return result

    # MetersF (Float)
    def meters_saifi(self):
        """Returns SAIFI for this meter's zone. Execute reliability calc method first."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def meters_saifikw(self):
        """Returns the SAIFI based on kW rather than number of customers. Get after reliability calcs."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    def meters_saidi(self):
        """Returns the SAIDI for this meter zone. Execute DoreliabilityCalc first."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def meters_custinterrupts(self):
        """Returns the total customer interruptions for this meter zone based on reliability calcs."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def meters_avgrepairtime(self):
        """Returns the average Repair Time in this Section of the meter zone."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def meters_faultratexrepairhrs(self):
        """Returns the sum of Fault Rate Time Repair Hours in this section of the meter zone."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def meters_sumbranchfltrates(self):
        """Returns the sum of the branch fault rates in this section of the meter's zone."""
        result = float(self.dssObj.MetersF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    # MetersS (String)
    def meters_read_name(self):
        """Returns the active Energy Meter's name."""
        result = ctypes.c_char_p(self.dssObj.MetersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def meters_write_name(self, argument):
        """Sets the active Energy Meter's name."""
        result = ctypes.c_char_p(self.dssObj.MetersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def meters_read_meteredelement(self):
        """Returns the name of the metered element (considering the active Energy Meter)."""
        result = ctypes.c_char_p(self.dssObj.MetersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def meters_write_meteredelement(self, argument):
        """Sets the name of the metered element (considering the active Energy Meter)."""
        result = ctypes.c_char_p(self.dssObj.MetersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # MetersV (Variant)
    def meters_allnames(self):
        """Returns an array of all Energy Meter names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def meters_registernames(self):
        """Returns an array of strings containing the names of the registers."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def meters_registervalues(self):
        """Returns an array of values contained in the Meter registers for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def meters_totals(self):
        """Returns the totals for all registers of all Meters."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_peakcurrent(self):
        """Returns an array of doubles with the Peak Current Property."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_peakcurrent(self, argument):
        """Receives an array of doubles to set values of Peak Current Property."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.MetersV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_calcurrent(self):
        """Returns the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_calcurrent(self, argument):
        """Sets the magnitude of the real part of the Calculated Current (normally determined by solution)
        for the meter to force some behavior on Load Allocation."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.MetersV(ctypes.c_int(7), variant_pointer)
        return variant_pointer.contents.value

    def meters_read_allocfactors(self):
        """Returns an array of doubles: allocation factors for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(8), variant_pointer)
        return variant_pointer.contents.value

    def meters_write_allocfactors(self, argument):
        """Receives an array of doubles to set the phase allocation factors for the active Meter."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.MetersV(ctypes.c_int(9), variant_pointer)
        return variant_pointer.contents.value

    def meters_allendelements(self):
        """Returns a variant array of names of all zone end elements."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(10), variant_pointer)
        return variant_pointer.contents.value

    def meters_allbranchesinzone(self):
        """Returns a wide string list of all branches in zone of the active Energy Meter object."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MetersV(ctypes.c_int(11), variant_pointer)
        return variant_pointer.contents.value

# Monitor Interface

    # MonitorsI (int)
    def monitors_first(self):
        """Sets the first monitor active. Returns 0 if no monitors."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def monitors_next(self):
        """Set the next monitor active. Returns 0 if no more."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def monitors_reset(self):
        """Resets the active Monitor object."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def monitors_resetall(self):
        """Resets all Monitor object."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def monitors_sample(self):
        """Causes active monitor to take a sample."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def monitors_save(self):
        """Causes active monitor to save its current sample buffer to its monitor stream.
        Then you can access the Bytestream or channel data. Most standard solution modes do this automatically."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def monitors_show(self):
        """Converts monitor file into text and displays with text editor."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def monitors_read_mode(self):
        """Returns the monitor mode (bitmask integer - see DSS Help)."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def monitors_write_mode(self, argument):
        """Sets the monitor mode (bitmask integer - see DSS Help)."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def monitors_samplecount(self):
        """Returns number of samples in Monitor at present."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def monitors_sampleall(self):
        """Causes all Monitors to take a sample of the present state. Returns 0."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def monitors_saveall(self):
        """Save all Monitor buffers to their respective file streams. Returns 0."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def monitors_count(self):
        """Returns the number of monitors."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def monitors_process(self):
        """Post-process monitor samples taken so far, e.g., Pst for mode = 4."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def monitors_processall(self):
        """Makes that all monitors post-process the data taken so far."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(14), ctypes.c_int32(0))
        return result

    def monitors_fileversion(self):
        """Returns the Monitor File version (integer)."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(15), ctypes.c_int32(0))
        return result

    def monitors_recordsize(self):
        """Returns the size of each record in ByteStream."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(16), ctypes.c_int32(0))
        return result

    def monitors_numchannels(self):
        """Returns the number of Channels on the active Monitor."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(17), ctypes.c_int32(0))
        return result

    def monitors_read_terminal(self):
        """Returns the terminal number of element being monitored."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(18), ctypes.c_int32(0))
        return result

    def monitors_write_terminal(self, argument):
        """Sets sets the terminal number of element being monitored."""
        result = self.dssObj.MonitorsI(ctypes.c_int32(19), ctypes.c_int32(argument))
        return result

    # MonitorsS (String)
    def monitors_filename(self):
        """Returns the name of the CSV file associated with active monitor."""
        result = ctypes.c_char_p(self.dssObj.MonitorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def monitors_read_name(self):
        """Returns the active Monitor object by name."""
        result = ctypes.c_char_p(self.dssObj.MonitorsS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def monitors_write_name(self, argument):
        """Sets the active Monitor object by name."""
        result = ctypes.c_char_p(self.dssObj.MonitorsS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def monitors_read_element(self):
        """Returns the full name of element being monitored by the active Monitor."""
        result = ctypes.c_char_p(self.dssObj.MonitorsS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def monitors_write_element(self, argument):
        """Sets the full name of element being monitored by the active Monitor."""
        result = ctypes.c_char_p(self.dssObj.MonitorsS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    # MonitorsV (Variant)
    def monitors_allnames(self):
        """Returns an array of all Monitor names (array of strings)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MonitorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def monitors_bytestream(self):
        """Returns a byte array containing monitor stream values.
        Make sure a "save" is done first (standard solution modes do this automatically)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MonitorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def monitors_header(self):
        """Returns the header string; Variant array of strings containing Channel Names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MonitorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def monitors_dblhour(self):
        """Returns returns a variant array of doubles containing time value in hours for the time-sampled monitor
        values; empty if frequency-sampled values for harmonics solution (see dblFreq)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MonitorsV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def monitors_dblfreq(self):
        """Returns a variant array of doubles containing time values for harmonics mode solutions;
        empty for time mode solutions (use dblHour)."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.MonitorsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def monitors_channel(self, argument):
        """Returns a variant array of doubles for the specified channel (usage: MyArray = DSSmonitor.
        Channel(i)) A save or SaveAll should be executed first. Done automatically by most standard solution modes."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.MonitorsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

# Parallel Interface

    # ParallelI (int)
    def parallel_numcpus(self):
        """Returns the number of CPUs available in the local computer."""
        result = self.dssObj.ParallelI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def parallel_numcores(self):
        """Returns the number of physical cores available in the local computer.
        If your computer has less than 64 Cores, this number should be the number of CPUs/2.
        For more information,
        please check: https://www.howtogeek.com/194756/cpu-basics-multiple-cpus-cores-and-hyper-threading-explained/."""
        result = self.dssObj.ParallelI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def parallel_read_activeactor(self):
        """Returns the ID of the active actor."""
        result = self.dssObj.ParallelI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def parallel_write_activeactor(self, argument):
        """Sets the ID of the active actor; this number cannot be higher than the number of existing actors."""
        result = self.dssObj.ParallelI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def parallel_createactor(self):
        """Creates a new actor and sets the active actor ID as the ID for the recently created actor.
        If there are no more CPUs available, the system will not allow the creation of the new actor."""
        result = self.dssObj.ParallelI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def parallel_read_actorcpu(self):
        """Gets the ID of the CPU assigned for the execution of the active actor."""
        result = self.dssObj.ParallelI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def parallel_write_actorcpu(self, argument):
        """Sets the CPU for the execution of the active actor."""
        result = self.dssObj.ParallelI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def parallel_numactors(self):
        """Gets the number of actors created in the actual session."""
        result = self.dssObj.ParallelI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def parallel_wait(self):
        """Waits until all the actors are free and ready to receive a new command."""
        result = self.dssObj.ParallelI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def parallel_read_activeparallel(self):
        """Gets if the parallel features of OpenDSS-PM are active. If active, this parameter will return 1, otherwise,
         will return 0 and OpenDSS-PM will behave sequentially."""
        result = self.dssObj.ParallelI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def parallel_write_activeparallel(self, argument):
        """Sets enables/disables the parallel features of OpenDSS-PM. To enable set the argument in 1, otherwise,
        the argument should be 0 and OpenDSS-PM will behave sequentially."""
        result = self.dssObj.ParallelI(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    def parallel_read_concatenatereportsl(self):
        """Gets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
         executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor
         will be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
         activate the actor of interest and then perform the Show/Export command on the desired monitor."""
        result = self.dssObj.ParallelI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def parallel_write_concatenatereportsl(self, argument):
        """Sets the state of the ConcatenateReports property of OpenDSS-PM. If 1, means that every time the user
        executes a Show/Export monitor operation, the data stored on the monitors with the same name for each actor will
         be concatenated one after the other. Otherwise (0), to get access of each monitor the user will have to
         activate the actor of interest and then perform the Show/Export command on the desired monitor."""
        result = self.dssObj.ParallelI(ctypes.c_int32(12), ctypes.c_int32(argument))
        return result

    # ParallelV (Variant)
    def parallel_actorprogress(self):
        """Returns an array of integers containing the progress in percentage for each active actor."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ParallelV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def parallel_actorstatus(self):
        """Returns an array of integers containing the status of each active actor. If 1, the actor is ready to receive
         new commands, if 0, the actor is busy performing a simulation and cannot take new ?solve? commands at this time.
         However, the actor is capable to deliver values while the simulation is being performed."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ParallelV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

#  Parser Interface

    # ParserI (int)
    def parser_intvalue(self):
        """Returns next parameter as a long integer."""
        result = self.dssObj.ParserI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def parser_resetdelimeters(self):
        """Reset delimiters to their default values."""
        result = self.dssObj.ParserI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def parser_read_autoincrement(self):
        """In this parameter the default is false (0). If true (1) parser automatically advances to next token after
         DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names."""
        result = self.dssObj.ParserI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def parser_write_autoincrement(self, argument):
        """In this parameter the default is false (0). If true (1) parser automatically advances to next token after
         DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names."""
        result = self.dssObj.ParserI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    # ParserF (Float)
    def parser_dblvalue(self):
        """Returns next parameter as a double."""
        result = float(self.dssObj.ParserF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    # ParserS (String)
    def parser_read_cmdstring(self):
        """Gets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_cmdstring(self, argument):
        """Sets a string to be parsed. Loading this string resets the parser to the beginning of the line.
        Then parse off the tokens in sequence."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_nextparam(self):
        """Gets next token and return tag name (before = sign) if any. See Autoincrement."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_strvalue(self):
        """Returns next parameter as a string."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_read_whitespace(self):
        """Gets the characters used for White space in the command string. Default in blank and Tab."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_whitespace(self, argument):
        """Sets the characters used for White space in the command string. Default in blank and Tab."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_beginquote(self):
        """Gets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(6), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_beginquote(self, argument):
        """Sets the string containing the characters for quoting in OpenDSS scripts. Matching pairs defined in EndQuote.
         Default is "([{."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(7), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_endquote(self):
        """Gets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_endquote(self, argument):
        """Sets the string containing the characters, in order, that match the beginning quote characters in BeginQuote.
         Default is ")]}."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(9), argument.encode('ascii')))
        return result.value.decode('ascii')

    def parser_read_delimiters(self):
        """Gets the string defining hard delimiters used to separate token on the command string.
        Default is , and =. The = separates token name from token value. These override whitespaces to separate
        tokens."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def parser_write_delimiters(self, argument):
        """Sets the string defining hard delimiters used to separate token on the command string.
        Default is , and =. The = separates token name from token value. These override whitespace to separate
        tokens."""
        result = ctypes.c_char_p(self.dssObj.ParserS(ctypes.c_int32(11), argument.encode('ascii')))
        return result.value.decode('ascii')

    # ParserV (Variant)
    def parser_vector(self):
        """Returns token as variant array of doubles. For parsing quoted array syntax."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ParserV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def parser_matrix(self):
        """Use this property to parse a Matrix token in OpenDSS format. Returns square matrix of order specified.
        Order same as default fortran order: column by column."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ParserV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def parser_symmatrix(self):
        """Use this property to parse a Matrix token in lower triangular form. Symmetry is forced."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ParserV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# PDElements Interface

    # PDElementsI (int)
    def pdelements_count(self):
        """Gets number of PDElements in active circuit."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def pdelements_first(self):
        """Sets the first enabled PD element to be the active element. Returns 0 if none found."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def pdelements_next(self):
        """Sets the next enabled PD element to be the active element. Returns 0 if none found."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def pdelements_isshunt(self):
        """Sets returns 1 if the PD element should be treated as a shunt element rather than a series element.
        Applies to capacitor and reactor elements in particular."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def pdelements_numcustomers(self):
        """Gets the number of customers in this branch."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def pdelements_totalcustomers(self):
        """Gets the total number of customers from this branch to the end of the zone."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def pdelements_parentpdelement(self):
        """Gets the parent PD element to be the active circuit element. Returns 0 if no more elements upline."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def pdelements_fromterminal(self):
        """Gets the number of the terminal of active PD element that is on the "from" side.
        This is set after the meter zone is determined."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def pdelements_sectionid(self):
        """Gets the integer ID of the feeder section that this PDElement branch is part of."""
        result = self.dssObj.PDElementsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    # PDElementsF (Float)
    def pdelements_read_faultrate(self):
        """Gets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def pdelements_write_faultrate(self, argument):
        """Sets the number of failures per year. For LINE elements: Number of failures per unit length per year."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def pdelements_read_pctpermanent(self):
        """Gets the percent of faults that are permanent (require repair). Otherwise,
        fault is assumed to be transient/temporary."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def pdelements_write_pctpermanent(self, argument):
        """Sets the percent of faults that are permanent (require repair). Otherwise,
        fault is assumed to be transient/temporary."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def pdelements_lambda(self):
        """Gets the failure rate for this branch. Faults per year including length of line."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def pdelements_accumulatedl(self):
        """Gets the accumulated failure rate for this branch on down line."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def pdelements_repairtime(self):
        """Gets the average time to repair a permanent fault on this branch, hours."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def pdelements_totalmiles(self):
        """Gets the total miles of line from this element to the end of the zone. For recloser siting algorithm."""
        result = float(self.dssObj.PDElementsF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    # PDElementsS (String)
    def pdelements_read_name(self):
        """Gets the name of the active PDElement, returns null string if active element id not PDElement."""
        result = ctypes.c_char_p(self.dssObj.PDElementsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def pdelements_write_name(self, argument):
        """Sets the name of the active PDElement, returns null string if active element id not PDElement."""
        result = ctypes.c_char_p(self.dssObj.PDElementsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

# PVsystems Interface

    # PVSystemsI(int)
    def pvsystems_count(self):
        """Returns the number of PVSystem objects currently defined in the active circuit."""
        result = int(self.dssObj.PVsystemsI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def pvsystems_first(self):
        """Sets the first PVSystem to be active; returns 0 if none."""
        result = int(self.dssObj.PVsystemsI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def pvsystems_next(self):
        """Sets the next PVSystem to be active; returns 0 if none."""
        result = int(self.dssObj.PVsystemsI(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result

    def pvsystems_read_idx(self):
        """Gets the active PVSystem by index; 1..Count."""
        result = int(self.dssObj.PVsystemsI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def pvsystems_write_idx(self, argument):
        """Sets the active PVSystem by index; 1..Count.."""
        result = int(self.dssObj.PVsystemsI(ctypes.c_int32(4), ctypes.c_int32(argument)))
        return result

    # PVsystemsF (Float)
    def pvsystems_read_irradiance(self):
        """Gets the present value of the Irradiance property in W/sq-m."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def pvsystems_write_irradiance(self, argument):
        """Sets the present value of the Irradiance property in W/sq-m."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def pvsystems_kw(self):
        """Gets the kW output."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def pvsystems_read_kvar(self):
        """Gets the kvar value."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def pvsystems_write_kvar(self, argument):
        """Sets the kvar value."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(4), ctypes.c_double(argument)))
        return result

    def pvsystems_read_pf(self):
        """Gets the power factor value."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def pvsystems_write_pf(self, argument):
        """Sets the power factor value."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(6), ctypes.c_double(argument)))
        return result

    def pvsystems_read_kvarated(self):
        """Gets the rated kVA of the PVSystem."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    def pvsystems_write_kvarated(self, argument):
        """Sets the rated kVA of the PVSystem."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(8), ctypes.c_double(argument)))
        return result

    def pvsystems_read_pmpp(self):
        """Gets the Pmpp."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(9), ctypes.c_double(0)))
        return result

    def pvsystems_read_kv(self):
        """Gets the kV."""
        result = float(self.dssObj.PVsystemsF(ctypes.c_int32(11), ctypes.c_double(0)))
        return result

    # PVsystemsS (String)
    def pvsystems_read_name(self):
        """Gets the name of the active PVSystem."""
        result = ctypes.c_char_p(self.dssObj.PVsystemsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def pvsystems_write_name(self, argument):
        """Sets the name of the active PVSystem."""
        result = ctypes.c_char_p(self.dssObj.PVsystemsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # PVsystemsV (Variant)
    def pvsystems_allnames(self):
        """Gets the variant array of string containing names of all PVSystems in the circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.PVsystemsV(ctypes.c_int32(0), variant_pointer)
        return variant_pointer.contents.value

# Reclosers Interface

    # ReclosersI (int)
    def reclosers_count(self):
        """Gets number of Reclosers in active circuit."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def reclosers_first(self):
        """Sets first recloser to be active Circuit Element. Returns 0 if none."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def reclosers_next(self):
        """Sets next recloser to be active Circuit Element. Returns 0 if none."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def reclosers_read_monitoredterm(self):
        """Gets the terminal number of Monitored Object for the Recloser."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def reclosers_write_monitoredterm(self, argument):
        """Sets the terminal number of Monitored Object for the Recloser."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def reclosers_read_switchedterm(self):
        """Gets the terminal of the controlled device being switched by the Recloser."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def reclosers_write_switchedterm(self, argument):
        """Sets the terminal of the controlled device being switched by the Recloser."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def reclosers_read_numfast(self):
        """Gets the number of fast shots."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def reclosers_write_numfast(self, argument):
        """Sets the number of fast shots."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def reclosers_read_shots(self):
        """Gets the number of shots to lockout (fast + delayed)."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def reclosers_write_shots(self, argument):
        """Sets the number of shots to lockout (fast + delayed)."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(10), ctypes.c_int32(argument))
        return result

    def reclosers_open(self):
        """Open recloser's controlled element and lock out the recloser."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def reclosers_close(self):
        """Close the switched object controlled by the recloser. Resets recloser to first operation."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def reclosers_read_idx(self):
        """Gets the active recloser by index into the recloser list. 1..Count."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def reclosers_write_idx(self, argument):
        """Sets the active recloser by index into the recloser list. 1..Count."""
        result = self.dssObj.ReclosersI(ctypes.c_int32(14), ctypes.c_int32(argument))
        return result

    # ReclosersF (Float)
    def reclosers_read_phasetrip(self):
        """Gets the phase trip curve multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def reclosers_write_phasetrip(self, argument):
        """Sets the phase trip curve multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def reclosers_read_phaseins(self):
        """Gets the phase instantaneous curve multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def reclosers_write_phaseins(self, argument):
        """Sets the phase instantaneous curve multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def reclosers_read_groundtrip(self):
        """Gets the ground (3I0) trip multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def reclosers_write_groundtrip(self, argument):
        """Sets the ground (3I0) trip multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def reclosers_read_groundinst(self):
        """Gets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def reclosers_write_groundinst(self, argument):
        """Sets the ground (3I0) instantaneous trip setting - curve multiplier or actual amps."""
        result = float(self.dssObj.ReclosersF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    # ReclosersS (String)
    def reclosers_read_name(self):
        """Gets the name of the active Recloser Object."""
        result = ctypes.c_char_p(self.dssObj.ReclosersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_name(self, argument):
        """Sets the name of the active Recloser Object."""
        result = ctypes.c_char_p(self.dssObj.ReclosersS(ctypes.c_int32(1),argument.encode('ascii')))
        return result.value.decode('ascii')

    def reclosers_read_monitoredobj(self):
        """Gets the full name of object this Recloser is monitoring."""
        result = ctypes.c_char_p(self.dssObj.ReclosersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_monitoredobj(self, argument):
        """Sets the full name of object this Recloser is monitoring."""
        result = ctypes.c_char_p(self.dssObj.ReclosersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def reclosers_read_switchedobj(self):
        """Gets the full name of the circuit element that is being switched by this Recloser."""
        result = ctypes.c_char_p(self.dssObj.ReclosersS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def reclosers_write_switchedobj(self, argument):
        """Sets the full name of the circuit element that is being switched by this Recloser."""
        result = ctypes.c_char_p(self.dssObj.ReclosersS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # ReclosersV (Variant)
    def reclosers_allnames(self):
        """Gets a variant array of strings with names of all reclosers in active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ReclosersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def reclosers_recloseintervals(self):
        """Gets a variant array of doubles: reclose intervals (s) between shots."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.ReclosersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

# RegControls Interface

    # RegControlsI (int)
    def regcontrols_first(self):
        """Sets the first RegControl active. Returns 0 if no more."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def regcontrols_next(self):
        """Sets the next RegControl active. Returns 0 if no more"""
        result = self.dssObj.RegControlsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def regcontrols_read_tapwinding(self):
        """Gets the tapped winding number."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def regcontrols_write_tapwinding(self, argument):
        """Sets the tapped winding number."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def regcontrols_read_winding(self):
        """Gets the winding number for PT and CT connections."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def regcontrols_write_winding(self, argument):
        """Sets the winding number for PT and CT connections."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def regcontrols_read_isreversible(self):
        """Gets the setting in the reverse direction, usually not applicable to substation transformers."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def regcontrols_write_isreversible(self, argument):
        """Sets the different settings for the reverse direction (see Manual for details),
        usually not applicable to substation transformers."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def regcontrols_read_isinversetime(self):
        """Gets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def regcontrols_write_isinversetime(self, argument):
        """Sets the inverse time feature. Time delay is inversely adjusted, proportional to the amount of voltage
         outside the regulator band."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(9), ctypes.c_int32(argument))
        return result

    def regcontrols_read_maxtapchange(self):
        """Gets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def regcontrols_write_maxtapchange(self, argument):
        """Sets the maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for
         faster solution."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(11), ctypes.c_int32(argument))
        return result

    def regcontrols_count(self):
        """Gets the number of RegControl objects in Active Circuit."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    def regcontrols_read_tapnumber(self):
        """Gets the actual tap number of the active RegControl."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(13), ctypes.c_int32(0))
        return result

    def regcontrols_write_tapnumber(self, argument):
        """Sets the actual tap number of the active RegControl."""
        result = self.dssObj.RegControlsI(ctypes.c_int32(14), ctypes.c_int32(argument))
        return result

    # RegControlsF (Float)
    def regcontrols_read_ctprimary(self):
        """Gets the CT primary ampere rating (secondary is 0.2 amperes)."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def regcontrols_write_ctprimary(self, argument):
        """Sets the CT primary ampere rating (secondary is 0.2 amperes)."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def regcontrols_read_ptratio(self):
        """Gets the PT ratio for voltage control settings."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def regcontrols_write_ptratio(self, argument):
        """Sets the PT ratio for voltage control settings."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardr(self):
        """Gets the LDC R settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardr(self, argument):
        """Sets the LDC R settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardx(self):
        """Gets the LDC X settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardx(self, argument):
        """Sets sets the LDC X settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reverser(self):
        """Gets the reverse LDC R settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def regcontrols_write_reverser(self, argument):
        """Sets the reverse LDC R settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reverserx(self):
        """Gets the reverse LDC X settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def regcontrols_write_reverserx(self, argument):
        """Sets the reverse LDC X settings in Volts."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def regcontrols_read_delay(self):
        """Gets the time delay [s] after arming before the first tap change.
        Control may reset before actually changing taps."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def regcontrols_write_delay(self, argument):
        """Sets the time delay [s] after arming before the first tap change.
        Control may reset before actually changing taps."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def regcontrols_read_tapdelay(self):
        """Gets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
        taps."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def regcontrols_write_tapdelay(self, argument):
        """Sets the time delay [s] for subsequent tap changes in a set. Control may reset before actually changing
        taps."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def regcontrols_read_voltagelimit(self):
        """Gets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def regcontrols_write_voltagelimit(self, argument):
        """Sets the first house voltage limit on PT secondary base. Setting to 0 disables this function."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardband(self):
        """Gets the regulation bandwidth in forward direction, centered on Vreg."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardband(self, argument):
        """Sets the regulation bandwidth in forward direction, centered on Vreg."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def regcontrols_read_forwardvreg(self):
        """Gets the target voltage in the forward direction, on PT secondary base."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def regcontrols_write_forwardvreg(self, argument):
        """Sets the target voltage in the forward direction, on PT secondary base."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reverseband(self):
        """Gets the bandwidth in reverse direction, centered on reverse Vreg."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def regcontrols_write_reverseband(self, argument):
        """Sets the bandwidth in reverse direction, centered on reverse Vreg."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(23), ctypes.c_double(argument)))
        return result

    def regcontrols_read_reversevreg(self):
        """Gets the target voltage in the reverse direction, on PT secondary base."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    def regcontrols_write_reversevreg(self, argument):
        """Sets the target voltage in the reverse direction, on PT secondary base."""
        result = float(self.dssObj.RegControlsF(ctypes.c_int32(25), ctypes.c_double(argument)))
        return result

    # RegControlsS (String)
    def regcontrols_read_name(self):
        """Gets the active RegControl name."""
        result = ctypes.c_char_p(self.dssObj.RegControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_name(self, argument):
        """Sets the active RegControl name."""
        result = ctypes.c_char_p(self.dssObj.RegControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_monitoredbus(self):
        """Gets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dssObj.RegControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_monitoredbus(self, argument):
        """Sets the name of the remote regulated bus, in lieu of LDC settings."""
        result = ctypes.c_char_p(self.dssObj.RegControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def regcontrols_read_transformer(self):
        """Gets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dssObj.RegControlsS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def regcontrols_write_transformer(self, argument):
        """Sets the name of the transformer this regulator controls."""
        result = ctypes.c_char_p(self.dssObj.RegControlsS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # RegControlsV (Variant)
    def regcontrols_allnames(self):
        """Gets a variant array of strings containing all RegControl names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.RegControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# Relays Interface

    # RelaysI (int)
    def relays_count(self):
        """Gets number of Relays in active circuit."""
        result = self.dssObj.RelaysI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def relays_first(self):
        """Sets first relay active. If none, returns 0."""
        result = self.dssObj.RelaysI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def relays_next(self):
        """Sets next relay active. If none, returns 0."""
        result = self.dssObj.RelaysI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def relays_read_monitoredterm(self):
        """Gets the number of terminal of monitored element that this relay is monitoring."""
        result = self.dssObj.RelaysI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def relays_write_monitoredterm(self, argument):
        """Sets the number of terminal of monitored element that this relay is monitoring."""
        result = self.dssObj.RelaysI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def relays_read_switchedterm(self):
        """Gets the number of terminal of the switched object that will be opened when the relay trips."""
        result = self.dssObj.RelaysI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def relays_write_switchedterm(self, argument):
        """Sets the number of terminal of the switched object that will be opened when the relay trips."""
        result = self.dssObj.RelaysI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def relays_read_idx(self):
        """Gets the active relay by index into the Relay list. 1..Count."""
        result = self.dssObj.RelaysI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def relays_write_idx(self, argument):
        """Sets the active relay by index into the Relay list. 1..Count."""
        result = self.dssObj.RelaysI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    # RelaysS (String)
    def relays_read_name(self):
        """Gets the name of the active Relay."""
        result = ctypes.c_char_p(self.dssObj.RelaysS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_name(self, argument):
        """Sets the name of the active Relay."""
        result = ctypes.c_char_p(self.dssObj.RelaysS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def relays_read_monitoredobj(self):
        """Gets the full name of the object this relay is monitoring."""
        result = ctypes.c_char_p(self.dssObj.RelaysS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_monitoredobj(self, argument):
        """Sets the full name of the object this relay is monitoring."""
        result = ctypes.c_char_p(self.dssObj.RelaysS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def relays_read_switchedobj(self):
        """Gets the full name of element that will switched when relay trips."""
        result = ctypes.c_char_p(self.dssObj.RelaysS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def relays_write_switchedobj(self, argument):
        """Sets the full name of element that will switched when relay trips."""
        result = ctypes.c_char_p(self.dssObj.RelaysS(ctypes.c_int32(5), argument.encode('ascii')))
        return result.value.decode('ascii')

    # RelaysV (Variant)
    def relays_allnames(self):
        """Getsa variant array of strings containing names of all relay elements."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.RelaysV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# Sensors Interface

    # SensorsI (int)
    def sensors_count(self):
        """Gets number of sensors in active circuit."""
        result = self.dssObj.SensorsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def sensors_first(self):
        """Sets the first sensor active. Returns 0 if none."""
        result = self.dssObj.SensorsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def sensors_next(self):
        """Sets the next sensor active. Returns 0 if none."""
        result = self.dssObj.SensorsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def sensors_read_isdelta(self):
        """Returns 1 if the sensor is connected in delta; otherwise, returns 0."""
        result = self.dssObj.SensorsI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def sensors_write_isdelta(self, argument):
        """Allows to set 1 if the sensor is connected in delta; otherwise, set 0 (argument)."""
        result = self.dssObj.SensorsI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def sensors_read_reversedelta(self):
        """Returns 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        result = self.dssObj.SensorsI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def sensors_write_reversedelta(self, argument):
        """Allows to set 1 if voltage measurements are 1-3, 3-2, 2-1; otherwise 0."""
        result = self.dssObj.SensorsI(ctypes.c_int32(6), ctypes.c_int32(argument))
        return result

    def sensors_read_meteredterminal(self):
        """Gets the number of the measured terminal in the measured element."""
        result = self.dssObj.SensorsI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def sensors_write_meteredterminal(self, argument):
        """Sets the number of the measured terminal in the measured element."""
        result = self.dssObj.SensorsI(ctypes.c_int32(8), ctypes.c_int32(argument))
        return result

    def sensors_reset(self):
        """Clears the active sensor."""
        result = self.dssObj.SensorsI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def sensors_resetall(self):
        """Clears all sensors in the active circuit."""
        result = self.dssObj.SensorsI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    # SensorsF (Float)
    def sensors_read_pcterror(self):
        """Gets the assumed percent error in the Sensor measurement. Default is 1."""
        result = float(self.dssObj.SensorsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def sensors_write_pcterror(self, argument):
        """Sets the assumed percent error in the Sensor measurement. Default is 1."""
        result = float(self.dssObj.SensorsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def sensors_read_weight(self):
        """Gets the weighting factor for this sensor measurement with respect to the other sensors. Default is 1."""
        result = float(self.dssObj.SensorsF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def sensors_write_weight(self, argument):
        """Sets the weighting factor for this sensor measurement with respect to the other sensors. Default is 1."""
        result = float(self.dssObj.SensorsF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def sensors_read_kvbase(self):
        """Gets the voltage base for the sensor measurements. LL for 2 and 3 - phase sensors, LN for 1-phase sensors."""
        result = float(self.dssObj.SensorsF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def sensors_write_kvbase(self, argument):
        """Sets the voltage base for the sensor measurements. LL for 2 and 3 - phase sensors, LN for 1-phase sensors."""
        result = float(self.dssObj.SensorsF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    # SensorsS (String)
    def sensors_read_name(self):
        """Gets the name of the active sensor object."""
        result = ctypes.c_char_p(self.dssObj.SensorsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def sensors_write_name(self, argument):
        """Sets the name of the active sensor object."""
        result = ctypes.c_char_p(self.dssObj.SensorsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def sensors_read_meteredelement(self):
        """Gets the full name of the measured element."""
        result = ctypes.c_char_p(self.dssObj.SensorsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def sensors_write_meteredelement(self, argument):
        """Sets the full name of the measured element."""
        result = ctypes.c_char_p(self.dssObj.SensorsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SensorsV (Variant)
    def sensors_allnames(self):
        """Returns a variant array of sensor names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SensorsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def sensors_read_currents(self):
        """Gets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SensorsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def sensors_write_currents(self, argument):
        """Sets an array of doubles for the line current measurements; don't use with KWS and KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.SensorsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def sensors_read_kvars(self):
        """Gets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SensorsV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def sensors_write_kvars(self, argument):
        """Sets an array of doubles for Q measurements; overwrites currents with a new estimate using KWS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.SensorsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def sensors_read_kws(self):
        """Gets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SensorsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def sensors_write_kws(self, argument):
        """Sets an array of doubles for P measurements; overwrites currents with a new estimate using KVARS."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.SensorsV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value

# Settings Interface

    # SettingsI (int)
    def settings_read_allowduplicates(self):
        """Gets if OpenDSS allows duplicate names of objects: {1 allow, 0 not allow}."""
        result = self.dssObj.SettingsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def settings_write_allowduplicates(self, argument):
        """Sets if OpenDSS allows duplicate names of objects: {1 allow, 0 not allow}."""
        result = self.dssObj.SettingsI(ctypes.c_int32(1), ctypes.c_int32(argument))
        return result

    def settings_read_zonelock(self):
        """Gets the status of Lock zones on energy meters to prevent rebuilding if a circuit
        change occurs: {1= true, 0= False}."""
        result = self.dssObj.SettingsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def settings_write_zonelock(self, argument):
        """Sets the status of Lock zones on energy meters to prevent rebuilding if a circuit
        change occurs: {1= true, 0= False}."""
        result = self.dssObj.SettingsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def settings_read_cktmodel(self):
        """Gets {dssMultiphase* | dssPositiveSeq} Indicate if the circuit model is positive sequence."""
        result = self.dssObj.SettingsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def settings_write_cktmodel(self, argument):
        """Sets {dssMultiphase* | dssPositiveSeq} Indicate if the circuit model is positive sequence."""
        result = self.dssObj.SettingsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def settings_read_trapezoidal(self):
        """Gets {True (1) | False (0)} value of trapezoidal integration flag in Energy Meters."""
        result = self.dssObj.SettingsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def settings_write_trapezoidal(self, argument):
        """Sets {True (1) | False (0)} value of trapezoidal integration flag in Energy Meters."""
        result = self.dssObj.SettingsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    # SettingsF (Float)
    def settings_allocationfactors(self):
        """Sets all load allocation factors for all loads defined by XFKVA property to this value."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def settings_read_normvminpu(self):
        """Gets the per unit minimum voltage for Normal conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(1), ctypes.c_double(0)))
        return result

    def settings_write_normvminpu(self, argument):
        """Sets the per unit minimum voltage for Normal conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(2), ctypes.c_double(argument)))
        return result

    def settings_read_normvmaxpu(self):
        """Gets the per unit maximum voltage for Normal conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(3), ctypes.c_double(0)))
        return result

    def settings_write_normvmaxpu(self, argument):
        """Sets the per unit maximum voltage for Normal conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(4), ctypes.c_double(argument)))
        return result

    def settings_read_emergvminpu(self):
        """Gets the per unit minimum voltage for Emergency conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(5), ctypes.c_double(0)))
        return result

    def settings_write_emergvminpu(self, argument):
        """Sets the per unit minimum voltage for Emergency conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(6), ctypes.c_double(argument)))
        return result

    def settings_read_emergvmaxpu(self):
        """Gets the per unit maximum voltage for Emergency conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(7), ctypes.c_double(0)))
        return result

    def settings_write_emergvmaxpu(self, argument):
        """Sets the per unit maximum voltage for Emergency conditions."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(8), ctypes.c_double(argument)))
        return result

    def settings_read_ueweight(self):
        """Gets the weighting factor applied to UE register values."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(9), ctypes.c_double(0)))
        return result

    def settings_write_ueweight(self, argument):
        """Sets the weighting factor applied to UE register values."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(10), ctypes.c_double(argument)))
        return result

    def settings_read_lossweight(self):
        """Gets the weighting factor applied to Loss register values."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(11), ctypes.c_double(0)))
        return result

    def settings_write_lossweight(self, argument):
        """Sets the weighting factor applied to Loss register values."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(12), ctypes.c_double(argument)))
        return result

    def settings_read_pricesignal(self):
        """Gets the price signal for the circuit."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(13), ctypes.c_double(0)))
        return result

    def settings_write_pricesignal(self, argument):
        """Sets the price signal for the circuit."""
        result = float(self.dssObj.SettingsF(ctypes.c_int32(14), ctypes.c_double(argument)))
        return result

    # SettingsS (String)
    def settings_read_autobuslist(self):
        """Gets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        result = ctypes.c_char_p(self.dssObj.SettingsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def settings_write_autobuslist(self, argument):
        """Sets the list of Buses or (File=xxxxx) syntax for the AutoAdd solution mode."""
        result = ctypes.c_char_p(self.dssObj.SettingsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def settings_read_pricecurve(self):
        """Gets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        result = ctypes.c_char_p(self.dssObj.SettingsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def settings_write_pricecurve(self, argument):
        """Sets the name of LoadShape object that serves as the source of price signal data for yearly simulations,
        etc."""
        result = ctypes.c_char_p(self.dssObj.SettingsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SettingsV (Variant)
    def settings_read_ueregs(self):
        """Gets the array of Integers defining Energy Meter registers to use for computing UE."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SettingsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def settings_write_ueregs(self, argument):
        """Sets the array of Integers defining Energy Meter registers to use for computing UE."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.SettingsV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def settings_read_lossregs(self):
        """Gets the array of Integers defining Energy Meter registers to use for computing Losses."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SettingsV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def settings_write_lossregs(self, argument):
        """Sets the array of Integers defining Energy Meter registers to use for computing Losses."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.SettingsV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def settings_read_voltagebases(self):
        """Gets the array of doubles defining the legal voltage bases in kV L-L."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SettingsV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def settings_write_voltagebases(self, argument):
        """Sets the array of doubles defining the legal voltage bases in kV L-L."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.SettingsV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

# Solution Interface

    # SolutionI (int)
    def solution_solve(self):
        """Solution for the present solution mode. Returns 0. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result

    def solution_read_mode(self):
        """Returns the present solution mode (See DSS help). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result

    def solution_write_mode(self, argument):
        """Modifies the present solution mode (See DSS help). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(2), ctypes.c_int32(argument)))
        return result

    def solution_read_hour(self):
        """Returns the present hour (See DSS help). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result

    def solution_write_hour(self, argument):
        """Modifies the present hour (See DSS help). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(4), ctypes.c_int32(argument)))
        return result

    def solution_read_year(self):
        """Returns the present Year (See DSS help). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result

    def solution_write_year(self, argument):
        """Modifies the present Year (See DSS help). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(6), ctypes.c_int32(argument)))
        return result

    def solution_iterations(self):
        """Returns the number of iterations taken for the last solution. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(7), ctypes.c_int32(0)))
        return result

    def solution_read_maxiterations(self):
        """Returns the Maximum number of iterations used to solve the circuit. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(8), ctypes.c_int32(0)))
        return result

    def solution_write_maxiterations(self, argument):
        """Modifies the Maximum number of iterations used to solve the circuit. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(9), ctypes.c_int32(argument)))
        return result

    def solution_read_number(self):
        """Returns the number of solutions to perform for MonteCarlo and time series simulations. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(10), ctypes.c_int32(0)))
        return result

    def solution_write_number(self, argument):
        """Modifies the number of solutions to perform for MonteCarlo and time series simulations. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(11), ctypes.c_int32(argument)))
        return result

    def solution_read_random(self):
        """Returns the randomization mode for random variables "Gaussian" o "Uniform". """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(12), ctypes.c_int32(0)))
        return result

    def solution_write_random(self, argument):
        """Modifies the randomization mode for random variables "Gaussian" o "Uniform". """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(13), ctypes.c_int32(argument)))
        return result

    def solution_read_loadmodel(self):
        """Returns the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(14), ctypes.c_int32(0)))
        return result

    def solution_write_loadmodel(self, argument):
        """Modifies the Load Model: {dssPowerFlow (default)|dssAdmittance}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(15), ctypes.c_int32(argument)))
        return result

    def solution_read_addtype(self):
        """Returns the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(16), ctypes.c_int32(0)))
        return result

    def solution_write_addtype(self, argument):
        """Modifies the type of device to add in AutoAdd Mode: {dssGen (default)|dssCap}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(17), ctypes.c_int32(argument)))
        return result

    def solution_read_algorithm(self):
        """Returns the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(18), ctypes.c_int32(0)))
        return result

    def solution_write_algorithm(self, argument):
        """Modifies the base solution algorithm: {dssNormalSolve | dssNewtonSolve}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(19), ctypes.c_int32(argument)))
        return result

    def solution_read_controlmode(self):
        """Returns the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(20), ctypes.c_int32(0)))
        return result

    def solution_write_controlmode(self, argument):
        """Modifies the mode for control devices: {dssStatic (default) | dssEvent | dssTime}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(21), ctypes.c_int32(argument)))
        return result

    def solution_read_controliterations(self):
        """Returns the current value of the control iteration counter. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(22), ctypes.c_int32(0)))
        return result

    def solution_write_controliterations(self, argument):
        """Modifies the current value of the control iteration counter. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(23), ctypes.c_int32(argument)))
        return result

    def solution_read_maxcontroliterations(self):
        """Returns the maximum allowable control iterations."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(24), ctypes.c_int32(0)))
        return result

    def solution_write_maxcontroliterations(self, argument):
        """Modifies the maximum allowable control iterations. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(25), ctypes.c_int32(argument)))
        return result

    def solution_sampledocontrolactions(self):
        """Sample controls and then process the control queue for present control mode and dispatch control actions.
        Returns 0."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(26), ctypes.c_int32(0)))
        return result

    def solution_checkfaultstatus(self):
        """Executes status check on all fault objects defined in the circuit. Returns 0."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(27), ctypes.c_int32(0)))
        return result

    def solution_solvedirect(self):
        """Executes a direct solution from the system Y matrix, ignoring compensation currents of loads, generators
        (includes Yprim only)."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(28), ctypes.c_int32(0)))
        return result

    def solution_solvepflow(self):
        """Solves using present power flow method. Iterative solution rather than direct solution. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(29), ctypes.c_int32(0)))
        return result

    def solution_solvenocontrol(self):
        """Is similar to SolveSnap except no control actions are checked or executed."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(30), ctypes.c_int32(0)))
        return result

    def solution_solvepluscontrol(self):
        """Executes a power flow solution (SolveNoControl) plus executes a CheckControlActions that executes any
        pending control actions."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(31), ctypes.c_int32(0)))
        return result

    def solution_initsnap(self):
        """Initializes some variables for snap shot power flow. SolveSnap does this automatically."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(32), ctypes.c_int32(0)))
        return result

    def solution_checkcontrols(self):
        """Performs the normal process for sampling and executing Control Actions and Fault Status and rebuilds Y
        if necessary."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(33), ctypes.c_int32(0)))
        return result

    def solution_samplecontroldevices(self):
        """Executes a sampling of all intrinsic control devices, which push control actions into the control queue."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(34), ctypes.c_int32(0)))
        return result

    def solution_docontrolactions(self):
        """Pops control actions off the control queue and dispatches to the proper control element."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(35), ctypes.c_int32(0)))
        return result

    def solution_buildymatrix(self):
        """Forces building of the System Y matrix according to the argument:
        {1= series elements only | 2= Whole Y matrix}."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(36), ctypes.c_int32(0)))
        return result

    def solution_systemychanged(self):
        """Indicates if elements of the System Y have been changed by recent activity. If changed returns 1;
        otherwise 0."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(37), ctypes.c_int32(0)))
        return result

    def solution_read_converged(self):
        """Indicates whether the circuit solution converged (1 converged | 0 not converged)."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(38), ctypes.c_int32(0)))
        return result

    def solution_write_converged(self, argument):
        """Modifies the converged flag (1 converged | 0 not converged). """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(39), ctypes.c_int32(argument)))
        return result

    def solution_totaliterations(self):
        """Returns the total iterations including control iterations for most recent solution."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(40), ctypes.c_int32(0)))
        return result

    def solution_mostiterationsdone(self):
        """Returns the max number of iterations required to converge at any control iteration of the most recent
        solution.."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(41), ctypes.c_int32(0)))
        return result

    def solution_read_controlactionsdone(self):
        """Indicates that the control actions are done: {1 done, 0 not done}."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(42), ctypes.c_int32(0)))
        return result

    def solution_write_controlactionsdone(self, argument):
        """Modifies the flag to indicate that the control actions are done: {1 done, 0 not done}. """
        result = int(self.dssObj.SolutionI(ctypes.c_int32(43), ctypes.c_int32(argument)))
        return result

    def solution_finishtimestep(self):
        """Calls cleanup, sample monitors, and increment time at end of time step."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(44), ctypes.c_int32(0)))
        return result

    def solution_cleanup(self):
        """Update storage, invcontrol, etc., at end of time step."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(45), ctypes.c_int32(0)))
        return result

    def solution_solveall(self):
        """Starts the solution process for all the actors created in memory.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
        command."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(46), ctypes.c_int32(0)))
        return result

    def solution_calcincmatrix(self):
        """Starts the calculation of the incidence matrix for the active actor.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before using this
        command."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(47), ctypes.c_int32(0)))
        return result

    def solution_calcincmatrix_0(self):
        """Starts the calculation of the Branch to Node incidence matrix for the active actor.
        Please be sure that the circuits of each actor have been compiled and ready to be solved before
        using this command. The difference between this command and the CalcIncMatrix is that the calculated matrix
        will be ordered hierarchically from the substation to the feeder end, which can be helpful for many operations.
        Additionally, the Bus Levels vector is calculated and the rows (PDElements) and columns (Buses) are permuted so
        it is easy to identify their position in the circuit."""
        result = int(self.dssObj.SolutionI(ctypes.c_int32(48), ctypes.c_int32(0)))
        return result

    # SolutionF (Float)
    def solution_read_frequency(self):
        """Returns the frequency for the next solution."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def solution_write_frequency(self, argument):
        """Sets the frequency for the next solution."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def solution_read_seconds(self):
        """Returns the seconds from top of the hour."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def solution_write_seconds(self, argument):
        """Sets the seconds from top of the hour."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def solution_read_stepsize(self):
        """Returns the step size for the next solution."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def solution_write_stepsize(self, argument):
        """Sets the step size for the next solution."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def solution_read_loadmult(self):
        """Returns the default load multiplier applied to all non-fixed loads."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def solution_write_loadmult(self, argument):
        """Sets the default load multiplier applied to all non-fixed loads."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def solution_read_tolerance(self):
        """Returns the solution convergence tolerance."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def solution_write_tolerance(self, argument):
        """Sets the solution convergence tolerance."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def solution_read_pctgrowth(self):
        """Returns the percent default annual load growth rate."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def solution_write_pctgrowth(self, argument):
        """Sets the percent default annual load growth rate."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def solution_read_genkw(self):
        """Returns the generator kW for AutoAdd mode."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def solution_write_genkw(self, argument):
        """Sets the generator kW for AutoAdd mode."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def solution_read_genpf(self):
        """Returns the pf for generators in AutoAdd mode."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def solution_write_genpf(self, argument):
        """Sets the pf for generators in AutoAdd mode."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def solution_read_capkvar(self):
        """Returns the capacitor kvar for adding in AutoAdd mode."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def solution_write_capkvar(self, argument):
        """Sets the capacitor kvar for adding in AutoAdd mode."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def solution_read_genmult(self):
        """Returns the default multiplier applied to generators (like LoadMult)."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def solution_write_genmult(self, argument):
        """Sets the default multiplier applied to generators (like LoadMult)."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def solution_read_dblhour(self):
        """Returns the hour as a double, including fractional part."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def solution_write_dblhour(self, argument):
        """Sets the hour as a double, including fractional part."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    def solution_stepsizemin(self):
        """Sets the step size in minutes."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(22), ctypes.c_double(0)))
        return result

    def solution_stepsizehr(self):
        """Sets the step size in Hours."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(23), ctypes.c_double(0)))
        return result

    def solution_processtime(self):
        """Retrieves the time required (microseconds) to perform the latest solution time step,
        this time does not include the time required for sampling meters/monitors."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(24), ctypes.c_double(0)))
        return result

    def solution_read_totaltime(self):
        """Retrieves the accumulated time required (microseconds) to perform the simulation."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(25), ctypes.c_double(0)))
        return result

    def solution_write_totaltime(self, argument):
        """Sets the accumulated time (microseconds) register.
        The new value for this register must be specified in the argument."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(26), ctypes.c_double(argument)))
        return result

    def solution_processtimetimestep(self):
        """Retrieves the time required (microseconds) to perform the latest solution time step including the time
        required for sampling meters/monitors."""
        result = float(self.dssObj.SolutionF(ctypes.c_int32(27), ctypes.c_double(0)))
        return result

    # SolutionS (String)
    def solution_modeid(self):
        """Returns the ID (text) of the present solution mode."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_read_ldcurve(self):
        """Returns the Load-Duration Curve name for LD modes."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(1), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_ldcurve(self, argument):
        """Sets the Load-Duration Curve name for LD modes."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(2), argument.encode('ascii')))
        return result.value.decode('ascii')

    def solution_read_defaultdaily(self):
        """Returns the default daily load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(3), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_defaultdaily(self, argument):
        """Sets the default daily load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(4), argument.encode('ascii')))
        return result.value.decode('ascii')

    def solution_read_defaultyearly(self):
        """Returns the default yearly load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(5), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def solution_write_defaultyearly(self, argument):
        """Sets the default yearly load shape (defaults to "Default")."""
        result = ctypes.c_char_p(self.dssObj.SolutionS(ctypes.c_int32(6), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SolutionV (Variant)
    def solution_eventlog(self):
        """Returns an array of strings containing the Event Log."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SolutionV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def solution_ncmatrix(self):
        """Returns an array of integers containing the incidence matrix (1-D).
        Each cell of the incidence matrix is delivered using 3 elements of the array delivered,
        the first is the row, the second is the column and the third is the value (1/-1).
        This procedure will only deliver the non-zero elements.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SolutionV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def solution_buslevels(self):
        """Returns an array of integers containing BusLevels array.
        This array gives a numeric value to each bus to specify how far it is from the
        circuit?s backbone (a continuous path from the feeder head to the feeder end).
        It is very handy to understand the circuit?s topology."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SolutionV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def solution_incmatrixrows(self):
        """Returns an array of strings specifying the way the rows of the incidence matrix (PDElements) are organized,
         depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
         the result could be very different.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SolutionV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def solution_incmatrixcols(self):
        """Returns an array of strings specifying the way the cols of the incidence matrix (buses) are organized,
        depending on the way the Branch to node incidence matrix was calculated (CalcIncMatrix/CalcIncMatrix_O)
        the result could be very different."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SolutionV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def solution_laplacian(self):
        """Returns an array of integers containing the Laplacian matrix using the incidence matrix previously calculated
        , this means that before calling this command the incidence matrix needs to be calculated using
        calcincmatrix/calcincmatrix_o. This command will return only the non-zero values in compressed coordinate
        format (row, col, value).."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SolutionV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

# SwtControls Interface

    # SwtControlsI (int)
    def swtcontrols_first(self):
        """Sets the first SwtControl active. Returns 0 if no more."""
        result = self.dssObj.SwtControlsI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def swtcontrols_next(self):
        """Sets the next SwtControl active. Returns 0 if no more."""
        result = self.dssObj.SwtControlsI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def swtcontrols_read_action(self):
        """Gets the open (1) or close (2) action of the switch. No effect if switch is locked.
        However, reset removes any lock and then closes the switch (shelf state). 0 = none action."""
        result = self.dssObj.SwtControlsI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def swtcontrols_write_action(self, argument):
        """Sets open (1) or close (2) the switch. No effect if switch is locked. However,
        reset removes any lock and then closes the switch (shelf state). 0 = none action (see manual for details). """
        result = self.dssObj.SwtControlsI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def swtcontrols_read_islocked(self):
        """Gets the lock state: {1 locked | 0 not locked}."""
        result = self.dssObj.SwtControlsI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def swtcontrols_write_islocked(self, argument):
        """Sets the lock to prevent both manual and automatic switch operation. """
        result = self.dssObj.SwtControlsI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def swtcontrols_read_switchedterm(self):
        """Gets the terminal number where the switch is located on the SwitchedObj."""
        result = self.dssObj.SwtControlsI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def swtcontrols_write_switchedterm(self, argument):
        """Sets the terminal number where the switch is located on the SwitchedObj. """
        result = self.dssObj.SwtControlsI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def swtcontrols_count(self):
        """Gets the total number of SwtControls in the active circuit."""
        result = self.dssObj.SwtControlsI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    # SwtControlsF (Float)
    def swtcontrols_read_delay(self):
        """Gets the time delay [s] between arming and opening or closing the switch.
        Control may reset before actually operating the switch."""
        result = float(self.dssObj.SwtControlsF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def swtcontrols_write_delay(self, argument):
        """Sets sets the time delay [s] between arming and opening or closing the switch.
        Control may reset before actually operating the switch."""
        result = float(self.dssObj.SwtControlsF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    # SwtControlsS (string)
    def swtcontrols_read_name(self):
        """Gets the active swtcontrol name."""
        result = ctypes.c_char_p(self.dssObj.SwtControlsS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def swtcontrols_write_name(self, argument):
        """Sets the active swtcontrol by name."""
        result = ctypes.c_char_p(self.dssObj.SwtControlsS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def swtcontrols_read_switchedobj(self):
        """Gets the name of the switched object by the active SwtControl """
        result = ctypes.c_char_p(self.dssObj.SwtControlsS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def swtcontrols_write_switchedobj(self, argument):
        """Sets the switched object by name."""
        result = ctypes.c_char_p(self.dssObj.SwtControlsS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # SwtControlsV (Variant)
    def swtcontrols_allnames(self):
        """Gets a variant array of strings with all SwtControl names in the active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.SwtControlsV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# Text Interface
    def text(self, argument):
        """Can be used to send commands to the text interface of OpenDSS (DSS.Text)."""
        # ctypes.c_char_p(self.dssObj.DSSPut_Command(argument.encode('ascii')))
        result = ctypes.c_char_p(self.dssObj.DSSPut_Command(argument.encode('ascii')))
        return result.value.decode("ascii")

# Topology Interface

    # TopologyI (int)
    def topology_numloops(self):
        """Gets the number of loops."""
        result = self.dssObj.TopologyI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def topology_numisolatedbranches(self):
        """Gets the number of isolated branches (PD elements and capacitors)."""
        result = self.dssObj.TopologyI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def topology_numisolatedloadss(self):
        """Gets the number of isolated loads."""
        result = self.dssObj.TopologyI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def topology_first(self):
        """Sets the first branch active, returns 0 if none."""
        result = self.dssObj.TopologyI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def topology_next(self):
        """Sets the next branch active, returns 0 if none."""
        result = self.dssObj.TopologyI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def topology_activebranch(self):
        """Returns the index of the active Branch."""
        result = self.dssObj.TopologyI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def topology_forwardbranch(self):
        """Moves forward in the tree, return index of new active branch or 0 if no more."""
        result = self.dssObj.TopologyI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def topology_backwardbranch(self):
        """Moves back toward the source, return index of new active branch or 0 if no more."""
        result = self.dssObj.TopologyI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def topology_loopedbranch(self):
        """Moves to looped branch, return index or 0 if none."""
        result = self.dssObj.TopologyI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def topology_parallelbranch(self):
        """Mode to directly parallel branch, return index or 0 if none."""
        result = self.dssObj.TopologyI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def topology_firstload(self):
        """Sets as active load the first load at the active branch, return index or 0 if none."""
        result = self.dssObj.TopologyI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def topology_nextload(self):
        """Sets as active load the next load at the active branch, return index or 0 if none."""
        result = self.dssObj.TopologyI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def topology_activelevel(self):
        """Gets the topological depth of the active branch."""
        result = self.dssObj.TopologyI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

    # TopologyS (String)
    def topology_read_branchname(self):
        """Gets the name of the active branch."""
        result = ctypes.c_char_p(self.dssObj.TopologyS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def topology_write_branchname(self, argument):
        """Sets the name of the active branch."""
        result = ctypes.c_char_p(self.dssObj.TopologyS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def topology_read_busname(self):
        """Gets the name of the active Bus."""
        result = ctypes.c_char_p(self.dssObj.TopologyS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def topology_write_busname(self, argument):
        """Sets the Bus active by name."""
        result = ctypes.c_char_p(self.dssObj.TopologyS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    # TopologyV (Variant)
    def topology_allloopedpairs(self):
        """Gets a variant array of all looped element names, by pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.TolopolgyV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def topology_allisolatedbranches(self):
        """Gets a variant array of all isolated branch names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.TolopolgyV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def topology_allisolatedloads(self):
        """Gets a variant array of all isolated load names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.TolopolgyV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# Transformers Interface

    # TransformersI (int)
    def transformers_read_numwindings(self):
        """Gets the number of windings on this transformer. Allocates memory; set or change this property first."""
        result = self.dssObj.TransformersI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def transformers_write_numwindings(self, argument):
        """Sets the number of windings on this transformer. Allocates memory; set or change this property first."""
        result = self.dssObj.TransformersI(ctypes.c_int32(1), ctypes.c_int32(argument))
        return result

    def transformers_read_wdg(self):
        """Gets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        result = self.dssObj.TransformersI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def transformers_write_wdg(self, argument):
        """Sets the active winding number from 1..NumWindings.
        Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)."""
        result = self.dssObj.TransformersI(ctypes.c_int32(3), ctypes.c_int32(argument))
        return result

    def transformers_read_numtaps(self):
        """Gets the active winding number of tap steps between MinTap and MaxTap."""
        result = self.dssObj.TransformersI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def transformers_write_numtaps(self, argument):
        """Sets the active winding number of tap steps between MinTap and MaxTap."""
        result = self.dssObj.TransformersI(ctypes.c_int32(5), ctypes.c_int32(argument))
        return result

    def transformers_read_isdelta(self):
        """Gets the information about if the active winding is delta (1) or wye (0) connection."""
        result = self.dssObj.TransformersI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def transformers_write_isdelta(self, argument):
        """Sets the information about if the active winding is delta (1) or wye (0) connection."""
        result = self.dssObj.TransformersI(ctypes.c_int32(7), ctypes.c_int32(argument))
        return result

    def transformers_first(self):
        """Sets the first Transformer active. Return 0 if no more."""
        result = self.dssObj.TransformersI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def transformers_next(self):
        """Sets the next Transformer active. Return 0 if no more."""
        result = self.dssObj.TransformersI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def transformers_count(self):
        """Gets the number of transformers within the active circuit."""
        result = self.dssObj.TransformersI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    # TransformersF (Float)
    def transformers_read_r(self):
        """Gets the active winding resistance in %."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def transformers_write_r(self, argument):
        """Sets the active winding resistance in %."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def transformers_read_tap(self):
        """Gets the active winding tap in per-unit."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def transformers_write_tap(self, argument):
        """Sets the active winding tap in per-unit."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def transformers_read_mintap(self):
        """Gets the active winding minimum tap in per-unit."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def transformers_write_mintap(self, argument):
        """Sets the active winding minimum tap in per-unit."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def transformers_read_maxtap(self):
        """Gets the active winding maximum tap in per-unit."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def transformers_write_maxtap(self, argument):
        """Sets the active winding maximum tap in per-unit."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def transformers_read_kv(self):
        """Gets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def transformers_write_kv(self, argument):
        """Sets the active winding kV rating. Phase-phase for 2 or 3 phases, actual winding kV 1 phase transformer."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def transformers_read_kva(self):
        """Gets the active winding kVA rating. On winding 1, this also determines normal and
        emergency current ratings for all windings."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def transformers_write_kva(self, argument):
        """Sets the active winding kVA rating. On winding 1, this also determines normal and
        emergency current ratings for all windings."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    def transformers_read_xneut(self):
        """Gets the active winding neutral reactance [ohms] for wye connections."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(12), ctypes.c_double(0)))
        return result

    def transformers_write_xneut(self, argument):
        """Sets the active winding neutral reactance [ohms] for wye connections."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(13), ctypes.c_double(argument)))
        return result

    def transformers_read_rneut(self):
        """Gets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(14), ctypes.c_double(0)))
        return result

    def transformers_write_rneut(self, argument):
        """Sets the active winding neutral resistance [ohms] for wye connections. Set less than zero ungrounded wye."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(15), ctypes.c_double(argument)))
        return result

    def transformers_read_xhl(self):
        """Gets the percent reactance between windings 1 and 2, on winding 1 kVA base.
        Use for 2 winding or 3 winding transformers."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(16), ctypes.c_double(0)))
        return result

    def transformers_write_xhl(self, argument):
        """Sets the percent reactance between windings 1 and 2, on winding 1 kVA base.
        Use for 2 winding or 3 winding transformers."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(17), ctypes.c_double(argument)))
        return result

    def transformers_read_xht(self):
        """Gets the percent reactance between windings 1 and 3, on winding 1 kVA base. Use for 3 winding transformers
        only."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(18), ctypes.c_double(0)))
        return result

    def transformers_write_xht(self, argument):
        """Sets the percent reactance between windings 1 and 3, on winding 1 kVA base.
        Use for 3 winding transformers only."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(19), ctypes.c_double(argument)))
        return result

    def transformers_read_xlt(self):
        """Gets he percent reactance between windings 2 and 3, on winding 1 kVA base.
         Use for 3 winding transformers only."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(20), ctypes.c_double(0)))
        return result

    def transformers_write_xlt(self, argument):
        """Sets the percent reactance between windings 2 and 3, on winding 1 kVA base.
        Use for 3 winding transformers only."""
        result = float(self.dssObj.TransformersF(ctypes.c_int32(21), ctypes.c_double(argument)))
        return result

    # TransformersS (String)
    def transformers_read_xfmrcode(self):
        """Gets the name of an XfrmCode that supplies electrical parameters for this transformer."""
        result = ctypes.c_char_p(self.dssObj.TransformersS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def transformers_write_xfmrcode(self, argument):
        """Sets the name of an XfrmCode that supplies electrical parameters for this transformer."""
        result = ctypes.c_char_p(self.dssObj.TransformersS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def transformers_read_name(self):
        """Gets the active transformer name."""
        result = ctypes.c_char_p(self.dssObj.TransformersS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def transformers_write_name(self, argument):
        """Sets the active transformer by name."""
        result = ctypes.c_char_p(self.dssObj.TransformersS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

    def transformers_strwdgvoltages(self):
        """Gets the voltages at the active winding of the active transformer in string format."""
        result = ctypes.c_char_p(self.dssObj.TransformersS(ctypes.c_int32(4), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    # TransformersV (Variant)
    def transformers_allNames(self):
        """Gets a variant array of strings with all Transformer names in the active circuit."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.TransformersV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def transformers_wdgvoltages(self):
        """Gets a variant array of doubles containing the voltages at the active winding on the active transformer.
        These voltages come as complex pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.TransformersV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def transformers_wdgcurrents(self):
        """Gets a a variant array of doubles containing the currents at the active winding on the active transformer.
        These currents come as complex pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.TransformersV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

# VSources Interface

    # VSourcesI (int)
    def vsources_count(self):
        """Returns the number of VSource objects currently defined in the active circuit."""
        result = self.dssObj.VsourcesI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def vsources_first(self):
        """Sets the first VSource to be active; returns 0 if none."""
        result = self.dssObj.VsourcesI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def vsources_next(self):
        """Sets the next VSource to be active; returns 0 if none."""
        result = self.dssObj.VsourcesI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def vsources_read_phases(self):
        """Gets the number of phases of the active VSource."""
        result = self.dssObj.VsourcesI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def vsources_write_phases(self, argument):
        """Sets the number of phases of the active VSource."""
        result = self.dssObj.VsourcesI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    # VSourcesF (Float)
    def vsources_read_basekv(self):
        """Gets the source voltage in kV."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def vsources_write_basekv(self, argument):
        """Sets the source voltage in kV."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def vsources_read_pu(self):
        """Gets the source voltage in pu."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def vsources_write_pu(self, argument):
        """Sets the source voltage in pu."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def vsources_read_angledeg(self):
        """Gets the source phase angle of first phase in degrees."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def vsources_write_angledeg(self, argument):
        """Sets the source phase angle of first phase in degrees."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def vsources_read_frequency(self):
        """Gets the source frequency in Hz."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def vsources_write_frequency(self, argument):
        """Sets the source frequency in Hz."""
        result = float(self.dssObj.VsourcesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    # VSourcesS (String)
    def vsources_read_name(self):
        """Gets the name of the active VSource."""
        result = ctypes.c_char_p(self.dssObj.VsourcesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def vsources_write_name(self, argument):
        """Sets the name of the active VSource."""
        result = ctypes.c_char_p(self.dssObj.VsourcesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # VSourcesV (Variant)
    def vsources_allnames(self):
        """Gets the name of the active VSource."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.VsourcesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

# XYCurves Interface

    # XYCurves (int)
    def xycurves_count(self):
        """Gets number of XYCurves in active circuit."""
        result = self.dssObj.XYCurveI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def xycurves_first(self):
        """Sets first XYCurves object active; returns 0 if none."""
        result = self.dssObj.XYCurveI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def xycurves_next(self):
        """Sets next XYCurves object active; returns 0 if none."""
        result = self.dssObj.XYCurveI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def xycurves_read_npts(self):
        """Gets the number of points in X-Y curve."""
        result = self.dssObj.XYCurveI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def xycurves_write_npts(self, argument):
        """Sets the number of points in X-Y curve."""
        result = self.dssObj.XYCurveI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    # XYCurvesF (Float)
    def xycurves_read_x(self):
        """Gets the interpolated value after setting Y."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def xycurves_write_x(self, argument):
        """Sets the X value."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def xycurves_read_y(self):
        """Gets the interpolated value after setting X."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def xycurves_write_y(self, argument):
        """Sets the Y value."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def xycurves_read_xshift(self):
        """Gets the amount to shift X value from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def xycurves_write_xshift(self, argument):
        """Sets the amount to shift X value from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def xycurves_read_yshift(self):
        """Gets the amount to shift Y value from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def xycurves_write_yshift(self, argument):
        """Sets the amount to shift Y value from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def xycurves_read_xscale(self):
        """Gets the factor to scale X values from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def xycurves_write_xscale(self, argument):
        """Sets the factor to scale X values from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    def xycurves_read_yscale(self):
        """Gets the factor to scale Y values from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(10), ctypes.c_double(0)))
        return result

    def xycurves_write_yscale(self, argument):
        """Sets the factor to scale Y values from original curve."""
        result = float(self.dssObj.XYCurvesF(ctypes.c_int32(11), ctypes.c_double(argument)))
        return result

    # XYCurvesS (String)
    def xycurves_read_name(self):
        """Gets the name of the active XYCurve Object."""
        result = ctypes.c_char_p(self.dssObj.XYCurvesS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def xycurves_write_name(self, argument):
        """Sets the name of the active XYCurve Object."""
        result = ctypes.c_char_p(self.dssObj.XYCurvesS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    # XYCurvesV (Variant)
    def xycurves_read_xarray(self):
        """Gets the X values as a variant array of doubles. Set Npts to max number expected if setting."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.XYCurvesV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def xycurves_write_xarray(self, argument):
        """Sets the X values as a variant array of doubles specified in Argument. Set Npts to max number expected
        if setting."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.XYCurvesV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def xycurves_read_yarray(self):
        """Gets the Y values as a variant array of doubles. Set Npts to max number expected if setting.."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dssObj.XYCurvesV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value

    def xycurves_write_yarray(self, argument):
        """Sets the Y values as a variant array of doubles specified in Argument. Set Npts to max number expected
        if setting."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dssObj.XYCurvesV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value
