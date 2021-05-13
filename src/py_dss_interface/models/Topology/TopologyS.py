# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Topology(Base):


        # TopologyS (String)

    def topology_read_branchname(self):
        """Gets the name of the active branch."""
        result = ctypes.c_char_p(self.dss_obj.TopologyS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def topology_write_branchname(self, argument):
        """Sets the name of the active branch."""
        result = ctypes.c_char_p(self.dss_obj.TopologyS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    def topology_read_busname(self):
        """Gets the name of the active Bus."""
        result = ctypes.c_char_p(self.dss_obj.TopologyS(ctypes.c_int32(2), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def topology_write_busname(self, argument):
        """Sets the Bus active by name."""
        result = ctypes.c_char_p(self.dss_obj.TopologyS(ctypes.c_int32(3), argument.encode('ascii')))
        return result.value.decode('ascii')

        # TopologyV (Variant)

    def topology_allloopedpairs(self):
        """Gets a variant array of all looped element names, by pairs."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TolopolgyV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def topology_allisolatedbranches(self):
        """Gets a variant array of all isolated branch names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TolopolgyV(ctypes.c_int(1), variant_pointer)
        return variant_pointer.contents.value

    def topology_allisolatedloads(self):
        """Gets a variant array of all isolated load names."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.TolopolgyV(ctypes.c_int(2), variant_pointer)
        return variant_pointer.contents.value
