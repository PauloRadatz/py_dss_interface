# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation
from py_dss_interface.models.Base import Base


class Topology(Base):
    # TopologyI (int)

    def topology_numloops(self):
        """Gets the number of loops."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def topology_numisolatedbranches(self):
        """Gets the number of isolated branches (PD elements and capacitors)."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def topology_numisolatedloadss(self):
        """Gets the number of isolated loads."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def topology_first(self):
        """Sets the first branch active, returns 0 if none."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def topology_next(self):
        """Sets the next branch active, returns 0 if none."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(4), ctypes.c_int32(0))
        return result

    def topology_activebranch(self):
        """Returns the index of the active Branch."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def topology_forwardbranch(self):
        """Moves forward in the tree, return index of new active branch or 0 if no more."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def topology_backwardbranch(self):
        """Moves back toward the source, return index of new active branch or 0 if no more."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(7), ctypes.c_int32(0))
        return result

    def topology_loopedbranch(self):
        """Moves to looped branch, return index or 0 if none."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(8), ctypes.c_int32(0))
        return result

    def topology_parallelbranch(self):
        """Mode to directly parallel branch, return index or 0 if none."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(9), ctypes.c_int32(0))
        return result

    def topology_firstload(self):
        """Sets as active load the first load at the active branch, return index or 0 if none."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(10), ctypes.c_int32(0))
        return result

    def topology_nextload(self):
        """Sets as active load the next load at the active branch, return index or 0 if none."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(11), ctypes.c_int32(0))
        return result

    def topology_activelevel(self):
        """Gets the topological depth of the active branch."""
        result = self.dss_obj.TopologyI(ctypes.c_int32(12), ctypes.c_int32(0))
        return result

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
