# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import ctypes
from comtypes import automation

from py_dss_interface.models import Bridge
from py_dss_interface.models.Base import Base


class LoadShapesV(Base):
    """
    This interface implements the LoadShape (ILoadShape) interface of OpenDSS by declaring 4 procedures for accessing
    the different properties included in this interface: .
    """

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        int32_t LoadShapeI(int32_t Parameter, int32_t Argument);

    This interface returns an integer number with the result of the query according to the value of the variable
    Parameter, which can be one of the following.
    """

    def loadshapes_count(self):
        """Returns the number of LoadShape objects currently defined in LoadShape collection."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(0), ctypes.c_int32(0))
        return result

    def loadshapes_first(self):
        """sets the first loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(1), ctypes.c_int32(0))
        return result

    def loadshapes_next(self):
        """Sets the next loadshape active and return integer index of the loadshape. Returns 0 if no more."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(2), ctypes.c_int32(0))
        return result

    def loadshapes_read_npts(self):
        """Gets the number of points in active LoadShape."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(3), ctypes.c_int32(0))
        return result

    def loadshapes_write_npts(self, argument):
        """Sets the number of points in active LoadShape."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(4), ctypes.c_int32(argument))
        return result

    def loadshapes_normalize(self):
        """Normalizes the P and Q curves based on either Pbase, Qbase or simply the peak value of the curve."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(5), ctypes.c_int32(0))
        return result

    def loadshapes_read_useactual(self):
        """Gets a TRUE/FALSE (1/0) to let Loads know to use the actual value in the curve rather than use the value as
         a multiplier."""
        result = self.dss_obj.LoadShapeI(ctypes.c_int32(6), ctypes.c_int32(0))
        return result

    def loadshapes_write_useactual(self, argument):
        """Sets a TRUE/FALSE (1/0 - Argument) to let Loads know to use the actual value in the curve rather than use
         the value as a multiplier."""
        return self.dss_obj.LoadShapeI(ctypes.c_int32(7), ctypes.c_int32(argument))

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        double LoadShapeF(int32_t Parameter, double Argument);

    This interface returns a floating point number with the result of the query according to the value of the
    variable Parameter, which can be one of the following.
    """

    def loadshapes_read_hrinterval(self):
        """Gets the fixed interval time value, hours."""
        result = float(self.dss_obj.LoadShapeF(ctypes.c_int32(0), ctypes.c_double(0)))
        return result

    def loadshapes_write_hrinterval(self, argument):
        """Sets the fixed interval time value, hours."""
        result = float(self.dss_obj.LoadShapeI(ctypes.c_int32(1), ctypes.c_double(argument)))
        return result

    def loadshapes_read_mininterval(self):
        """Gets the fixed interval time value, in minutes."""
        result = float(self.dss_obj.LoadShapeF(ctypes.c_int32(2), ctypes.c_double(0)))
        return result

    def loadshapes_write_mininterval(self, argument):
        """Sets the fixed interval time value, in minutes."""
        result = float(self.dss_obj.LoadShapeI(ctypes.c_int32(3), ctypes.c_double(argument)))
        return result

    def loadshapes_read_pbase(self):
        """Gets the base for normalizing P curve. If left at zero, the peak value is used."""
        result = float(self.dss_obj.LoadShapeF(ctypes.c_int32(4), ctypes.c_double(0)))
        return result

    def loadshapes_write_pbase(self, argument):
        """Sets the base for normalizing P curve. If left at zero, the peak value is used."""
        result = float(self.dss_obj.LoadShapeI(ctypes.c_int32(5), ctypes.c_double(argument)))
        return result

    def loadshapes_read_qbase(self):
        """Gets the base for normalizing Q curve. If left at zero, the peak value is used."""
        result = float(self.dss_obj.LoadShapeF(ctypes.c_int32(6), ctypes.c_double(0)))
        return result

    def loadshapes_write_qbase(self, argument):
        """Sets the base for normalizing Q curve. If left at zero, the peak value is used."""
        result = float(self.dss_obj.LoadShapeI(ctypes.c_int32(7), ctypes.c_double(argument)))
        return result

    def loadshapes_read_sinterval(self):
        """Gets the fixed interval data time interval, seconds."""
        result = float(self.dss_obj.LoadShapeF(ctypes.c_int32(8), ctypes.c_double(0)))
        return result

    def loadshapes_write_sinterval(self, argument):
        """Sets the fixed interval data time interval, seconds."""
        result = float(self.dss_obj.LoadShapeI(ctypes.c_int32(9), ctypes.c_double(argument)))
        return result

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        CStr LoadShapeS(int32_t Parameter, CStr Argument);

    This interface returns string with the result of the query according to the value of the variable Parameter,
    which can be one of the following.
    """
    def loadshapes_read_name(self):
        """Gets the name of the active LoadShape object."""
        result = ctypes.c_char_p(self.dss_obj.LoadShapeS(ctypes.c_int32(0), ctypes.c_int32(0)))
        return result.value.decode('ascii')

    def loadshapes_write_name(self, argument):
        """Sets the name of the active LoadShape object."""
        result = ctypes.c_char_p(self.dss_obj.LoadShapeS(ctypes.c_int32(1), argument.encode('ascii')))
        return result.value.decode('ascii')

    """
    This interface can be used to read/write certain properties of the active DSS object.

    The structure of the interface is as follows:
        void LoadShapeV(int32_t Parameter, VARIANT *Argument);

    This interface returns a Variant with the result of the query according to the value of the variable Parameter,
    which can be one of the following. """
    def loadshapes_allnames(self):
        """Gets a variant array of strings containing names of all LoadShape objects currently defined."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LoadShapeV(ctypes.c_int(0), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_read_pmult(self):
        """Gets a variant array of doubles for the P multiplier in the LoadShape."""
        # variant_pointer = ctypes.pointer(automation.VARIANT())
        # self.dss_obj.LoadShapeV(ctypes.c_int(1), variant_pointer)
        # aqui = variant_pointer.contents.value
        # # print(f'Aqui1: {aqui}')
        aqui = Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, ctypes.c_int(1), ctypes.c_int(0), None)
        # print(aqui)
        # print(type(aqui))
        return aqui

    def loadshapes_write_pmult(self, argument):
        """Sets a variant array of doubles for the P multiplier in the LoadShape."""
        # print(fr'Argument {argument}')
        # variant_pointer = ctypes.pointer(automation.VARIANT())
        # print(variant_pointer.__dir__())
        # variant_pointer.contents.value = argument
        points = ctypes.c_float * len(argument)
        # # print(f'points: {points}')
        ppoints = ctypes.POINTER(points)
        # # print(f'ppoints: {ppoints}')
        ppoints.contents = argument
        # print(f'ppoints: {ppoints.contents}')

        # variant_pointer = ctypes.POINTER(ctypes.c_float)
        # print(f'variant_pointer.contents: {variant_pointer.contents}')
        # variant_pointer.contents.value = argument
        # print(f'variant_pointer: {variant_pointer}')

        # aqui = Bridge.ArrayEnio()
        # # print(f'aqui: {aqui}')
        # # print(f'aqui.data {aqui.data}')
        #
        # aqui.size = len(argument)  # print(f'aqui.size: {aqui.size}')
        #
        # aqui.data = (ctypes.c_float * aqui.size)()  # print(f'aqui.data {aqui.data}')
        #
        # for i in range(aqui.size):
        #     aqui.data[i] = argument[i]
        # # print(aqui.data[i])
        #
        # data = ctypes.cast(aqui.data, ctypes.POINTER(ctypes.c_double * aqui.size))
        #
        # # Converting CFloat to Python float, more efficiency could be gained by using NumPy
        # # TODO: Consider making numpy/pandas a dependency?
        # l = []
        # for i in data.contents:
        #     l.append(i)
        # print(l)

        # print(f'argument : {argument}')
        # values_ = (ctypes.c_float*aqui.size)
        # print(f'values: {values_}')
        # aqui.data.contents.value = argument
        # aqui.data = ctypes.POINTER()()
        # print(f'aqui: {aqui.data}')

        # print(type(variant_pointer.contents))
        # print(f'Aqui 2 {variant_pointer.contents.value}')
        # print(type(ppoints))
        # oi = self.dss_obj.LoadShapeV(ctypes.c_int(2), ppoints)
        # print(f'oi: {oi}')
        # return oi

        return Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, ctypes.c_int(1), ctypes.c_int(0), None)

        # return Bridge.VarArrayFunction(self.dss_obj.LoadShapeV, l, ctypes.c_int(2), 'None')
        # print(f'Aqui 3 {variant_pointer.contents.value}')
        # return aqui.data

    def loadshapes_read_qmult(self):
        """Gets a variant array of doubles for the Q multiplier in the LoadShape."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LoadShapeV(ctypes.c_int(3), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_write_qmult(self, argument):
        """Sets a variant array of doubles for the Q multiplier in the LoadShape."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LoadShapeV(ctypes.c_int(4), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_read_timearray(self):
        """Gets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        self.dss_obj.LoadShapeV(ctypes.c_int(5), variant_pointer)
        return variant_pointer.contents.value

    def loadshapes_write_timearray(self, argument):
        """Sets a time array in hours corresponding to P and Q multipliers when the Interval = 0."""
        variant_pointer = ctypes.pointer(automation.VARIANT())
        variant_pointer.contents.value = argument
        self.dss_obj.LoadShapeV(ctypes.c_int(6), variant_pointer)
        return variant_pointer.contents.value
