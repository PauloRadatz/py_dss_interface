# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 30/04/2021
"""
import ctypes
from typing import List


def pointer_read(f: callable, param: int, optional=None) -> List:
    f.argtypes = [
        ctypes.c_long,
        ctypes.POINTER(ctypes.c_void_p),
        ctypes.POINTER(ctypes.c_long),
        ctypes.POINTER(ctypes.c_long)
    ]

    f.restype = None

    myPointer = ctypes.c_void_p()  # Create a pointer variable
    myType = ctypes.c_long()
    mySize = ctypes.c_long()

    if optional:
        if type(optional) == list:
            op_list = optional
        else:
            op_list = [optional]
        c_array = (ctypes.c_int32 * len(op_list))(*op_list)
        myPointer = ctypes.cast(c_array, ctypes.c_void_p)

        f(param, ctypes.byref(myPointer), ctypes.byref(myType), ctypes.byref(mySize))
    else:
        f(
            param,
            ctypes.byref(myPointer),
            ctypes.byref(myType),
            ctypes.byref(mySize)
        )

    # 0 - Boolean, 1- Integer (32 bit), 2- double (64 bit), 3- Complex, 4- String, 5-byte stream.
    num_type = 0
    if myType.value == 1:
        c_type = ctypes.c_int32
        num_type = 4
    elif myType.value == 2:
        c_type = ctypes.c_double
        num_type = 8
    elif myType.value == 3:
        c_type = ctypes.c_double
        num_type = 8
    elif myType.value == 4:
        c_type = ctypes.c_char
        num_type = 1
    elif myType.value == 5:  # byte stream
        c_type = ctypes.c_char
        num_type = 1
    # Access the returned array
    array_length = int(mySize.value / num_type)
    # data_array = ctypes.cast(myPointer, ctypes.POINTER(ctypes.c_int * array_length)).contents

    if not bool(myPointer):
        return []
    data_array = ctypes.cast(myPointer, ctypes.POINTER(c_type * array_length)).contents

    # Convert the data_array to a Python list
    if myType.value == 4:
        python_list = list(data_array.raw.decode('utf-8').replace('\x00', '__SPLITHERE__').split('__SPLITHERE__')[:])
        while "" in python_list:
            python_list.remove("")
    else:
        python_list = list(data_array)
    # Access the returned values
    # print(f"Data array: {python_list}")
    # print(f"myType: {myType.value}")
    # print(f"mySize: {mySize.value}")

    return python_list


def pointer_write(f: callable, param: int, arg: List, myType: int):
    """
    Writes a list to a COM variant pointer.
    """

    mode = param
    f.argtypes = [
        ctypes.c_long,
        ctypes.POINTER(ctypes.c_void_p),
        ctypes.POINTER(ctypes.c_long),
        ctypes.POINTER(ctypes.c_long)
    ]

    f.restype = None

    if myType == 1:
        c_type = ctypes.c_int32
    elif myType == 2:
        c_type = ctypes.c_double
    elif myType == 3:
        c_type = ctypes.c_double
    elif myType == 4:
        c_type = ctypes.c_char

    # Prepare input values
    if myType == 4:  # Handle string or characters
        data = [s.encode('utf-8') + b'\x00' for s in arg]  # Encode each string and null-terminate
        c_array = ctypes.create_string_buffer(b''.join(data))  # Concatenate into a single byte buffer
        my_size = ctypes.c_long(len(c_array))  # Length of the total byte buffer
    else:
        data = arg
        c_array = (c_type * len(data))(*data)
        my_size = ctypes.c_long(len(data))

    my_type = ctypes.c_long(myType)
    pc_array = ctypes.cast(c_array, ctypes.c_void_p)
    f(mode, ctypes.byref(pc_array), ctypes.byref(my_type), ctypes.byref(my_size))
