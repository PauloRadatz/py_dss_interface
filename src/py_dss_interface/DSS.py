# -*- coding: iso-8859-15 -*-
import ctypes
import json
import os
import pathlib

from . import ActiveClass, Bus, CapControls, Capacitors, Circuit, CktElement, CMathLib, CtrlQueue, DSSElement
from . import DSSExecutive, DSSInterface, DSSProgress, DSSProperties, ErrorOpenDSS, Fuses, Generators, ISources
from . import LineCodes, Lines, Loads, LoadShapes, Meters, Monitors, Parallel, Parser, PVSystems, Sensors, VSources
from . import Solution, Text, Topology, Transformers, XYCurves
from .utils.System import System

DLL_NAME_WIN = "OpenDSSDirect.dll"
DLL_NAME_LINUX = "libopendssdirect.so"


class DSSDLL(ActiveClass, Bus, CapControls, Capacitors, Circuit, CktElement, CMathLib, CtrlQueue, DSSElement,
             DSSExecutive, DSSInterface, DSSProgress, DSSProperties, ErrorOpenDSS, Fuses, Generators, Lines, Loads,
             ISources, LineCodes, LoadShapes, Meters, Monitors, Parallel, Parser, PVSystems, Sensors, Solution, Text,
             Topology, Transformers, VSources, XYCurves):
    dll_folder: str
    dll_path: str
    my_dss_version: ctypes.c_char_p
    dss_obj: ctypes.cdll
    started = False
    memory_commands = []
    class_commands = []

    # TODO need to be able to get different dll names:
    #  https://www.youtube.com/watch?v=74hCbYfdZdU&list=PLhdRxvt3nJ8x74v7XWcp6iLJL_nCOjxjK&index=9&t=2827s
    def __init__(self, dll_folder_param=None, dll_by_user=None):
        # TODO: dss_write_allowforms
        """
        Class to create an OpenDSS object
        :param dll_folder_param: None will use the OpenDSS available within the package. The dll path allows to use a
        different OpenDSS
        """
        self.started = False
        if dll_folder_param is not None and dll_by_user is not None:
            os.chdir(dll_folder_param)
            self.dss_obj = ctypes.cdll.LoadLibrary(os.path.join(dll_folder_param, dll_by_user))
            self.started = True
        elif dll_by_user is None:
            if dll_folder_param is None:
                dll_folder_param = os.path.join(pathlib.Path(os.path.dirname(os.path.abspath(__file__))), "dll")
            if System.detect_platform() == 'Linux':
                dll_folder_param = pathlib.Path(dll_folder_param)
                dll_by_user = DLL_NAME_LINUX
            elif System.detect_platform() == 'Windows':
                dll_folder_param = pathlib.Path(dll_folder_param)
                dll_by_user = DLL_NAME_WIN

            self.dll_path = System.get_architecture_path(dll_folder_param)
            self.dll_file_path = os.path.join(self.dll_path, dll_by_user)
            self.dss_obj = ctypes.cdll.LoadLibrary(self.dll_file_path)
            self.started = True
        # elif dll_folder_param is None and dll_by_user is not None:
        #     print("To specific a dll you MUST define the base folder")
        #     exit()
        elif dll_folder_param is not None and dll_by_user is None:
            print("Please specify a DLL in the defined folder.")
            exit()

        if self.started:
            self.load_json()
            self._allocate_memory()

            if self.check_started():
                print(
                    "OpenDSS Started successfully! \nOpenDSS {}\n\n".format(self.my_dss_version.value.decode('ascii')))
            else:
                print("OpenDSS Failed to Start")
                exit()
        else:
            print("An error occur!")
            exit()


    def check_started(self):
        if int(self.dss_obj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0))) == 1:
            # TODO: Need refactor this call to use a method that already exists
            self.my_dss_version = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(1), "".encode('ascii')))
            return True
        else:
            return False

    def load_json(self):
        dir_path = os.path.dirname(os.path.realpath(__file__))
        with open(dir_path + '/' + 'configurations.json') as json_f:
            data = json.load(json_f)
            for n in data['structured_data']:
                for t in n['types']:
                    if t == 'S':
                        ctype = 'c_char_p'
                    elif t == 'F':
                        ctype = 'c_double'
                    command_ = 'self.dss_obj.' + n['name'] + t + '.restype' + ' = ' + 'ctypes.' + ctype
                    self.memory_commands.append(command_)

    def _allocate_memory(self):
        self.dss_obj.DSSPut_Command.restype = ctypes.c_char_p
        self.dss_obj.DSSProperties.restype = ctypes.c_char_p

        for i in self.memory_commands:
            exec(i)
