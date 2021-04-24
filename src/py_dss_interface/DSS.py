# -*- coding: iso-8859-15 -*-
import ctypes
import os
import json
import pathlib

from . import Bus, Circuit, DSSInterface, Loads, LoadShapes, Solution, Text, Topology, Transformers, XYCurves
from .utils.System import System
# from py_dss_interface import Circuit, DSSInterface, Loads, LoadShapes, Solution, Topology, Transformers, XYCurves

DLL_NAME = "OpenDSSDirect.dll"


class DSS(Bus, Circuit, DSSInterface, Loads, LoadShapes, Solution, Text, Topology, Transformers, XYCurves):
    dll_folder: str
    dll_path: str
    dss_version: ctypes.c_char_p
    dss_obj: ctypes.cdll
    started = False
    memory_commands = []
    class_commands = []

    def __init__(self, dll_folder_param=None, dll_name=DLL_NAME):
        """
        Class to create an OpenDSS object
        :param dll_folder_param: None will use the OpenDSS available within the package. The dll path allows to use a
        different OpenDSS
        """
        if dll_folder_param is None:
            base_folder = os.path.dirname(os.path.abspath(__file__))
            self.dll_folder = os.path.join(pathlib.Path(base_folder), "dll")

        self.dll_path = System.get_architecture_path(self.dll_folder)
        os.chdir(self.dll_path)

        self.dll_file_path = os.path.join(self.dll_path, dll_name)

        self.dss_obj = ctypes.cdll.LoadLibrary(self.dll_file_path)
        self.started = True

        self.load_json()
        self._allocate_memory()

        if self.check_started():
            print("OpenDSS Started successfully! \nOpenDSS {}\n\n".format(self.dss_version.value.decode('ascii')))
        else:
            print("OpenDSS Failed to Start")

        # self.Circuit = Circuit(self.dss_obj)
        # self.DSSInterface = DSSInterface(self.dss_obj)
        # self.Loads = Loads(self.dss_obj)
        # self.LoadShapes = LoadShapes(self.dss_obj)
        # self.Text = Text(self.dss_obj)
        # self.Topology = Topology(self.dss_obj)
        # self.Transformers = Transformers(self.dss_obj)
        # self.Solution = Solution(self.dss_obj)
        # self.XYCurves = XYCurves(self.dss_obj)

    def check_started(self):
        if int(self.dss_obj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0))) == 1:
            self.dss_version = ctypes.c_char_p(self.dss_obj.DSSS(ctypes.c_int32(1), "".encode('ascii')))
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
