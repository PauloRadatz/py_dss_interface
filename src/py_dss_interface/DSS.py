# -*- coding: iso-8859-15 -*-
# TODO to work the docs

import _ctypes
import ctypes
import json
import os
import pathlib
import shutil
import tempfile
import uuid
import weakref

from . import ActiveClass, Bus, CapControls, Capacitors, Circuit, CktElement, CMathLib, CtrlQueue, DSSElement, Base
from . import DSSExecutive, DSSInterface, DSSProperties, ErrorOpenDSS, Fuses, Generators, ISources
from . import LineCodes, Lines, Loads, LoadShapes, Meters, Monitors, Parallel, Parser, PDElements, PVSystems, Reactors, \
    Reclosers
from . import Relays, RegControls, Sensors, Settings, Solution, Storages, SwtControls, Text, Topology, Transformers, \
    VSources
from . import XYCurves
from .utils.System import System
from .utils.Error import Error

DLL_NAME_WIN_DELPHI = "OpenDSSDirect.dll"
DLL_NAME_WIN_CPP = "OpenDSSC.dll"
DLL_NAME_LINUX = "libOpenDSSC.so"


class DSSDLL:

    def __init__(self, *args):
        Error.use_package_v1()
        raise RuntimeError("DSSDLL is not supported in this version of py-dss-interface. "
                           "Please check the package version or update your code accordingly.")


class DSS:

    def __init__(self, dll_folder_param=None, dll_by_user=None, print_dss_info=False, windows_version: str = "delphi", single_instance: bool = True):
        # TODO: dss_write_allowforms
        """
        Class to create an OpenDSS object
        :param dll_folder_param: None will use the OpenDSS available within the package. The dll path allows to use a
        different OpenDSS
        """
        self._my_dss_version = None
        self.started = False
        self.__memory_commands = []
        self.backend = None

        if System.detect_platform() == 'Linux':
            self.backend = "Linux-C++"

        else:
            if windows_version == "delphi":
                self.backend = "Windows-Delphi"
            elif windows_version == "cpp":
                self.backend = "Windows-C++"

        if dll_folder_param and dll_by_user:
            self._dll_path = dll_folder_param
            self.dll_file_path = os.path.join(dll_folder_param, dll_by_user)
        else:
            if System.detect_platform() == 'Linux':
                if not dll_folder_param:
                    dll_folder_param = os.path.join(pathlib.Path(os.path.dirname(os.path.abspath(__file__))),
                                                    "opendss_official", "linux", "cpp")
                dll_folder_param = pathlib.Path(dll_folder_param)
                dll_by_user = DLL_NAME_LINUX

                self._dll_path = dll_folder_param
            elif System.detect_platform() == 'Windows':
                if not dll_folder_param:
                    valid_versions = ["cpp", "delphi"]
                    if windows_version not in valid_versions:
                        raise ValueError(f"Invalid version '{windows_version}'. Valid options are {valid_versions}.")
                    dll_folder_param = os.path.join(pathlib.Path(os.path.dirname(os.path.abspath(__file__))),
                                                    "opendss_official", "windows", windows_version)
                dll_folder_param = pathlib.Path(dll_folder_param)
                if windows_version == "delphi":
                    dll_by_user = DLL_NAME_WIN_DELPHI
                if windows_version == "cpp":
                    dll_by_user = DLL_NAME_WIN_CPP

                self._dll_path = System.get_architecture_path(dll_folder_param)

            self.dll_file_path = os.path.join(self._dll_path, dll_by_user)

        # Load Library based on single_instance option
        if single_instance:
            _dll_dir_cookie = None
            if os.name == 'nt' and hasattr(os, 'add_dll_directory'):
                _dll_dir_cookie = os.add_dll_directory(str(self._dll_path))
            try:
                self._dss_obj = ctypes.cdll.LoadLibrary(str(self.dll_file_path))
            except Exception as e:
                raise e
            finally:
                if _dll_dir_cookie:
                    _dll_dir_cookie.close()
        else:
            # Create a unique temporary directory
            temp_dir = os.path.join(tempfile.gettempdir(), "py_dss_interface", f"instance_{uuid.uuid4().hex}")
            os.makedirs(temp_dir, exist_ok=True)
            
            # Copy all files from the DLL folder to the temp folder (so dependencies like KLUSolve are copied too)
            dll_dir = os.path.dirname(self.dll_file_path)
            shutil.copytree(dll_dir, temp_dir, dirs_exist_ok=True)
            
            # Load the DLL from the copied temp path
            copied_dll_path = os.path.join(temp_dir, os.path.basename(self.dll_file_path))
            
            _dll_dir_cookie = None
            if os.name == 'nt' and hasattr(os, 'add_dll_directory'):
                _dll_dir_cookie = os.add_dll_directory(str(temp_dir))
                
            try:
                self._dss_obj = ctypes.cdll.LoadLibrary(copied_dll_path)
            except Exception as e:
                raise e
            finally:
                if _dll_dir_cookie:
                    _dll_dir_cookie.close()
                
            # Register finalizer to unload the DLL and delete the temp directory
            self._finalizer = weakref.finalize(self, self._cleanup_temp_dir, self._dss_obj, temp_dir)

        self.started = bool(self._dss_obj)
        if self.started:
            self.__load_json()
            self.__allocate_memory()

            if self.__check_started():
                self._base = Base(self._dss_obj)

                self.active_class = ActiveClass(self._dss_obj)
                self.bus = Bus(self._dss_obj)
                self.capacitors = Capacitors(self._dss_obj)
                self.capcontrols = CapControls(self._dss_obj)
                self.circuit = Circuit(self._dss_obj)
                self.cktelement = CktElement(self._dss_obj)
                self.cmathlib = CMathLib(self._dss_obj)
                self.ctrlqueue = CtrlQueue(self._dss_obj)
                self.dsselement = DSSElement(self._dss_obj)
                self.dssexecutive = DSSExecutive(self._dss_obj)
                self.dssinterface = DSSInterface(self._dss_obj)
                # self.dssprogress = DSSProgress(self._dss_obj)
                self.dssproperties = DSSProperties(self._dss_obj)
                self.errorinterface = ErrorOpenDSS(self._dss_obj)
                self.fuses = Fuses(self._dss_obj)
                self.generators = Generators(self._dss_obj)
                self.isources = ISources(self._dss_obj)
                self.linecodes = LineCodes(self._dss_obj)
                self.lines = Lines(self._dss_obj)
                self.loads = Loads(self._dss_obj)
                self.loadshapes = LoadShapes(self._dss_obj)
                self.meters = Meters(self._dss_obj)
                self.monitors = Monitors(self._dss_obj)
                self.parallel = Parallel(self._dss_obj)
                self.parser = Parser(self._dss_obj)
                self.pdelements = PDElements(self._dss_obj)
                self.pvsystems = PVSystems(self._dss_obj)
                self.reactors = Reactors(self._dss_obj)
                self.reclosers = Reclosers(self._dss_obj)
                self.regcontrols = RegControls(self._dss_obj)
                self.relays = Relays(self._dss_obj)
                self.sensors = Sensors(self._dss_obj)
                self.settings = Settings(self._dss_obj)
                self.solution = Solution(self._dss_obj)
                self.storages = Storages(self._dss_obj)
                self.swtcontrols = SwtControls(self._dss_obj)
                self.text = Text(self._dss_obj).text
                self.topology = Topology(self._dss_obj)
                self.transformers = Transformers(self._dss_obj)
                self.vsources = VSources(self._dss_obj)
                self.xycurves = XYCurves(self._dss_obj)

                if print_dss_info:
                    backend_info = f" (Backend: {self.backend})"
                    print(f"OpenDSS Started successfully! \nOpenDSS {self._my_dss_version.value.decode('ascii')}{backend_info}\n\n")

            else:
                print("OpenDSS Failed to Start")
                exit()
        else:
            print("An error occur!")
            exit()

    def __check_started(self):
        if int(self._dss_obj.DSSI(ctypes.c_int32(3), ctypes.c_int32(0))) != 1:
            return False
        # TODO: Need refactor this call to use a method that already exists
        self._my_dss_version = ctypes.c_char_p(self._dss_obj.DSSS(ctypes.c_int32(1), "".encode('ascii')))
        return True

    def __load_json(self):
        dir_path = os.path.dirname(os.path.realpath(__file__))
        with open(f'{dir_path}/configurations.json') as json_f:
            data = json.load(json_f)
            for n in data['structured_data']:
                for t in n['types']:
                    if t == 'F':
                        ctype = 'c_double'
                    elif t == 'S':
                        ctype = 'c_char_p'
                    command_ = 'self._dss_obj.' + n['name'] + t + '.restype' + ' = ' + 'ctypes.' + ctype
                    self.__memory_commands.append(command_)

    def __allocate_memory(self):
        self._dss_obj.DSSPut_Command.restype = ctypes.c_char_p
        self._dss_obj.DSSProperties.restype = ctypes.c_char_p
        self._dss_obj.ErrorDesc.restype = ctypes.c_char_p
        self._dss_obj.ErrorCode.restype = ctypes.c_int32

        for i in self.__memory_commands:
            exec(i)

    @staticmethod
    def _cleanup_temp_dir(dss_obj, temp_dir):
        # 1. Unload the DLL from memory to release OS file locks
        try:
            import _ctypes
            import os
            if os.name == 'nt':
                _ctypes.FreeLibrary(dss_obj._handle)
            else:
                _ctypes.dlclose(dss_obj._handle)
        except Exception:
            pass

        # 2. Safely remove the temp directory
        try:
            import shutil
            shutil.rmtree(temp_dir, ignore_errors=True)
        except Exception:
            pass
