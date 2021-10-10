# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
import os
import platform


class System:
    @staticmethod
    def detect_platform():
        """
        Method to detect platform. Based on that result the methods can change
        :return: plat: A string contains the platform name like 'Windows', 'Linux'.
        """
        plat = platform.system()
        if plat == 0:
            return 'System cannot be determined'
        return plat

    @staticmethod
    def get_architecture_path(dll_folder):
        """
        Method to detect the architecture of the machine
        :param dll_folder: Folder that contains dll
        :return path: A string contains the path based on the architecture
        """
        path = "Nothing was decided about the architecture"
        if platform.architecture()[0] == "64bit":
            path = os.path.join(dll_folder, "x64")
            System.check_path_environment(path)
        elif platform.architecture()[0] == "32bit":
            path = os.path.join(dll_folder, "x86")
            System.check_path_environment(path)
        else:
            raise Exception("Make sure you are using the OpenDSS DLL and Python with the same bits")

        return path

    @staticmethod
    def check_path_environment(str_path):
        if str_path not in os.environ['PATH']:
            os.environ['PATH'] = str_path + os.pathsep + os.environ['PATH']
