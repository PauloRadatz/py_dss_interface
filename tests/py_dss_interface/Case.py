# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 20/06/2021
"""
import os
import json
from py_dss_interface import DSS as DSS


class Case:
    def __init__(self, case="13"):
        self.path = os.path.dirname(__file__)
        self.dss_file = ""
        self.dss = DSS()
        self.actual = self.dss.started
        self.message = f"OpenDSSDirectDLL has been loaded: {self.actual}"

        if case is None:
            self.case = ""
        self.case = case
        self.load_json_(self.case, True)  # 13, 34, 37, 123, 8500+
        self.dss.text("compile {0}".format(self.dss_file))

    def load_json_(self, case_param: str, default_file: bool) -> None:
        with open(f'{os.path.dirname(__file__)}/configurations.json') as json_file:
            configurations = json.load(json_file)

            case_found = False
            for case in configurations['cases']:
                if case['case'] == case_param:
                    self.path = os.path.join(self.path, r"cases", case['case_folder'])
                    self.dss_file = case['dss_file' if default_file else 'dss_file_alternative']
                    self.dss_file = os.path.join(self.path, self.dss_file)
                    case_found = True
                    break

            if not case_found:
                raise ValueError(f"Case {case_param} not found in configurations.json")

