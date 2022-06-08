# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 20/06/2021
"""
import os
import json
from py_dss_interface import DSSDLL as DSS


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
        dir_path = os.path.dirname(os.path.realpath(__file__))
        with open(f'{dir_path}/configurations.json') as json_f:
            data = json.load(json_f)
            for c in data['cases']:
                if c['case'] == case_param:
                    if default_file:
                        self.dss_file = c['dss_file']
                    elif c['dss_file_alternative'] != '':
                        self.dss_file = c['dss_file_alternative']
                    self.path = os.path.join(self.path, r"cases")
                    self.path = os.path.join(self.path, c['case_folder'])
                    self.dss_file = os.path.join(self.path, self.dss_file)
                    break
