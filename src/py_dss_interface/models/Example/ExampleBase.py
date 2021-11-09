# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 20/06/2021 at 15:02:12
 Project: py_dss_interface [jun, 2021]
"""
import json
import pathlib

from py_dss_interface import DSSDLL as DSS


class ExampleBase:
    def __init__(self, case="13"):
        # self.path = os.path.dirname(__file__)
        self.dss_file_path = ""
        self.dss_file_name = ""
        self.dss_file_full_path = ""
        self.dir_path = ""
        self.dss = DSS()

        if case is None:
            self.case = ""
        self.case = case
        self.load_json_(self.case, True)  # 13, 34, 37, 123, 8500+
        dss.text("compile {0}".format(self.dss_file_full_path))

    def load_json_(self, case_param: str, default_file: bool) -> None:
        self.dir_path = pathlib.Path(__file__).resolve().parents[4]
        self.dir_path = self.dir_path.joinpath('tests/py_dss_interface')
        config_path = self.dir_path.joinpath('configurations.json')
        with open(config_path) as json_f:
            data = json.load(json_f)
            for c in data['cases']:
                if c['case'] == case_param:
                    if default_file:
                        self.dss_file_name = c['dss_file']
                    else:
                        if not c['dss_file_alternative'] == '':
                            self.dss_file_name = c['dss_file_alternative']
                    self.dss_file_path = self.dir_path.joinpath("cases")
                    self.dss_file_path = self.dss_file_path.joinpath(c['case_folder'])
                    self.dss_file_full_path = self.dss_file_path.joinpath(self.dss_file_name)
                    break
