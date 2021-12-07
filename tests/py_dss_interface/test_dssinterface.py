# -*- coding: utf-8 -*-
# @Time    : 8/26/2021 09:40 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_dssinterface.py
# @Software: PyCharm


import pytest
import os
import pathlib


class TestDSSInterface13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_dss_num_circuits(self, dss):
        expected = 1
        actual = dss.dss_num_circuits()
        assert actual == expected

    def test_dss_clear_all(self, dss):
        expected = 0
        actual = dss.dss_clear_all()
        assert actual == expected

    # Todo we might not need this one here.
    # def test_dss_show_panel(self, dss):
    #     expected = 0
    #     actual = dss.dss_show_panel()
    #     assert actual == expected

    def test_dss_start(self, dss):
        expected = 1
        actual = dss.dss_start()
        assert actual == expected

    def test_dss_num_classes(self, dss):
        expected = 52
        actual = dss.dss_num_classes()
        assert actual == expected

    def test_dss_num_user_classes(self, dss):
        expected = 0
        actual = dss.dss_num_user_classes()
        assert actual == expected

    def test_dss_reset(self, dss):
        expected = 0
        actual = dss.dss_reset()
        assert actual == expected

    def test_dss_read_allow_forms(self, dss):
        expected = 1
        actual = dss.dss_read_allow_forms()
        assert actual == expected

    def test_dss_write_allow_forms(self, dss):
        expected = 1
        dss.dss_write_allow_forms(0)
        actual = dss.dss_read_allow_forms()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    # TODO check it later
    # def test_dss_new_circuit(self, dss):
    #     expected = 'New Circuit'
    #     actual = dss.dss_new_circuit('new_rest_circuit')
    #     assert actual == expected

    # def test_dss_version(self, dss):
    #     expected = 'Version 9.4.0.1 (64-bit build); License Status: Open '
    #     actual = dss.dss_version()
    #     assert actual == expected

    def test_dss_read_datapath(self, dss):
        expected = r"C:\\PauloRadatz\\GitHub\\py-dss-interface\\tests\\py_dss_interface\\cases\\13Bus\\"
        actual = dss.dss_read_datapath()
        assert actual.replace("\\", "").split("py-dss-interfacetests")[1] == expected.replace("\\", "").split("py-dss-interfacetests")[1]

    def test_dss_write_datapath(self, dss):
        data_path = str(pathlib.Path(os.path.dirname(__file__)).joinpath("cases", "13Bus", "datapath"))
        dss.dss_write_datapath(data_path)
        expected = data_path
        actual = dss.dss_read_datapath()
        assert actual.replace("\\", "").split("py-dss-interfacetests")[1] == expected.replace("\\", "").split("py-dss-interfacetests")[1]

    def test_dss_default_editor(self, dss):
        expected = 'Notepad.exe'
        actual = dss.dss_default_editor()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_dss_classes(self, dss):
        expected = ['Solution', 'LineCode', 'LoadShape', 'TShape', 'PriceShape', 'XYcurve', 'GrowthShape', 'TCC_Curve',
                    'Spectrum', 'WireData', 'CNData', 'TSData', 'LineGeometry', 'LineSpacing', 'XfmrCode', 'Line',
                    'Vsource', 'Isource', 'VCCS', 'Load', 'Transformer', 'RegControl', 'Capacitor', 'Reactor',
                    'CapControl', 'Fault', 'Generator',
                    'WindGen', 'GenDispatcher', 'Storage', 'StorageController', 'Relay', 'Recloser', 'Fuse',
                    'SwtControl', 'PVSystem', 'UPFC', 'UPFCControl', 'ESPVLControl', 'IndMach012', 'GICsource',
                    'AutoTrans', 'InvControl', 'ExpControl', 'GICLine', 'GICTransformer', 'VSConverter', 'Monitor',
                    'EnergyMeter', 'Sensor', 'FMonitor', 'Generic5']
        actual = dss.dss_classes()
        assert actual == expected

    def test_dss_user_classes(self, dss):
        expected = []
        actual = dss.dss_user_classes()
        assert actual == expected
