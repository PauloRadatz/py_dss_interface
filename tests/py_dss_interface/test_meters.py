# -*- coding: utf-8 -*-
# @Time    : 8/22/2021 08:20 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_meters.py
# @Software: PyCharm


import pytest


class TestMeters13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.text("New energymeter.EM2 element=Line.670671")
        self.dss.text("solve")
        self.dss.meters_write_name("em1")

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_meters_first(self):
        expected = 1
        actual = self.dss.meters_first()
        assert actual == expected

    def test_meters_next(self):
        expected = 2
        actual = self.dss.meters_next()
        assert actual == expected

    def test_meters_count(self):
        expected = 2
        actual = self.dss.meters_count()
        assert actual == expected

    def test_meters_count_branches(self):
        expected = 6
        actual = self.dss.meters_count_branches()
        assert actual == expected

    def test_meters_reset(self):
        expected = 0
        actual = self.dss.meters_reset()
        assert actual == expected

    def test_meters_reset_all(self):
        expected = 0
        actual = self.dss.meters_reset_all()
        assert actual == expected

    def test_meters_sample(self):
        expected = 0
        actual = self.dss.meters_sample()
        assert actual == expected

    def test_meters_sample_all(self):
        expected = 0
        actual = self.dss.meters_sample_all()
        assert actual == expected

    def test_meters_save(self):
        expected = 0
        actual = self.dss.meters_save()
        assert actual == expected

    def test_meters_save_all(self):
        expected = 0
        actual = self.dss.meters_save_all()
        assert actual == expected

    def test_meters_read_metered_terminal(self):
        expected = 1
        actual = self.dss.meters_read_metered_terminal()
        assert actual == expected

    def test_meters_write_metered_terminal(self):
        expected = 2
        self.dss.meters_write_metered_terminal(expected)
        actual = self.dss.meters_read_metered_terminal()
        assert actual == expected

    def test_meters_open_all_di_files(self):
        expected = 0
        actual = self.dss.meters_open_all_di_files()
        assert actual == expected

    def test_meters_close_all_di_files(self):
        expected = 0
        actual = self.dss.meters_close_all_di_files()
        assert actual == expected

    def test_meters_read_sequence_index(self):
        expected = 1
        actual = self.dss.meters_read_sequence_index()
        assert actual == expected

    def test_meters_write_sequence_index(self):
        expected = 2
        self.dss.meters_write_sequence_index(expected)
        actual = self.dss.meters_read_sequence_index()
        assert actual == expected

    def test_meters_do_reliability_calc(self):
        self.dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        expected = 0
        actual = self.dss.meters_do_reliability_calc()
        assert actual == expected

    def test_meters_seq_list_size(self):
        expected = 6
        actual = self.dss.meters_seq_list_size()
        assert actual == expected

    def test_meters_total_customers(self):
        expected = 0
        actual = self.dss.meters_total_customers()
        assert actual == expected

    def test_meters_num_sections(self):
        expected = 0
        actual = self.dss.meters_num_sections()
        assert actual == expected

    def test_meters_set_active_section(self):
        expected = 0
        actual = self.dss.meters_set_active_section(1)
        assert actual == expected

    def test_meters_ocp_device_type(self):
        expected = 0
        actual = self.dss.meters_ocp_device_type()
        assert actual == expected

    def test_meters_num_section_branches(self):
        expected = 0
        actual = self.dss.meters_num_section_branches()
        assert actual == expected

    def test_meters_sect_seq_idx(self):
        expected = 0
        actual = self.dss.meters_sect_seq_idx()
        assert actual == expected

    def test_meters_sect_total_cust(self):
        expected = 0
        actual = self.dss.meters_sect_total_cust()
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_meters_read_name(self):
        expected = "em1"
        actual = self.dss.meters_read_name()
        assert expected.lower() == actual.lower()

    def test_meters_write_name(self):
        expected = "em2"
        self.dss.meters_write_name(expected)
        actual = self.dss.meters_read_name()
        assert expected.lower() == actual.lower()

    def test_meters_read_metered_element(self):
        expected = 'line.650632'
        actual = self.dss.meters_read_metered_element()
        assert actual == expected

    def test_meters_write_metered_element(self):
        expected = 'line.684652'
        self.dss.meters_write_metered_element(expected)
        actual = self.dss.meters_read_metered_element()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_meters_saifi(self):
        expected = 0
        actual = self.dss.meters_saifi()
        assert actual == expected

    def test_meters_saifi_kw(self):
        expected = 0
        actual = self.dss.meters_saifi_kw()
        assert actual == expected

    def test_meters_cust_interrupts(self):
        expected = 0
        actual = self.dss.meters_cust_interrupts()
        assert actual == expected

    def test_meters_avg_repair_time(self):
        expected = 0
        actual = self.dss.meters_avg_repair_time()
        assert actual == expected

    def test_meters_fault_rate_x_repair_hrs(self):
        expected = 0
        actual = self.dss.meters_fault_rate_x_repair_hrs()
        assert actual == expected

    def test_meters_sum_branch_flt_rates(self):
        expected = 0
        actual = self.dss.meters_sum_branch_flt_rates()
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_meters_all_names(self):
        expected = ["em1", "em2"]
        actual = self.dss.meters_all_names()
        assert actual == expected

    def test_meters_register_names(self):
        expected = ['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Zone kWh', 'Zone kvarh', 'Zone Max kW', 'Zone Max kVA',
                    'Overload kWh Normal', 'Overload kWh Emerg', 'Load EEN', 'Load UE', 'Zone Losses kWh',
                    'Zone Losses kvarh', 'Zone Max kW Losses', 'Zone Max kvar Losses', 'Load Losses kWh',
                    'Load Losses kvarh', 'No Load Losses kWh', 'No Load Losses kvarh', 'Max kW Load Losses',
                    'Max kW No Load Losses', 'Line Losses', 'Transformer Losses', 'Line Mode Line Losses',
                    'Zero Mode Line Losses', '3-phase Line Losses', '1- and 2-phase Line Losses', 'Gen kWh',
                    'Gen kvarh', 'Gen Max kW', 'Gen Max kVA', '4.16 kV Losses', 'Aux1', 'Aux6', 'Aux11', 'Aux16',
                    'Aux21', 'Aux26', '4.16 kV Line Loss', 'Aux2', 'Aux7', 'Aux12', 'Aux17', 'Aux22', 'Aux27',
                    '4.16 kV Load Loss', 'Aux3', 'Aux8', 'Aux13', 'Aux18', 'Aux23', 'Aux28', '4.16 kV No Load Loss',
                    'Aux4', 'Aux9', 'Aux14', 'Aux19', 'Aux24', 'Aux29', '4.16 kV Load Energy', 'Aux5', 'Aux10', 'Aux15',
                    'Aux20', 'Aux25', 'Aux30']
        actual = self.dss.meters_register_names()
        assert actual == expected

    def test_meters_register_values(self):
        self.dss.meters_write_metered_element("Line.650632")

        expected = [0.0, 0.0, -1e+50, -1e+50, 0.0, 0.0, -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

        actual = self.dss.meters_register_values()
        assert actual == expected

    def test_meters_totals(self):
        expected = [0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = self.dss.meters_totals()
        assert actual == expected

    def test_meters_read_peak_current(self):
        expected = [400.0, 400.0, 400.0]
        actual = self.dss.meters_read_peak_current()
        assert actual == expected

    def test_meters_write_peak_current(self):
        expected = [550.0, 600.0, 680.0]
        expected_str = '(' + str(expected[0]) + ',' + str(expected[1]) + ',' + str(expected[2]) + ')'
        self.dss.meters_write_peak_current(expected_str)
        actual = self.dss.meters_read_peak_current()
        assert actual == expected

    def test_meters_read_calc_current(self):
        self.dss.text("Edit Energymeter.EM2 peakcurrent=[394, 301, 403]")
        self.dss.text("Allocateloads")
        expected = [473.76911764821904, 188.82002588596725, 424.90119440383563]
        actual = self.dss.meters_read_calc_current()
        assert actual == expected

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/6
    # def test_meters_write_calc_current(self):
    #     expected = [394, 301, 403]
    #     self.dss.meters_write_calc_current(expected)
    #     self.dss.text("Allocateloads")
    #     actual = self.dss.meters_read_calc_current()
    #     assert actual == expected

    def test_meters_read_alloc_factors(self):
        self.dss.text("Edit Energymeter.EM2 peakcurrent=[394, 301, 403]")
        self.dss.text("Allocateloads")
        expected = [0.8316287096883996, 1.5941105748061957, 0.9484557946828922]
        actual = self.dss.meters_read_alloc_factors()
        assert actual == expected

    # TODO it changed it
    def test_meters_write_alloc_factors(self):
        # self.dss.text("Edit Energymeter.EM2 peakcurrent=[394, 301, 403]")
        # self.dss.text("Allocateloads")
        expected = [0.831627229659705, 1.5941085756581377, 0.9484539173579261]
        self.dss.meters_write_alloc_factors(expected)
        actual = self.dss.meters_read_alloc_factors()
        # assert actual == expected

    def test_meters_all_end_elements(self):
        expected = ['Line.645646', 'Transformer.xfm1', 'Line.632670']
        actual = self.dss.meters_all_end_elements()
        assert actual == expected
