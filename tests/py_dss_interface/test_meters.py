# -*- coding: utf-8 -*-
# @Time    : 8/22/2021 08:20 PM
# @Author  : Rodolfo Londero
# @Email   : rodolfpl@gmail.com
# @File    : test_meters.py
# @Software: PyCharm


import pytest


class TestMeters13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("New energymeter.EM2 element=Line.670671")
        dss.text("solve")
        dss.meters.name = "em1"

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_meters_first(self, dss):
        expected = 1
        actual = dss.meters.first()
        assert actual == expected

    def test_meters_next(self, dss):
        expected = 2
        actual = dss.meters.next()
        assert actual == expected

    def test_meters_count(self, dss):
        expected = 2
        actual = dss.meters.count
        assert actual == expected

    def test_meters_count_branches(self, dss):
        expected = 6
        actual = dss.meters.count_branches
        assert actual == expected

    def test_meters_reset(self, dss):
        expected = 0
        actual = dss.meters.reset()
        assert actual == expected

    def test_meters_reset_all(self, dss):
        expected = 0
        actual = dss.meters.reset_all()
        assert actual == expected

    def test_meters_sample(self, dss):
        expected = 0
        actual = dss.meters.sample()
        assert actual == expected

    def test_meters_sample_all(self, dss):
        expected = 0
        actual = dss.meters.sample_all()
        assert actual == expected

    def test_meters_save(self, dss):
        expected = 0
        actual = dss.meters.save()
        assert actual == expected

    def test_meters_save_all(self, dss):
        expected = 0
        actual = dss.meters.save_all()
        assert actual == expected

    def test_meters_read_metered_terminal(self, dss):
        expected = 1
        actual = dss.meters.metered_terminal
        assert actual == expected

    def test_meters_write_metered_terminal(self, dss):
        expected = 2
        dss.meters.metered_terminal = expected
        actual = dss.meters.metered_terminal
        assert actual == expected

    def test_meters_open_all_di_files(self, dss):
        expected = 0
        actual = dss.meters.open_all_di_files()
        assert actual == expected

    def test_meters_close_all_di_files(self, dss):
        expected = 0
        actual = dss.meters.close_all_di_files()
        assert actual == expected

    def test_meters_read_sequence_index(self, dss):
        expected = 1
        actual = dss.meters.sequence_index
        assert actual == expected

    def test_meters_write_sequence_index(self, dss):
        expected = 2
        dss.meters.sequence_index = expected
        actual = dss.meters.sequence_index
        assert actual == expected

    def test_meters_do_reliability_calc(self, dss):
        dss.text("New Fuse.Fuse Line.650632 1 fusecurve=tlink  Ratedcurrent=10")
        expected = 0
        actual = dss.meters.do_reliability_calc()
        assert actual == expected

    def test_meters_seq_list_size(self, dss):
        expected = 6
        actual = dss.meters.seq_list_size
        assert actual == expected

    def test_meters_total_customers(self, dss):
        expected = 0
        actual = dss.meters.total_customers
        assert actual == expected

    def test_meters_num_sections(self, dss):
        expected = 0
        actual = dss.meters.num_sections
        assert actual == expected

    def test_meters_set_active_section(self, dss):
        expected = 0
        # TODO
        # actual = dss.meters.set_active_section(1)
        # assert actual == expected

    def test_meters_ocp_device_type(self, dss):
        expected = 0
        actual = dss.meters.ocp_device_type
        assert actual == expected

    def test_meters_num_section_branches(self, dss):
        expected = 0
        actual = dss.meters.num_section_branches
        assert actual == expected

    def test_meters_sect_seq_idx(self, dss):
        expected = 0
        actual = dss.meters.sect_seq_idx
        assert actual == expected

    def test_meters_sect_total_cust(self, dss):
        expected = 0
        actual = dss.meters.sect_total_cust
        assert actual == expected

    def test_meters_count_end_elements(self, dss):
        expected = 3
        actual = dss.meters.count_end_elements
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_meters_read_name(self, dss):
        expected = "em1"
        actual = dss.meters.name
        assert expected.lower() == actual.lower()

    def test_meters_write_name(self, dss):
        expected = "em2"
        dss.meters.name = expected
        actual = dss.meters.name
        assert expected.lower() == actual.lower()

    def test_meters_read_metered_element(self, dss):
        expected = 'line.650632'
        actual = dss.meters.metered_element
        assert actual == expected

    def test_meters_write_metered_element(self, dss):
        expected = 'line.684652'
        dss.meters.metered_element = expected
        actual = dss.meters.metered_element
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_meters_saifi(self, dss):
        expected = 0
        actual = dss.meters.saifi
        assert actual == expected

    def test_meters_saifi_kw(self, dss):
        expected = 0
        actual = dss.meters.saifi_kw
        assert actual == expected

    def test_meters_cust_interrupts(self, dss):
        expected = 0
        actual = dss.meters.cust_interrupts
        assert actual == expected

    def test_meters_avg_repair_time(self, dss):
        expected = 0
        actual = dss.meters.avg_repair_time
        assert actual == expected

    def test_meters_fault_rate_x_repair_hrs(self, dss):
        expected = 0
        actual = dss.meters.fault_rate_x_repair_hrs
        assert actual == expected

    def test_meters_sum_branch_flt_rates(self, dss):
        expected = 0
        actual = dss.meters.sum_branch_flt_rates
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_meters_all_names(self, dss):
        expected = ["em1", "em2"]
        actual = dss.meters.names
        assert actual == expected

    def test_meters_register_names(self, dss):
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
        actual = dss.meters.register_names
        assert actual == expected

    def test_meters_register_values(self, dss):
        dss.meters.metered_element = "Line.650632"

        expected = [0.0, 0.0, -1e+50, -1e+50, 0.0, 0.0, -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    -1e+50, -1e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

        actual = dss.meters.register_values
        assert actual == expected

    def test_meters_totals(self, dss):
        expected = [0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, -2e+50, -2e+50, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        actual = dss.meters.totals
        assert actual == expected

    def test_meters_read_peak_current(self, dss):
        expected = [400.0, 400.0, 400.0]
        actual = dss.meters.peak_current
        assert actual == expected

    def test_meters_write_peak_current(self, dss):
        expected = [550.0, 600.0, 680.0]
        dss.meters.peak_current = expected
        actual = dss.meters.peak_current
        assert actual == expected

    def test_meters_read_calc_current(self, dss):
        dss.text("Edit Energymeter.EM2 peakcurrent=[394, 301, 403]")
        dss.text("Allocateloads")
        expected = [473.76911764821904, 188.82002588596725, 424.90119440383563]
        actual = dss.meters.calc_current
        assert actual == expected

    # TODO: Ênio - https://github.com/PauloRadatz/py_dss_interface/issues/6
    # def test_meters_write_calc_current(self, dss):
    #     expected = [394, 301, 403]
    #     dss.meters.calc_current = expected
    #     dss.text("Allocateloads")
    #     actual = dss.meters.calc_current
    #     assert actual == expected

    def test_meters_read_alloc_factors(self, dss):
        dss.text("Edit Energymeter.EM2 peakcurrent=[394, 301, 403]")
        dss.text("Allocateloads")
        expected = [0.8316287096883996, 1.5941105748061957, 0.9484557946828922]
        actual = dss.meters.alloc_factors
        assert actual == expected

    # TODO it changed it
    def test_meters_write_alloc_factors(self, dss):
        # dss.text("Edit Energymeter.EM2 peakcurrent=[394, 301, 403]")
        # dss.text("Allocateloads")
        expected = [0.831627229659705, 1.5941085756581377, 0.9484539173579261]
        dss.meters.alloc_factors = expected
        actual = dss.meters.alloc_factors
        # assert actual == expected

    def test_meters_all_end_elements(self, dss):
        expected = ['Line.645646', 'Transformer.xfm1', 'Line.632670']
        actual = dss.meters.all_end_elements
        assert actual == expected

    def test_meters_all_pce_in_zone(self, dss):
        expected = ['Load.645',
                    'Load.646',
                    'Load.634a',
                    'Load.634b',
                    'Load.634c',
                    'Load.670a',
                    'Load.670b',
                    'Load.670c']
        actual = dss.meters.all_pce_in_zone
        assert actual == expected
