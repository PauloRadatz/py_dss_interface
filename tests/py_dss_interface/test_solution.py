# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:05 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_solution.py
# @Software : VSCode

import pytest


class TestSolution13Bus:

    @pytest.fixture(autouse=True)
    def _request(self, solve_snap_13bus):
        self.dss = solve_snap_13bus
        self.dss.solution_solve()

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_solution_solve(self):
        expected = 0
        actual = self.dss.solution_solve()
        assert expected == actual

    def test_solution_read_mode(self):
        expected = 0
        actual = self.dss.solution_read_mode()
        assert expected == actual

    def test_solution_write_mode(self):
        expected = 1
        self.dss.solution_write_mode(expected)
        actual = self.dss.solution_read_mode()
        assert expected == actual

    def test_solution_read_hour(self):
        expected = 0
        actual = self.dss.solution_read_hour()
        assert expected == actual

    def test_solution_write_hour(self):
        expected = 12
        self.dss.solution_write_hour(expected)
        actual = self.dss.solution_read_hour()
        assert expected == actual

    def test_solution_read_year(self):
        expected = 0
        actual = self.dss.solution_read_year()
        assert expected == actual

    def test_solution_write_year(self):
        expected = 2
        self.dss.solution_write_year(expected)
        actual = self.dss.solution_read_year()
        assert expected == actual

    def test_solution_iterations(self):
        expected = 2
        actual = self.dss.solution_iterations()
        assert expected == actual

    def test_solution_read_max_iterations(self):
        expected = 15
        actual = self.dss.solution_read_max_iterations()
        assert expected == actual

    def test_solution_write_max_iterations(self):
        expected = 20
        self.dss.solution_write_max_iterations(expected)
        actual = self.dss.solution_read_max_iterations()
        assert expected == actual

    def test_solution_read_number(self):
        expected = 100
        actual = self.dss.solution_read_number()
        assert expected == actual

    def test_solution_write_number(self):
        expected = 2
        self.dss.solution_write_number(expected)
        actual = self.dss.solution_read_number()
        assert expected == actual

    def test_solution_read_random(self):
        expected = 1
        actual = self.dss.solution_read_random()
        assert expected == actual

    def test_solution_write_random(self):
        expected = 2
        self.dss.solution_write_random(expected)
        actual = self.dss.solution_read_random()
        assert expected == actual

    def test_solution_read_load_model(self):
        expected = 1
        actual = self.dss.solution_read_load_model()
        assert expected == actual

    def test_solution_write_load_model(self):
        expected = 2
        self.dss.solution_write_load_model(expected)
        actual = self.dss.solution_read_load_model()
        assert expected == actual

    def test_solution_read_add_type(self):
        expected = 1
        actual = self.dss.solution_read_add_type()
        assert expected == actual

    def test_solution_write_add_type(self):
        expected = 2
        self.dss.solution_write_add_type(expected)
        actual = self.dss.solution_read_add_type()
        assert expected == actual

    def test_solution_read_algorithm(self):
        expected = 0
        actual = self.dss.solution_read_algorithm()
        assert expected == actual

    def test_solution_write_algorithm(self):
        expected = 2
        self.dss.solution_write_algorithm(expected)
        actual = self.dss.solution_read_algorithm()
        assert expected == actual

    def test_solution_read_control_mode(self):
        expected = 0
        actual = self.dss.solution_read_control_mode()
        assert expected == actual

    def test_solution_write_control_mode(self):
        expected = 1
        self.dss.solution_write_control_mode(expected)
        actual = self.dss.solution_read_control_mode()
        assert expected == actual

    def test_solution_read_control_iterations(self):
        expected = 1
        actual = self.dss.solution_read_control_iterations()
        assert expected == actual

    def test_solution_write_control_iterations(self):
        expected = 2
        self.dss.solution_write_control_iterations(expected)
        actual = self.dss.solution_read_control_iterations()
        assert expected == actual

    def test_solution_read_max_control_iterations(self):
        expected = 10
        actual = self.dss.solution_read_max_control_iterations()
        assert expected == actual

    def test_solution_write_max_control_iterations(self):
        expected = 2
        self.dss.solution_write_max_control_iterations(expected)
        actual = self.dss.solution_read_max_control_iterations()
        assert expected == actual

    def test_solution_sample_do_control_actions(self):
        expected = 0
        actual = self.dss.solution_sample_do_control_actions()
        assert expected == actual

    def test_solution_check_fault_status(self):
        expected = 0
        actual = self.dss.solution_check_fault_status()
        assert expected == actual

    def test_solution_solve_direct(self):
        expected = 0
        actual = self.dss.solution_solve_direct()
        assert expected == actual

    def test_solution_solve_power_flow(self):
        expected = 0
        actual = self.dss.solution_solve_power_flow()
        assert expected == actual

    def test_solution_solve_no_control(self):
        expected = 0
        actual = self.dss.solution_solve_no_control()
        assert expected == actual

    def test_solution_solve_plus_control(self):
        expected = 0
        actual = self.dss.solution_solve_plus_control()
        assert expected == actual

    def test_solution_init_snap(self):
        expected = 0
        actual = self.dss.solution_init_snap()
        assert expected == actual

    def test_solution_check_controls(self):
        expected = 0
        actual = self.dss.solution_check_controls()
        assert expected == actual

    def test_solution_sample_control_devices(self):
        expected = 0
        actual = self.dss.solution_sample_control_devices()
        assert expected == actual

    def test_solution_do_control_actions(self):
        expected = 0
        actual = self.dss.solution_do_control_actions()
        assert expected == actual

    def test_solution_build_y_matrix(self):
        expected = 0
        actual = self.dss.solution_build_y_matrix()
        assert expected == actual

    def test_solution_system_y_changed(self):
        expected = 0
        actual = self.dss.solution_system_y_changed()
        assert expected == actual

    def test_solution_read_converged(self):
        expected = 1
        actual = self.dss.solution_read_converged()
        assert expected == actual

    def test_solution_write_converged(self):
        expected = 1
        self.dss.solution_write_converged(expected)
        actual = self.dss.solution_read_converged()
        assert expected == actual

    def test_solution_total_iterations(self):
        expected = 2
        actual = self.dss.solution_total_iterations()
        assert expected == actual

    def test_solution_most_iterations_done(self):
        expected = 2
        actual = self.dss.solution_most_iterations_done()
        assert expected == actual

    def test_solution_read_control_actions_done(self):
        expected = 1
        actual = self.dss.solution_read_control_actions_done()
        assert expected == actual

    def test_solution_write_control_actions_done(self):
        expected = 1
        self.dss.solution_write_control_actions_done(expected)
        actual = self.dss.solution_read_control_actions_done()
        assert expected == actual

    def test_solution_finish_time_step(self):
        expected = 0
        actual = self.dss.solution_finish_time_step()
        assert expected == actual

    def test_solution_clean_up(self):
        expected = 0
        actual = self.dss.solution_clean_up()
        assert expected == actual

    def test_solution_solve_all(self):
        expected = 0
        actual = self.dss.solution_solve_all()
        assert expected == actual

    def test_solution_calc_inc_matrix(self):
        expected = 0
        actual = self.dss.solution_calc_inc_matrix()
        assert expected == actual

    def test_solution_calc_inc_matrix_0(self):
        expected = 0
        actual = self.dss.solution_calc_inc_matrix()
        assert expected == actual

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_solution_read_frequency(self):
        expected = 60
        actual = self.dss.solution_read_frequency()
        assert expected == actual

    def test_solution_write_frequency(self):
        expected = 50
        self.dss.solution_write_frequency(expected)
        actual = self.dss.solution_read_frequency()
        assert expected == actual

    def test_solution_read_seconds(self):
        expected = 0
        actual = self.dss.solution_read_seconds()
        assert expected == actual

    def test_solution_write_seconds(self):
        expected = 2.0
        self.dss.solution_write_seconds(expected)
        actual = self.dss.solution_read_seconds()
        assert expected == actual

    def test_solution_read_step_size(self):
        expected = 0.001
        actual = self.dss.solution_read_step_size()
        assert expected == actual

    def test_solution_write_step_size(self):
        expected = 1
        self.dss.solution_write_step_size(expected)
        actual = self.dss.solution_read_step_size()
        assert expected == actual

    def test_solution_read_load_mult(self):
        expected = 1.0
        actual = self.dss.solution_read_load_mult()
        assert expected == actual

    def test_solution_write_load_mult(self):
        expected = 2.0
        self.dss.solution_write_load_mult(expected)
        actual = self.dss.solution_read_load_mult()
        assert expected == actual

    def test_solution_read_tolerance(self):
        expected = 0.0001
        actual = self.dss.solution_read_tolerance()
        assert expected == actual

    def test_solution_write_tolerance(self):
        expected = 2.0
        self.dss.solution_write_tolerance(expected)
        actual = self.dss.solution_read_tolerance()
        assert expected == actual

    def test_solution_read_pct_growth(self):
        expected = 2.5
        actual = self.dss.solution_read_pct_growth()
        assert pytest.approx(expected) == pytest.approx(actual)

    def test_solution_write_pct_growth(self):
        expected = 2.0
        self.dss.solution_write_pct_growth(expected)
        actual = self.dss.solution_read_pct_growth()
        assert pytest.approx(expected) == pytest.approx(actual)

    def test_solution_read_gen_kw(self):
        expected = 1000
        actual = self.dss.solution_read_gen_kw()
        assert expected == actual

    def test_solution_write_gen_kw(self):
        expected = 100
        self.dss.solution_write_gen_kw(expected)
        actual = self.dss.solution_read_gen_kw()
        assert expected == actual

    def test_solution_read_gen_pf(self):
        expected = 1.0
        actual = self.dss.solution_read_gen_pf()
        assert expected == actual

    def test_solution_write_gen_pf(self):
        expected = 1.5
        self.dss.solution_write_gen_pf(expected)
        actual = self.dss.solution_read_gen_pf()
        assert expected == actual

    def test_solution_read_cap_kvar(self):
        expected = 600
        actual = self.dss.solution_read_cap_kvar()
        assert expected == actual

    def test_solution_write_cap_kvar(self):
        expected = 2.0
        self.dss.solution_write_cap_kvar(expected)
        actual = self.dss.solution_read_cap_kvar()
        assert expected == actual

    def test_solution_read_gen_mult(self):
        expected = 1.0
        actual = self.dss.solution_read_gen_mult()
        assert expected == actual

    def test_solution_write_gen_mult(self):
        expected = 2.0
        self.dss.solution_write_gen_mult(expected)
        actual = self.dss.solution_read_gen_mult()
        assert expected == actual

    def test_solution_read_dbl_hour(self):
        expected = 0
        actual = self.dss.solution_read_dbl_hour()
        assert expected == actual

    def test_solution_write_dbl_hour(self):
        expected = 1
        self.dss.solution_write_dbl_hour(expected)
        actual = self.dss.solution_read_dbl_hour()
        assert expected == actual

    def test_solution_step_size_min(self):
        expected = 0
        actual = self.dss.solution_step_size_min()
        assert expected == actual

    def test_solution_step_size_hr(self):
        expected = 0
        actual = self.dss.solution_step_size_hr()
        assert expected == 0

    def test_solution_process_time(self):
        expected = 0
        actual = self.dss.solution_process_time()
        assert actual > expected

    def test_solution_read_total_time(self):
        expected = 0
        actual = self.dss.solution_read_total_time()
        assert actual > expected

    def test_solution_write_total_time(self):
        expected = 2.0
        self.dss.solution_write_total_time(expected)
        actual = self.dss.solution_read_total_time()
        assert expected == actual

    def test_solution_process_time_step(self):
        expected = 0
        actual = self.dss.solution_process_time_step()
        assert expected == actual

    # ===================================================================
    # String methods
    # ===================================================================
    def test_solution_mode_id(self):
        expected = 'Snap'
        actual = self.dss.solution_mode_id()
        assert expected == actual

    def test_solution_read_ld_curve(self):
        expected = ''
        actual = self.dss.solution_read_ld_curve()
        assert expected == actual

    def test_solution_write_ld_curve(self):
        self.dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'test'
        self.dss.solution_write_ld_curve(expected)
        actual = self.dss.solution_read_ld_curve()
        assert expected == actual

    def test_solution_read_default_daily(self):
        expected = 'default'
        actual = self.dss.solution_read_default_daily()
        assert expected == actual

    def test_solution_write_default_daily(self):
        self.dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'test'
        self.dss.solution_write_default_daily(expected)
        actual = self.dss.solution_read_default_daily()
        assert expected == actual

    def test_solution_read_default_yearly(self):
        expected = 'default'
        actual = self.dss.solution_read_default_yearly()
        assert expected == actual

    def test_solution_write_default_yearly(self):
        self.dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                      "mult= "
                      "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                      "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                      "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                      "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'test'
        self.dss.solution_write_default_yearly(expected)
        actual = self.dss.solution_read_default_yearly()
        assert expected == actual

    # ===================================================================
    # Variant methods
    # ===================================================================
    def test_solution_event_log(self):
        expected = ['Hour=0, Sec=0, ControlIter=1, Element=Regulator.reg3, Action= CHANGED 7 TAPS TO 1.04375.',
                    'Hour=0, Sec=0, ControlIter=1, Element=Regulator.reg2, Action= CHANGED 5 TAPS TO 1.03125.',
                    'Hour=0, Sec=0, ControlIter=1, Element=Regulator.reg1, Action= CHANGED 7 TAPS TO 1.04375.',
                    'Hour=0, Sec=0, ControlIter=2, Element=Regulator.reg3, Action= CHANGED 2 TAPS TO 1.05625.',
                    'Hour=0, Sec=0, ControlIter=2, Element=Regulator.reg2, Action= CHANGED 1 TAPS TO 1.0375.',
                    'Hour=0, Sec=0, ControlIter=2, Element=Regulator.reg1, Action= CHANGED 2 TAPS TO 1.05625.']
        actual = self.dss.solution_event_log()
        assert expected == actual

    def test_solution_nc_matrix(self):
        expected = [0]
        actual = self.dss.solution_nc_matrix()
        assert expected == actual

    def test_solution_bus_levels(self):
        expected = []
        actual = self.dss.solution_bus_levels()
        assert expected == actual

    def test_solution_inc_matrix_rows(self):
        expected = []
        actual = self.dss.solution_inc_matrix_rows()
        assert expected == actual

    def test_solution_inc_matrix_cols(self):
        expected = ['sourcebus', '650', 'rg60', '633', '634',
                    '671', '645', '646', '692', '675', '611',
                    '652', '670', '632', '680', '684']
        actual = self.dss.solution_inc_matrix_cols()
        assert expected == actual

    def test_solution_laplacian(self):
        expected = [0]
        actual = self.dss.solution_laplacian()
        assert expected == actual
