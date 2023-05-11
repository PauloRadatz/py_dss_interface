# -*- coding: utf-8 -*-
# @Time     : 09/09/2021 03:05 PM
# @Author   : Rodolfo Londero
# @Email    : rodolfopl@gmail.com
# @File     : test_solution.py
# @Software : VSCode

import pytest


class TestSolution13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        dss = solve_snap_13bus
        dss.text("batchedit regcontrol..* eventlog=True")
        dss.solution.solve()

        return dss

    # ===================================================================
    # Integer methods
    # ===================================================================
    def test_solution_read_mode(self, dss):
        expected = 0
        actual = dss.solution.mode
        assert actual == expected

    def test_solution_write_mode(self, dss):
        expected = 1
        dss.solution.mode = expected
        actual = dss.solution.mode
        assert actual == expected

    def test_solution_read_hour(self, dss):
        expected = 0
        actual = dss.solution.hour
        assert actual == expected

    def test_solution_write_hour(self, dss):
        expected = 12
        dss.solution.hour = expected
        actual = dss.solution.hour
        assert actual == expected

    def test_solution_read_year(self, dss):
        expected = 0
        actual = dss.solution.year
        assert actual == expected

    def test_solution_write_year(self, dss):
        expected = 2
        dss.solution.year = expected
        actual = dss.solution.year
        assert actual == expected

    def test_solution_iterations(self, dss):
        expected = 2
        actual = dss.solution.iterations
        assert actual == expected

    def test_solution_read_max_iterations(self, dss):
        expected = 15
        actual = dss.solution.max_iterations
        assert actual == expected

    def test_solution_write_max_iterations(self, dss):
        expected = 20
        dss.solution.max_iterations = expected
        actual = dss.solution.max_iterations
        assert actual == expected

    def test_solution_read_number(self, dss):
        expected = 100
        actual = dss.solution.number
        assert actual == expected

    def test_solution_write_number(self, dss):
        expected = 2
        dss.solution.number = expected
        actual = dss.solution.number
        assert actual == expected

    def test_solution_read_random(self, dss):
        expected = 1
        actual = dss.solution.random
        assert actual == expected

    def test_solution_write_random(self, dss):
        expected = 2
        dss.solution.random = expected
        actual = dss.solution.random
        assert actual == expected

    def test_solution_read_load_model(self, dss):
        expected = 1
        actual = dss.solution.load_model
        assert actual == expected

    def test_solution_write_load_model(self, dss):
        expected = 2
        dss.solution.load_model = expected
        actual = dss.solution.load_model
        assert actual == expected

    def test_solution_read_add_type(self, dss):
        expected = 1
        actual = dss.solution.add_type
        assert actual == expected

    def test_solution_write_add_type(self, dss):
        expected = 2
        dss.solution.add_type = expected
        actual = dss.solution.add_type
        assert actual == expected

    def test_solution_read_algorithm(self, dss):
        expected = 0
        actual = dss.solution.algorithm
        assert actual == expected

    def test_solution_write_algorithm(self, dss):
        expected = 2
        dss.solution.algorithm = expected
        actual = dss.solution.algorithm
        assert actual == expected

    def test_solution_read_control_mode(self, dss):
        expected = 0
        actual = dss.solution.control_mode
        assert actual == expected

    def test_solution_write_control_mode(self, dss):
        expected = 1
        dss.solution.control_mode = expected
        actual = dss.solution.control_mode
        assert actual == expected

    def test_solution_read_control_iterations(self, dss):
        expected = 1
        actual = dss.solution.control_iterations
        assert actual == expected

    def test_solution_write_control_iterations(self, dss):
        expected = 2
        dss.solution.control_iterations = expected
        actual = dss.solution.control_iterations
        assert actual == expected

    def test_solution_read_max_control_iterations(self, dss):
        expected = 10
        actual = dss.solution.max_control_iterations
        assert actual == expected

    def test_solution_write_max_control_iterations(self, dss):
        expected = 2
        dss.solution.max_control_iterations = expected
        actual = dss.solution.max_control_iterations
        assert actual == expected

    def test_solution_sample_do_control_actions(self, dss):
        expected = 0
        actual = dss.solution.sample_do_control_actions()
        assert actual == expected

    def test_solution_check_fault_status(self, dss):
        expected = 0
        actual = dss.solution.check_fault_status()
        assert actual == expected

    def test_solution_solve_direct(self, dss):
        expected = 0
        actual = dss.solution.solve_direct()
        assert actual == expected

    def test_solution_solve_power_flow(self, dss):
        expected = 0
        actual = dss.solution.solve_power_flow()
        assert actual == expected

    def test_solution_solve_no_control(self, dss):
        expected = 0
        actual = dss.solution.solve_no_control()
        assert actual == expected

    def test_solution_solve_plus_control(self, dss):
        expected = 0
        actual = dss.solution.solve_plus_control()
        assert actual == expected

    def test_solution_init_snap(self, dss):
        expected = 0
        actual = dss.solution.init_snap()
        assert actual == expected

    def test_solution_check_controls(self, dss):
        expected = 0
        actual = dss.solution.check_controls()
        assert actual == expected

    def test_solution_sample_control_devices(self, dss):
        expected = 0
        actual = dss.solution.sample_control_devices()
        assert actual == expected

    def test_solution_do_control_actions(self, dss):
        expected = 0
        actual = dss.solution.do_control_actions()
        assert actual == expected

    def test_solution_build_y_matrix(self, dss):
        expected = 0
        actual = dss.solution.build_y_matrix()
        assert actual == expected

    def test_solution_system_y_changed(self, dss):
        expected = 0
        actual = dss.solution.system_y_changed
        assert actual == expected

    def test_solution_read_converged(self, dss):
        expected = 1
        actual = dss.solution.converged
        assert actual == expected

    def test_solution_write_converged(self, dss):
        expected = 1
        dss.solution.converged = expected
        actual = dss.solution.converged
        assert actual == expected

    def test_solution_total_iterations(self, dss):
        expected = 2
        actual = dss.solution.total_iterations
        assert actual == expected

    def test_solution_most_iterations_done(self, dss):
        expected = 2
        actual = dss.solution.most_iterations_done
        assert actual == expected

    def test_solution_read_control_actions_done(self, dss):
        expected = 1
        actual = dss.solution.control_actions_done
        assert actual == expected

    def test_solution_write_control_actions_done(self, dss):
        expected = 1
        dss.solution.control_actions_done = expected
        actual = dss.solution.control_actions_done
        assert actual == expected

    def test_solution_finish_time_step(self, dss):
        expected = 0
        actual = dss.solution.finish_time_step()
        assert actual == expected

    def test_solution_clean_up(self, dss):
        expected = 0
        actual = dss.solution.clean_up()
        assert actual == expected

    def test_solution_solve_all(self, dss):
        expected = 0
        actual = dss.solution.solve_all()
        assert actual == expected

    def test_solution_calc_inc_matrix(self, dss):
        expected = 0
        actual = dss.solution.calc_inc_matrix()
        assert actual == expected

    def test_solution_calc_inc_matrix_0(self, dss):
        expected = 0
        actual = dss.solution.calc_inc_matrix()
        assert actual == expected

    # ===================================================================
    # Float methods
    # ===================================================================
    def test_solution_read_frequency(self, dss):
        expected = 60
        actual = dss.solution.frequency
        assert actual == expected

    def test_solution_write_frequency(self, dss):
        expected = 50
        dss.solution.frequency = expected
        actual = dss.solution.frequency
        assert actual == expected

    def test_solution_read_seconds(self, dss):
        expected = 0
        actual = dss.solution.seconds
        assert actual == expected

    def test_solution_write_seconds(self, dss):
        expected = 2.0
        dss.solution.seconds = expected
        actual = dss.solution.seconds
        assert actual == expected

    def test_solution_read_step_size(self, dss):
        expected = 0.001
        actual = dss.solution.step_size
        assert actual == expected

    def test_solution_write_step_size(self, dss):
        expected = 1
        dss.solution.step_size = expected
        actual = dss.solution.step_size
        assert actual == expected

    def test_solution_read_load_mult(self, dss):
        expected = 1.0
        actual = dss.solution.load_mult
        assert actual == expected

    def test_solution_write_load_mult(self, dss):
        expected = 2.0
        dss.solution.load_mult = expected
        actual = dss.solution.load_mult
        assert actual == expected

    def test_solution_read_tolerance(self, dss):
        expected = 0.0001
        actual = dss.solution.tolerance
        assert actual == expected

    def test_solution_write_tolerance(self, dss):
        expected = 2.0
        dss.solution.tolerance = expected
        actual = dss.solution.tolerance
        assert actual == expected

    def test_solution_read_pct_growth(self, dss):
        expected = 2.5
        actual = dss.solution.pct_growth
        assert pytest.approx(expected) == pytest.approx(actual)

    def test_solution_write_pct_growth(self, dss):
        expected = 2.0
        dss.solution.pct_growth = expected
        actual = dss.solution.pct_growth
        assert pytest.approx(expected) == pytest.approx(actual)

    def test_solution_read_gen_kw(self, dss):
        expected = 1000
        actual = dss.solution.gen_kw
        assert actual == expected

    def test_solution_write_gen_kw(self, dss):
        expected = 100
        dss.solution.gen_kw = expected
        actual = dss.solution.gen_kw
        assert actual == expected

    def test_solution_read_gen_pf(self, dss):
        expected = 1.0
        actual = dss.solution.gen_pf
        assert actual == expected

    def test_solution_write_gen_pf(self, dss):
        expected = 1.5
        dss.solution.gen_pf = expected
        actual = dss.solution.gen_pf
        assert actual == expected

    def test_solution_read_cap_kvar(self, dss):
        expected = 600
        actual = dss.solution.cap_kvar
        assert actual == expected

    def test_solution_write_cap_kvar(self, dss):
        expected = 2.0
        dss.solution.cap_kvar = expected
        actual = dss.solution.cap_kvar
        assert actual == expected

    def test_solution_read_gen_mult(self, dss):
        expected = 1.0
        actual = dss.solution.gen_mult
        assert actual == expected

    def test_solution_write_gen_mult(self, dss):
        expected = 2.0
        dss.solution.gen_mult = expected
        actual = dss.solution.gen_mult
        assert actual == expected

    def test_solution_read_dbl_hour(self, dss):
        expected = 0
        actual = dss.solution.dbl_hour
        assert actual == expected

    def test_solution_write_dbl_hour(self, dss):
        expected = 1
        dss.solution.dbl_hour = expected
        actual = dss.solution.dbl_hour
        assert actual == expected

    def test_solution_step_size_min(self, dss):
        expected = 0
        actual = dss.solution.step_size_min
        assert actual == expected

    def test_solution_step_size_hr(self, dss):
        expected = 0
        actual = dss.solution.step_size_hr
        assert expected == actual

    def test_solution_process_time(self, dss):
        expected = -1
        actual = dss.solution.process_time
        assert actual > expected

    def test_solution_read_total_time(self, dss):
        expected = 0
        actual = dss.solution.total_time
        assert actual > expected

    def test_solution_write_total_time(self, dss):
        expected = 2.0
        dss.solution.total_time = expected
        actual = dss.solution.total_time
        assert actual == expected

    def test_solution_process_time_step(self, dss):
        expected = 0
        actual = dss.solution.process_time_step
        assert actual == expected

    # ===================================================================
    # String methods
    # ===================================================================
    def test_solution_mode_id(self, dss):
        expected = 'Snap'
        actual = dss.solution.mode_id
        assert actual == expected

    def test_solution_read_ld_curve(self, dss):
        expected = ''
        actual = dss.solution.ld_curve
        assert actual == expected

    def test_solution_write_ld_curve(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'test'
        dss.solution.ld_curve = expected
        actual = dss.solution.ld_curve
        assert actual == expected

    def test_solution_read_default_daily(self, dss):
        expected = 'default'
        actual = dss.solution.default_daily
        assert actual == expected

    def test_solution_write_default_daily(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'test'
        dss.solution.default_daily = expected
        actual = dss.solution.default_daily
        assert actual == expected

    def test_solution_read_default_yearly(self, dss):
        expected = 'default'
        actual = dss.solution.default_yearly
        assert actual == expected

    def test_solution_write_default_yearly(self, dss):
        dss.text("New Loadshape.Test npts=24 interval=1 Pbase=100 Qbase=50 "
                 "mult= "
                 "(0.18000001 0.19000000 0.23999999 0.33000001 0.38999999 0.41000000 "
                 "0.64999998 1.23000002 1.88999999 1.88999999 1.96000004 1.98000002 "
                 "1.45000005 1.62000000 1.88999999 1.79999995 1.78999996 1.19000006 "
                 "0.80000001 0.66000003 0.51999998 0.40000001 0.28000000 0.23000000)")
        expected = 'test'
        dss.solution.default_yearly = expected
        actual = dss.solution.default_yearly
        assert actual == expected

    # ===================================================================
    # Variant methods
    # ===================================================================
    # def test_solution_event_log(self, dss):
    #     expected = ['Hour=0, Sec=0, ControlIter=1, Element=Regulator.reg3, Action= CHANGED 7 TAPS TO 1.04375.',
    #                 'Hour=0, Sec=0, ControlIter=1, Element=Regulator.reg2, Action= CHANGED 5 TAPS TO 1.03125.',
    #                 'Hour=0, Sec=0, ControlIter=1, Element=Regulator.reg1, Action= CHANGED 7 TAPS TO 1.04375.',
    #                 'Hour=0, Sec=0, ControlIter=2, Element=Regulator.reg3, Action= CHANGED 2 TAPS TO 1.05625.',
    #                 'Hour=0, Sec=0, ControlIter=2, Element=Regulator.reg2, Action= CHANGED 1 TAPS TO 1.0375.',
    #                 'Hour=0, Sec=0, ControlIter=2, Element=Regulator.reg1, Action= CHANGED 2 TAPS TO 1.05625.']
    #     actual = dss.solution.event_log
    #     # TODO
    #     # assert actual == expected

    def test_solution_nc_matrix(self, dss):
        expected = [0]
        actual = dss.solution.nc_matrix
        assert actual == expected

    def test_solution_bus_levels(self, dss):
        expected = []
        actual = dss.solution.bus_levels
        assert actual == expected

    def test_solution_inc_matrix_rows(self, dss):
        expected = []
        actual = dss.solution.inc_matrix_rows
        assert actual == expected

    def test_solution_inc_matrix_cols(self, dss):
        expected = ['sourcebus', '650', 'rg60', '633', '634',
                    '671', '645', '646', '692', '675', '611',
                    '652', '670', '632', '680', '684']
        actual = dss.solution.inc_matrix_cols
        assert actual == expected

    def test_solution_laplacian(self, dss):
        expected = [0]
        actual = dss.solution.laplacian
        assert actual == expected

    def test_solution_solve(self, dss):
        expected = 0
        actual = dss.solution.solve()
        assert actual == expected
