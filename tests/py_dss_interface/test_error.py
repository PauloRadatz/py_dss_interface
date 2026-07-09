# -*- coding: utf-8 -*-

import pytest

class TestError13Bus:

    @pytest.fixture(scope='function')
    def dss(self, solve_snap_13bus):
        return solve_snap_13bus

    def test_error_code(self, dss):
        assert dss.errorinterface.error_code == 0

    def test_error_desc(self, dss):
        assert dss.errorinterface.error_desc == ""

    def test_error_after_invalid_command(self, dss):
        dss.text("compile invalid_file_path_123.dss")
        assert dss.errorinterface.error_code != 0
        assert dss.errorinterface.error_desc != ""
