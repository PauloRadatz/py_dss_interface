import pytest

import os
import pathlib
import py_dss_interface

script_path = os.path.dirname(os.path.abspath(__file__))
ddll_path = os.path.join(pathlib.Path(script_path), "DDLL")
dss13_path = os.path.join(pathlib.Path(script_path), "13Bus", "IEEE13Nodeckt.dss")

@pytest.fixture
def DSS():

    dss = py_dss_interface.DSSDLL(ddll_path)
    actual = dss.opendss_started
    expected = True

    message = ("OpenDSSDirectDLL has been loaded: {}".format(actual))

    assert actual is expected, message

    return dss

class TestDSS(object):

    @pytest.fixture(autouse=True)
    def _request_dss(self, DSS):
        self.dss = DSS

    def test_DSS(self):

        actual = self.dss.opendss_started
        expected = True

        message = ("OpenDSSDirectDLL has been loaded: {}".format(actual))

        assert actual is expected, message

    def test_DSS_version(self):
        assert self.dss.dss_version is not None


    def test_solution_totaliterations(self):
        self.dss.text("compile " + dss13_path)

        assert self.dss.solution_totaliterations() == 11
