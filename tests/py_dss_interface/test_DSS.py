import pytest

import os
import pathlib
from py_dss_interface import DSSDLL

script_path = os.path.dirname(os.path.abspath(__file__))
ddll_path = os.path.join(pathlib.Path(script_path).parent.parent, "DDLL")

@pytest.fixture
def DSS():

    dss = DSSDLL(ddll_path)
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



