# -*- encoding: utf-8 -*-
"""Frozen public-API surface for DSS().

Every public attribute that lives on a fresh DSS() instance is recorded in
EXPECTED below. The test re-derives the list from dir() at runtime and
asserts equality. A regression that drops or renames a wrapper on one
platform but not another will fail this test.

If a deliberate API change adds, removes, or renames a public attribute,
regenerate the golden list:

    python -c "import py_dss_interface; \\
               print(sorted(a for a in dir(py_dss_interface.DSS()) \\
                            if not a.startswith('_')))"

and replace EXPECTED.
"""
import py_dss_interface

EXPECTED = (
    "active_class",
    "backend",
    "bus",
    "capacitors",
    "capcontrols",
    "circuit",
    "cktelement",
    "cmathlib",
    "ctrlqueue",
    "dll_file_path",
    "dsselement",
    "dssexecutive",
    "dssinterface",
    "dssproperties",
    "errorinterface",
    "fuses",
    "generators",
    "isources",
    "linecodes",
    "lines",
    "loads",
    "loadshapes",
    "meters",
    "monitors",
    "parallel",
    "parser",
    "pdelements",
    "pvsystems",
    "reactors",
    "reclosers",
    "regcontrols",
    "relays",
    "sensors",
    "settings",
    "solution",
    "started",
    "storages",
    "swtcontrols",
    "text",
    "topology",
    "transformers",
    "vsources",
    "xycurves",
)


def test_dss_public_api_surface():
    dss = py_dss_interface.DSS()
    actual = tuple(sorted(a for a in dir(dss) if not a.startswith("_")))
    assert actual == EXPECTED
