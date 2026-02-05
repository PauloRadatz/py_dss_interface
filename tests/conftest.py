# -*- coding: utf-8 -*-
"""
Top-level conftest.py for pytest option registration.
This ensures pytest_addoption hook is discovered early during option parsing.
"""

def pytest_addoption(parser):
    """
    Add custom command-line options for pytest.
    """
    parser.addoption(
        "--run-together",
        action="store_true",
        default=False,
        help="Run all tests together (even on Linux/C++). Overrides automatic individual execution."
    )
