# -*- coding: utf-8 -*-
# @Time    : 6/4/2021 7:45 AM
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

import gc
import os
import pathlib
import subprocess
import sys
import time
import pytest

import py_dss_interface
from py_dss_interface.utils.System import System

script_path = os.path.dirname(os.path.abspath(__file__))

# Track test results for summary
_test_results = {
    'passed': [],
    'failed': [],
    'error': [],
    'skipped': []
}

# Track test progress for percentage display
_total_tests = 0
_current_test_number = 0


def _should_run_individually():
    """
    Determine if tests should run individually based on platform and backend.
    Returns True for Linux (always C++) or Windows with C++ backend.

    This prevents memory leak issues when running multiple C++ tests together.

    Command-line options:
    - --run-together: Override to run all tests together (even on Linux/C++)

    Environment variables:
    - PY_DSS_INTERFACE_CPP=true: Force C++ backend detection on Windows
    - PY_DSS_INTERFACE_RUN_TOGETHER=true: Override to run all tests together (even on Linux/C++)
    - _PY_DSS_INTERFACE_IN_SUBPROCESS=true: Internal flag to prevent recursion

    On Linux, tests automatically run individually since Linux always uses C++.
    On Windows, individual execution is triggered when C++ backend is detected.
    """
    # CRITICAL: Prevent recursion - if we're already in a subprocess, don't run individually again
    if os.environ.get('_PY_DSS_INTERFACE_IN_SUBPROCESS', '').lower() == 'true':
        return False

    # Check for override flag to run all tests together (command-line option)
    # We need to check sys.argv since pytest config might not be available at module level
    if '--run-together' in sys.argv:
        return False

    # Check for override flag to run all tests together (environment variable)
    if os.environ.get('PY_DSS_INTERFACE_RUN_TOGETHER', '').lower() == 'true':
        return False

    # Check if we're on Linux (always uses C++)
    # This should always return True on Linux systems
    detected_platform = System.detect_platform()
    if detected_platform == 'Linux':
        return True

    # Check if Windows C++ backend is being used
    # Check environment variable first (most reliable)
    if os.environ.get('PY_DSS_INTERFACE_CPP', '').lower() == 'true':
        return True

    # Check if C++ was detected during fixture initialization
    # This happens when solve_snap_13bus creates a C++ DSS instance
    if os.environ.get('_PY_DSS_INTERFACE_DETECTED_CPP', '').lower() == 'true':
        return True

    # Check command line arguments for --cpp flag
    if '--cpp' in sys.argv:
        return True

    # Default: run tests normally (not individually) on Windows with Delphi
    return False


# Store the flag at module level (will be checked/updated dynamically)
_RUN_INDIVIDUALLY = _should_run_individually()


@pytest.fixture(scope='function')
def solve_snap_13bus():
    """
    Fixture that creates a DSS instance for testing.

    To use C++ backend on Windows, uncomment the line below or set
    PY_DSS_INTERFACE_CPP=true environment variable.

    The backend choice automatically triggers individual test execution
    to prevent memory leaks with C++ version.
    """
    # dss = py_dss_interface.DSS("C:\OpenDSS_rep\Version8\Source")
    # Uncomment the line below to use C++ backend on Windows
    # This will automatically trigger individual test execution
    dss = py_dss_interface.DSS(windows_version="cpp")
    # dss = py_dss_interface.DSS()

    # Store backend info for detection
    # We'll check this in the hook to detect C++ usage
    if hasattr(dss, 'backend') and 'C++' in dss.backend:
        # Set environment variable so other parts of the code can detect it
        os.environ['_PY_DSS_INTERFACE_DETECTED_CPP'] = 'true'

    dss.text("Clear")
    dss.text("ClearAll")
    dss.text("set DefaultBaseFrequency=60")
    dss.text("Set EventLogDefault=yes")
    dss13_path = pathlib.Path(script_path).joinpath("cases", "13Bus", "IEEE13Nodeckt.dss")
    dss.text(f"compile [{dss13_path}]")

    dss.dssinterface.allow_forms = 0

    yield dss

    # Cleanup after test: Clear DSS state to help free memory
    # This is pytest-only cleanup, no changes to py-dss-interface code
    try:
        dss.text("Clear")
        dss.text("ClearAll")
    except Exception:
        # If cleanup fails, continue anyway
        pass

    # Force garbage collection to help free memory
    # This ensures Python releases references to DLL objects
    gc.collect()


def pytest_runtest_protocol(item, nextitem):
    """
    Pytest hook that runs each test individually when on Linux or C++ backend.
    This prevents memory leak issues when running multiple tests together.

    Returns:
        True: Test was handled (ran in subprocess), pytest will skip normal execution
        None: Let pytest handle the test normally
    """
    # Skip if we're in collection-only mode
    if hasattr(item.config.option, 'collectonly') and item.config.option.collectonly:
        return None

    # Check dynamically (in case fixture detected C++ backend)
    # Also check for --run-together option from pytest config
    should_run_individually = _should_run_individually()

    # Override with pytest option ONLY if explicitly set to True
    # The option exists by default (from tests/conftest.py) but defaults to False
    # IMPORTANT: Only check if the option was explicitly passed, not just if it exists
    # The option will always exist (defaults to False), so we check if it's True
    if hasattr(item.config.option, 'run_together'):
        run_together_flag = getattr(item.config.option, 'run_together', False)
        # Only override if explicitly True (not just truthy)
        if run_together_flag is True:
            should_run_individually = False
            # Debug: Uncomment to see when this override happens
            # print(f"DEBUG: Overriding individual execution due to --run-together flag")

    if not should_run_individually:
        # Let pytest handle tests normally
        return None

    # Run this test in a separate subprocess to isolate memory
    # Get the project root directory (two levels up from conftest.py)
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))

    # Get the test nodeid - pytest nodeids are relative to the rootdir
    # item.nodeid format: path/to/file.py::Class::test_method or path/to/file.py::test_function
    test_nodeid = item.nodeid

    # Fix the nodeid to be relative to project root
    # Get the actual file path and make it relative to project root
    test_file_path = str(item.fspath)
    if os.path.exists(test_file_path):
        rel_path = os.path.relpath(test_file_path, project_root).replace('\\', '/')

        # Extract test identifier (everything after the first ::)
        if '::' in test_nodeid:
            parts = test_nodeid.split('::', 1)
            test_part = parts[1]  # Class::test_method or test_function
            test_nodeid = f"{rel_path}::{test_part}"
        else:
            # No test identifier, just use the file path
            test_nodeid = rel_path

    # Build the pytest command to run just this test
    # Ensure pytest actually runs the test (not just collects)
    # CRITICAL: Set environment variable to prevent recursion in subprocess
    env = os.environ.copy()
    env['_PY_DSS_INTERFACE_IN_SUBPROCESS'] = 'true'

    cmd = [
        sys.executable,
        "-m", "pytest",
        test_nodeid,
        "-q",  # Quiet mode - less verbose output
        "--tb=short",
        "--no-header",  # Suppress session header
        "--no-summary"  # Suppress summary (we'll show results ourselves)
    ]

    # Preserve pytest options from command line (but avoid collection-only flags)
    # Note: We use -q for quiet mode, but if user wants verbose, we can add it
    if hasattr(item.config.option, 'verbose') and item.config.option.verbose:
        # Replace -q with -v if user wants verbose
        cmd = [c for c in cmd if c != "-q"]
        cmd.append("-v")
        # Remove --no-header and --no-summary for verbose mode
        cmd = [c for c in cmd if c not in ["--no-header", "--no-summary"]]
    if hasattr(item.config.option, 'tb') and item.config.option.tb:
        # Replace --tb=short with the option's tb value
        cmd = [c for c in cmd if not c.startswith('--tb=')]
        cmd.extend(["--tb", item.config.option.tb])

    # Run the test in a subprocess from project root
    # This isolates memory - when subprocess exits, all memory is freed
    # Each subprocess gets a completely fresh Python process and DLL state
    try:
        # Show which test is being run (before subprocess output)
        # Include file name and test identifier (e.g., "test_activeclass.py::TestActiveClass13Bus::test_first")
        # Always extract filename to ensure consistent display for ALL tests
        # Use item.fspath to get the actual file path as a reliable source
        test_file_path = str(item.fspath)
        # Extract just the filename from the full path
        if os.path.sep in test_file_path:
            filename = os.path.basename(test_file_path)
        else:
            # Fallback: try to extract from test_nodeid
            if '::' in test_nodeid:
                file_part = test_nodeid.split('::')[0]
            else:
                file_part = test_nodeid
            if '/' in file_part:
                filename = file_part.split('/')[-1]
            elif '\\' in file_part:
                filename = file_part.split('\\')[-1]
            else:
                filename = file_part

        # Extract test identifier (class::method or function name)
        if '::' in test_nodeid:
            parts = test_nodeid.split('::')
            if len(parts) >= 2:
                test_identifier = '::'.join(parts[1:])  # Everything after the first ::
                test_display_name = f"{filename}::{test_identifier}"
            else:
                test_display_name = filename
        else:
            # No test identifier in nodeid, try to get from item
            if hasattr(item, 'name'):
                test_display_name = f"{filename}::{item.name}"
            else:
                test_display_name = filename

        # Calculate and display percentage progress
        global _current_test_number, _total_tests
        _current_test_number += 1

        if _total_tests > 0:
            percentage = int((_current_test_number / _total_tests) * 100)
            # Always write the test name with filename prefix and percentage
            sys.stdout.write(f"{test_display_name} [{percentage}%] ")
        else:
            # Fallback if total not available yet
            sys.stdout.write(f"{test_display_name} ")
        sys.stdout.flush()

        # Capture output so we can display it properly
        # Use check=False so we can handle non-zero exit codes
        # Pass the environment with the recursion prevention flag
        # The subprocess will completely isolate memory - when it exits, all DLL memory is freed
        result = subprocess.run(
            cmd,
            cwd=project_root,  # Always run from project root
            env=env,  # Pass environment with recursion prevention flag
            capture_output=True,  # Capture to process and display
            text=True,
            timeout=300,  # 5 minute timeout per test
            check=False,  # Don't raise on non-zero exit
            # Ensure subprocess gets a clean environment
            # When subprocess exits, Python's garbage collector will clean up,
            # and the OS will free all memory allocated by the subprocess
            # This is the key: subprocess isolation = automatic memory cleanup
        )

        # Display the output (quiet mode already reduces verbosity)
        # Extract just the result (PASSED/FAILED) from output
        # Filter out pytest session headers and collection messages
        if result.stdout:
            output_lines = result.stdout.split('\n')
            test_result = None
            error_details = []

            for line in output_lines:
                # Skip session headers and collection messages
                if any(skip in line.lower() for skip in [
                    'test session starts',
                    'collecting',
                    'collected',
                    'platform',
                    'rootdir',
                    'configfile',
                    'plugins',
                    '====='
                ]):
                    continue

                # Look for test results
                if 'PASSED' in line:
                    test_result = 'PASSED'
                elif 'FAILED' in line:
                    test_result = 'FAILED'
                    # Capture failure details
                    if 'FAILED' in line or 'assert' in line.lower():
                        error_details.append(line.strip())
                elif 'ERROR' in line:
                    test_result = 'ERROR'
                    # Capture error details
                    if 'ERROR' in line or 'exception' in line.lower():
                        error_details.append(line.strip())
                elif line.strip() and test_result is None:
                    # Check if this looks like an error line
                    if any(keyword in line.lower() for keyword in ['error', 'exception', 'traceback']):
                        error_details.append(line.strip())

            # Display result
            if test_result:
                sys.stdout.write(f"{test_result}\n")
                # Track test result
                if test_result == 'PASSED':
                    _test_results['passed'].append(test_display_name)
                elif test_result == 'FAILED':
                    _test_results['failed'].append(test_display_name)
                elif test_result == 'ERROR':
                    _test_results['error'].append(test_display_name)
                # Show error details if any
                if error_details and test_result != 'PASSED':
                    for detail in error_details[:3]:  # Limit to first 3 lines
                        if detail:
                            sys.stderr.write(f"  {detail}\n")
            else:
                # If no result found but exit code is 0, assume passed
                if result.returncode == 0:
                    sys.stdout.write('.\n')
                    _test_results['passed'].append(test_display_name)
                else:
                    sys.stdout.write('ERROR\n')
                    _test_results['error'].append(test_display_name)
            sys.stdout.flush()
        else:
            # No output - check return code
            if result.returncode == 0:
                sys.stdout.write('.\n')
                _test_results['passed'].append(test_display_name)
            else:
                sys.stdout.write('ERROR\n')
                _test_results['error'].append(test_display_name)
            sys.stdout.flush()

        if result.stderr:
            # Only show stderr if it's not already captured in error_details
            stderr_lines = result.stderr.split('\n')
            for line in stderr_lines:
                if line.strip() and 'warning' not in line.lower():
                    sys.stderr.write(f"{line}\n")
            sys.stderr.flush()

        # Check if pytest actually ran the test (not just collected)
        test_executed = False
        if result.stdout:
            output_lower = result.stdout.lower()
            if 'passed' in output_lower or 'failed' in output_lower or 'error' in output_lower:
                test_executed = True

        # If test didn't execute, fall back to normal execution
        if not test_executed and result.returncode != 0:
            return None

        # Force garbage collection in parent process after subprocess exits
        # The subprocess memory is already freed by OS when subprocess exits
        gc.collect()

        # The subprocess already ran the test and printed results
        # We just need to return True to indicate we handled it
        # Pytest will see the subprocess output and understand the results
        # No need to manually report - the subprocess output is sufficient
        return True  # Indicate we handled it

    except subprocess.TimeoutExpired:
        print(f"\nERROR: Test {test_nodeid} timed out after 5 minutes")
        _test_results['error'].append(test_display_name if 'test_display_name' in locals() else test_nodeid)
        return True  # Still return True to indicate we handled it
    except Exception as e:
        print(f"\nERROR: Error running test {test_nodeid} individually: {e}")
        _test_results['error'].append(test_display_name if 'test_display_name' in locals() else test_nodeid)
        # Fall back to normal execution on error
        return None


# Track session start time for duration calculation
_session_start_time = None

def pytest_collection_modifyitems(config, items):
    """
    Pytest hook called after test collection.
    Store total number of tests for progress tracking.
    """
    global _total_tests
    _total_tests = len(items)

def pytest_sessionstart(session):
    """Track session start time and reset test counter."""
    global _session_start_time, _current_test_number
    _session_start_time = time.time()
    _current_test_number = 0  # Reset counter at session start

def pytest_terminal_summary(terminalreporter, exitstatus, config):
    """
    Pytest hook to modify terminal summary output.
    Suppresses "no tests ran" message when we have tracked results.
    """
    # Check if we have any tracked results
    total_tracked = sum(len(_test_results[key]) for key in _test_results)
    if total_tracked > 0:
        # Clear pytest's default summary sections to prevent "no tests ran" message
        # We'll show our own summary in pytest_sessionfinish instead
        terminalreporter.stats = {}


def pytest_sessionfinish(session, exitstatus):
    """
    Pytest hook called after all tests have been collected and run.
    Print a summary of test results.
    """
    # Check if we have any tracked results
    total_tracked = sum(len(_test_results[key]) for key in _test_results)
    if total_tracked == 0:
        # No tracked results, pytest will handle summary
        return

    # Calculate duration
    global _session_start_time
    if _session_start_time:
        duration = time.time() - _session_start_time
        minutes = int(duration // 60)
        seconds = int(duration % 60)
        duration_str = f"{minutes}:{seconds:02d}"
    else:
        duration_str = "unknown"

    total_passed = len(_test_results['passed'])
    total_failed = len(_test_results['failed'])
    total_error = len(_test_results['error'])
    total_skipped = len(_test_results['skipped'])
    total_tests = total_passed + total_failed + total_error + total_skipped

    # Print summary with timing (replaces "no tests ran" message)
    print(f"\n{'=' * 22} tests ran in {duration_str} {'=' * 22}")

    print("\n" + "=" * 80)
    print("TEST SUMMARY")
    print("=" * 80)

    print(f"\nTotal tests: {total_tests}")
    print(f"  ✅ Passed:  {total_passed}")
    print(f"  ❌ Failed:  {total_failed}")
    print(f"  ⚠️  Errors:  {total_error}")
    if total_skipped > 0:
        print(f"  ⏭️  Skipped: {total_skipped}")

    # Print failed tests
    if total_failed > 0:
        print(f"\n{'=' * 80}")
        print(f"FAILED TESTS ({total_failed}):")
        print("=" * 80)
        for test_name in _test_results['failed']:
            print(f"  ❌ {test_name}")

    # Print error tests
    if total_error > 0:
        print(f"\n{'=' * 80}")
        print(f"ERROR TESTS ({total_error}):")
        print("=" * 80)
        for test_name in _test_results['error']:
            print(f"  ⚠️  {test_name}")

    print("\n" + "=" * 80)
    if total_failed > 0 or total_error > 0:
        print(f"❌ {total_failed + total_error} TEST(S) FAILED")
        print("=" * 80)
