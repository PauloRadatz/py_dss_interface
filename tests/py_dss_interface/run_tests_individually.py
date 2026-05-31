#!/usr/bin/env python
"""
Script to run each pytest test individually to identify which tests fail
when run together vs individually.
"""
import subprocess
import sys
import os
from pathlib import Path

def get_all_test_files():
    """Get all test files in the tests directory"""
    test_dir = Path(__file__).parent
    test_files = list(test_dir.glob("test_*.py"))
    return sorted(test_files)

def get_all_test_ids(test_file=None):
    """Use pytest to collect all test IDs from a file or all files"""
    import subprocess
    
    cmd = [
        sys.executable, "-m", "pytest",
        "--collect-only",
        "-v"
    ]
    
    if test_file:
        cmd.append(str(test_file))
    else:
        cmd.append(str(Path(__file__).parent))
    
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=30,
            cwd=Path(__file__).parent.parent.parent
        )
        
        # Parse pytest output to extract test IDs
        # pytest --collect-only -v outputs lines like:
        # <Module tests/py_dss_interface/test_bus.py>
        #   <Class TestBus13Bus>
        #     <Function test_num_nodes>
        # Or in some formats: tests/py_dss_interface/test_bus.py::TestBus13Bus::test_num_nodes
        test_ids = []
        current_module = None
        current_class = None
        
        for line in result.stdout.split('\n'):
            line = line.strip()
            if not line:
                continue
            
            # Check if line contains a full test path (format: path::Class::function)
            if '::' in line and 'test_' in line.split('::')[-1]:
                # This is a complete test ID
                test_ids.append(line)
            # Check if line contains a module
            elif line.startswith('<Module ') and line.endswith('>'):
                module_path = line[8:-1].strip()
                project_root = Path(__file__).parent.parent.parent
                test_dir_rel = Path(__file__).parent.relative_to(project_root)
                
                # Normalize the path - pytest might give relative or absolute paths
                if Path(module_path).is_absolute():
                    # Convert absolute path to relative from project root
                    try:
                        current_module = str(Path(module_path).relative_to(project_root))
                    except ValueError:
                        # If can't make relative, use as-is
                        current_module = module_path
                elif module_path.startswith('tests/'):
                    # Already relative to project root
                    current_module = module_path
                else:
                    # It's a relative path (could be just filename or relative to test dir)
                    # Always make it relative to project root
                    if '/' in module_path or '\\' in module_path:
                        # Has directory separators, might be relative to test dir
                        test_file_path = Path(__file__).parent / module_path
                        if test_file_path.exists():
                            current_module = str(test_file_path.relative_to(project_root))
                        else:
                            # Try as-is, might be relative to project root already
                            current_module = module_path if module_path.startswith('tests/') else str(test_dir_rel / Path(module_path).name)
                    else:
                        # Just a filename, prepend test directory
                        current_module = str(test_dir_rel / module_path)
                
                current_class = None  # Reset class when new module starts
            # Check if line contains a class
            elif line.startswith('<Class ') and line.endswith('>') and current_module:
                class_name = line[7:-1].strip()
                current_class = f"{current_module}::{class_name}"
            # Check if line contains a function
            elif line.startswith('<Function ') and line.endswith('>'):
                func_name = line[10:-1].strip()
                if 'test_' in func_name:
                    if current_class:
                        test_ids.append(f"{current_class}::{func_name}")
                    elif current_module:
                        test_ids.append(f"{current_module}::{func_name}")
        
        # Also try parsing stderr in case pytest outputs there
        if not test_ids and result.stderr:
            for line in result.stderr.split('\n'):
                line = line.strip()
                if '::' in line and 'test_' in line.split('::')[-1]:
                    test_ids.append(line)
        
        # Remove duplicates while preserving order and normalize paths
        seen = set()
        unique_test_ids = []
        project_root = Path(__file__).parent.parent.parent
        
        # normalize_test_id is defined later, but we need it here
        # So we'll do basic normalization inline, then full normalization in run_test
        for tid in test_ids:
            # Basic check: if path doesn't start with tests/, try to fix it
            if '::' in tid:
                path_part = tid.split('::')[0]
                if '/' not in path_part and '\\' not in path_part:
                    # Just a filename, prepend test directory
                    test_dir_rel = Path(__file__).parent.relative_to(project_root)
                    tid = str(test_dir_rel / path_part) + '::' + '::'.join(tid.split('::')[1:])
            
            if tid not in seen:
                seen.add(tid)
                unique_test_ids.append(tid)
        
        return unique_test_ids
    except Exception as e:
        print(f"Error collecting tests: {e}")
        if 'result' in locals():
            print(f"  stdout: {result.stdout[:500]}")
            print(f"  stderr: {result.stderr[:500]}")
        return []

def normalize_test_id(test_id, project_root):
    """Normalize a test ID to be relative to project root"""
    if '::' not in test_id:
        # Just a file path, no test spec
        path_part = test_id
        test_spec = None
    else:
        # Split into path and test spec
        parts = test_id.split('::', 1)
        path_part = parts[0]
        test_spec = parts[1]
    
    # Check if path_part is just a filename (no directory separators)
    if '/' not in path_part and '\\' not in path_part:
        # It's just a filename, prepend the test directory
        test_dir_rel = Path(__file__).parent.relative_to(project_root)
        path_part = str(test_dir_rel / path_part)
    elif not path_part.startswith('tests/'):
        # Path doesn't start with tests/, might be relative to current dir
        # Try to make it relative to project root
        test_file_path = Path(__file__).parent / path_part
        if test_file_path.exists():
            path_part = str(test_file_path.relative_to(project_root))
        else:
            # Try with tests/py_dss_interface/ prefix
            test_dir_rel = Path(__file__).parent.relative_to(project_root)
            path_part = str(test_dir_rel / Path(path_part).name)
    
    # Reconstruct test_id
    if test_spec:
        return f"{path_part}::{test_spec}"
    else:
        return path_part

def run_test(test_id, junit_xml=None):
    """Run a single test by its pytest test ID"""
    project_root = Path(__file__).parent.parent.parent
    
    # Normalize the test ID to ensure it's relative to project root
    normalized_test_id = normalize_test_id(test_id, project_root)
    
    cmd = [
        sys.executable, "-m", "pytest",
        normalized_test_id,
        "-v",
        "--tb=long"  # Show full traceback for better error details
    ]
    
    # Add JUnit XML output if requested (useful for CI/CD)
    if junit_xml:
        cmd.extend(["--junit-xml", junit_xml])
    
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=60,  # 1 minute timeout per test function
            cwd=project_root
        )
        return result.returncode == 0, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return False, "", "Test timed out after 1 minute"
    except Exception as e:
        return False, "", str(e)

def main():
    """Run all tests individually and report results"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Run pytest tests individually')
    parser.add_argument('--junit-xml-dir', type=str, default=None,
                       help='Directory to save JUnit XML reports (for CI/CD)')
    parser.add_argument('--test-file', type=str, default=None,
                       help='Run only a specific test file')
    parser.add_argument('--run-files', action='store_true', default=False,
                       help='Run entire test files instead of individual test functions')
    args = parser.parse_args()
    
    test_files = get_all_test_files()
    
    # Filter to specific test file if requested
    if args.test_file:
        test_files = [f for f in test_files if f.name == args.test_file]
        if not test_files:
            print(f"Error: Test file '{args.test_file}' not found")
            return False
    
    print(f"Found {len(test_files)} test files\n")
    print("=" * 80)
    
    # Create JUnit XML directory if requested
    junit_dir = None
    if args.junit_xml_dir:
        junit_dir = Path(args.junit_xml_dir)
        junit_dir.mkdir(parents=True, exist_ok=True)
    
    results = {
        'passed': [],
        'failed': [],
        'total': 0
    }
    
    if args.run_files:
        # Run entire test files (original behavior)
        for test_file in test_files:
            test_name = test_file.name
            print(f"\n{'=' * 80}")
            print(f"Testing file: {test_name}")
            print(f"{'=' * 80}")
            
            junit_xml = None
            if junit_dir:
                junit_xml = str(junit_dir / f"{test_name}.xml")
            
            test_path = str(test_file)
            print(f"  Running entire file: {test_name}")
            passed, stdout, stderr = run_test(test_path, junit_xml=junit_xml)
            results['total'] += 1
            
            if passed:
                print(f"  ✓ PASSED: {test_name}")
                results['passed'].append(test_name)
            else:
                print(f"  ✗ FAILED: {test_name}")
                results['failed'].append(test_name)
                print(f"\n  {'=' * 76}")
                print(f"  ERROR DETAILS for {test_name}")
                print(f"  {'=' * 76}")
                
                # Show full stderr (error output)
                if stderr:
                    print(f"\n  STDERR:")
                    print(f"  {'-' * 76}")
                    for line in stderr.split('\n'):
                        if line.strip():
                            print(f"  {line}")
                
                # Show full stdout (test output including traceback)
                if stdout:
                    print(f"\n  STDOUT:")
                    print(f"  {'-' * 76}")
                    for line in stdout.split('\n'):
                        if line.strip():
                            print(f"  {line}")
                
                print(f"  {'=' * 76}\n")
    else:
        # Run each test function individually using pytest's test collection
        print("Collecting all test IDs using pytest...")
        all_test_ids = get_all_test_ids()
        
        if not all_test_ids:
            print("  No tests found. Trying to collect from individual files...")
            for test_file in test_files:
                test_ids = get_all_test_ids(test_file)
                all_test_ids.extend(test_ids)
        
        # Filter to specific test file if requested
        if args.test_file:
            all_test_ids = [tid for tid in all_test_ids if args.test_file in tid]
        
        print(f"Found {len(all_test_ids)} test functions to run\n")
        
        for i, test_id in enumerate(all_test_ids, 1):
            # Extract a short name for display
            test_display = test_id.split('::')[-1] if '::' in test_id else test_id
            print(f"\n[{i}/{len(all_test_ids)}] Running: {test_id}")
            
            # Prepare JUnit XML path if requested
            junit_xml = None
            if junit_dir:
                safe_name = test_id.replace('::', '_').replace('/', '_').replace('\\', '_')
                junit_xml = str(junit_dir / f"{safe_name}.xml")
            
            passed, stdout, stderr = run_test(test_id, junit_xml=junit_xml)
            results['total'] += 1
            
            if passed:
                print(f"    ✓ PASSED: {test_display}")
                results['passed'].append(test_id)
            else:
                print(f"    ✗ FAILED: {test_display}")
                results['failed'].append(test_id)
                print(f"\n      {'=' * 76}")
                print(f"      ERROR DETAILS for {test_id}")
                print(f"      {'=' * 76}")
                
                # Show full stderr (error output)
                if stderr:
                    print(f"\n      STDERR:")
                    print(f"      {'-' * 76}")
                    for line in stderr.split('\n'):
                        if line.strip():
                            print(f"      {line}")
                
                # Show full stdout (test output including traceback)
                if stdout:
                    print(f"\n      STDOUT:")
                    print(f"      {'-' * 76}")
                    # Show the full output, but indent it
                    for line in stdout.split('\n'):
                        if line.strip():
                            print(f"      {line}")
                
                print(f"      {'=' * 76}\n")
    
    # Summary
    print(f"\n{'=' * 80}")
    print("SUMMARY")
    print(f"{'=' * 80}")
    if args.run_files:
        print(f"Total test files: {results['total']}")
    else:
        print(f"Total test functions: {results['total']}")
    print(f"Passed: {len(results['passed'])}")
    print(f"Failed: {len(results['failed'])}")
    
    if results['failed']:
        print(f"\nFailed tests ({len(results['failed'])}):")
        for test in results['failed'][:20]:  # Show first 20 failures
            print(f"  - {test}")
        if len(results['failed']) > 20:
            print(f"  ... and {len(results['failed']) - 20} more")
    
    if results['passed'] and len(results['passed']) <= 50:
        print(f"\nPassed tests ({len(results['passed'])}):")
        for test in results['passed']:
            print(f"  - {test}")
    elif results['passed']:
        print(f"\nPassed tests: {len(results['passed'])} (too many to list)")
    
    return len(results['failed']) == 0

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
