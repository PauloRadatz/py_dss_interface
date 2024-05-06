# -*- coding: utf-8 -*-
# @Time    : 4/12/2024 2:04 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : run.py
# @Software: PyCharm

import os
import subprocess

def run_pytest_on_files():
    # Directory containing your test files
    test_dir = '.'
    # List all Python test files in the directory
    test_files = [f for f in os.listdir(test_dir) if f.startswith('test') and f.endswith('.py')]

    for test_file in test_files:
        try:
            # Creating a unique CSV file name for each test file
            csv_file_name = f"{test_file[:-3]}_results.csv"
            # Construct the full path for the test file
            full_test_path = os.path.join(test_dir, test_file)
            # Construct the pytest command
            command = [
                'pytest',
                full_test_path,
                '--csv', csv_file_name,
                '-p', 'pytest_csv'  # Ensure the custom plugin is used
            ]
            # Run the pytest command
            subprocess.run(command, check=True)
        except:
            pass

if __name__ == '__main__':
    run_pytest_on_files()
