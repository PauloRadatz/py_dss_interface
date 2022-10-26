# -*- coding: utf-8 -*-
# @Time    : 11/15/2021 10:00 AM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : line_example.py
# @Software: PyCharm

import py_dss_interface
import pathlib

dss = py_dss_interface.DSSDLL("C:\OpenDSS_rep\Version8\Source")
dss_file = r"C:\Users\ppra005\Box\Documents_PC\Projects\2021\OpenDSSTestFramework\Elements\Line\impedances.dss"

dss.text(f"compile [{dss_file}]")
dss.lines_write_name("line")

print("--------------------TEXT--------------------\n\n")
print(f"Original R1 {dss.text('? line.line.r1')}")
print(f"Original R0 {dss.text('? line.line.r0')}")
print(f"Original X1 {dss.text('? line.line.x1')}")
print(f"Original X1 {dss.text('? line.line.x0')}")

dss.solution.solve

y_original_text = dss.circuit_system_y()

dss.text("edit line.line r1=2 r0=4 x1=6 x0=8")

print(f"New R1 {dss.text('? line.line.r1')}")
print(f"New R0 {dss.text('? line.line.r0')}")
print(f"New X1 {dss.text('? line.line.x1')}")
print(f"New X1 {dss.text('? line.line.x0')}")

dss.solution.solve

y_new_text = dss.circuit_system_y()


print("\n\n--------------------DLL--------------------\n\n")
dss.text(f"compile [{dss_file}]")
dss.lines_write_name("line")

print(f"Original R1 {dss.lines_read_r1()}")
print(f"Original R0 {dss.lines_read_r0()}")
print(f"Original X1 {dss.lines_read_x1()}")
print(f"Original X1 {dss.lines_read_x0()}")

dss.solution.solve

y_original_dll = dss.circuit_system_y()

dss.text("edit line.line r1=2 r0=4 x1=6 x0=8")

print(f"New R1 {dss.lines_read_r1()}")
print(f"New R0 {dss.lines_read_r0()}")
print(f"New X1 {dss.lines_read_x1()}")
print(f"New X1 {dss.lines_read_x0()}")

dss.solution.solve

y_new_dll = dss.circuit_system_y()


print("\n\n--------------------Comparison--------------------\n\n")

print(f"y_original_text == y_original_dll {y_original_text == y_original_dll}")
print(f"y_new_text == y_new_dll {y_new_text == y_new_dll}")
