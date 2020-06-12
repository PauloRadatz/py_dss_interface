import sys
import os
import pathlib


from py_dss_interface import DSSDLL



# Creates an OpenDSS object
dss = DSSDLL()
dss_file = r"C:\PauloRadatz\GitLab\opendsspy_workflow\Feeders\Creelman\Master_NoPV.dss"

dss.text(r"compile " + dss_file)

dss.text("show voltages")




