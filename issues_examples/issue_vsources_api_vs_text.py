# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

from pathlib import Path

from py_dss_interface import DSS

DSS_MODEL = """
ClearAll
New Circuit.Thevenin bus1=SourceBus pu=1.0 basekv=13.8 model=ideal
New Line.Lb phases=3 bus1=SourceBus bus2=LoadBus length=0.001 units=mi r1=0.01 x1=0.01 r0=0.01 x0=0.01 c1=0 c0=0
New Vsource.V1 bus1=LoadBus basekv=2.4 pu=1.0001 phases=3 angle=30 MVAsc3=1000 MVAsc1=2000
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.vsources.name = "V1"

    print("=== MISMATCH: phases / base_kv / pu / frequency via API vs text query ===")
    dss.vsources.phases = 2
    dss.vsources.base_kv = 13.8
    dss.vsources.pu = 1.1
    dss.vsources.frequency = 50.0
    print("  API phases:", dss.vsources.phases, "  ? phases:", dss.text("? Vsource.V1.phases").strip())
    print("  API base_kv:", dss.vsources.base_kv, "  ? basekv:", dss.text("? Vsource.V1.basekv").strip())
    print("  API pu:", dss.vsources.pu, "  ? pu:", dss.text("? Vsource.V1.pu").strip())
    print("  API frequency:", dss.vsources.frequency, "  ? frequency:", dss.text("? Vsource.V1.frequency").strip())

    dss.text("save circuit Dir=vsource")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit Vsource.V1 phases=2 basekv=13.8 pu=1.1 frequency=50")
    print("  ? phases:", dss.text("? Vsource.V1.phases").strip())
    print("  ? basekv:", dss.text("? Vsource.V1.basekv").strip())
    print("  ? pu:", dss.text("? Vsource.V1.pu").strip())
    print("  ? frequency:", dss.text("? Vsource.V1.frequency").strip())


if __name__ == "__main__":
    main()

