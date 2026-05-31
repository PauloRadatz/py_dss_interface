# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

from pathlib import Path

from py_dss_interface import DSS

DSS_MODEL = """
ClearAll
New Circuit.Thevenin bus1=SourceBus pu=1.0 basekv=13.8 model=ideal
New Line.L1 phases=3 bus1=SourceBus bus2=LoadBus1 length=0.01 units=mi r1=0.1 x1=0.1 r0=0.1 x0=0.1 c1=0.0 c0=0.0
New Line.L2 phases=3 bus1=LoadBus1 bus2=LoadBus2 length=0.01 units=mi r1=0.1 x1=0.1 r0=0.1 x0=0.1 c1=0.0 c0=0.0
New EnergyMeter.EM element=Line.L1 terminal=1
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.meters.name = "EM"

    print("=== MISMATCH: metered_terminal / metered_element via API vs text query ===")
    dss.meters.metered_terminal = 2
    dss.meters.metered_element = "line.l2"
    print("  API metered_terminal:", dss.meters.metered_terminal, "  ? terminal:", dss.text("? EnergyMeter.EM.terminal").strip())
    print("  API metered_element:", dss.meters.metered_element, "  ? element:", dss.text("? EnergyMeter.EM.element").strip())

    dss.text("save circuit Dir=meter")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit EnergyMeter.EM terminal=2 element=Line.L2")
    print("  ? terminal:", dss.text("? EnergyMeter.EM.terminal").strip())
    print("  ? element:", dss.text("? EnergyMeter.EM.element").strip())


if __name__ == "__main__":
    main()
