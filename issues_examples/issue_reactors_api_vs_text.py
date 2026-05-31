# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

from pathlib import Path

from py_dss_interface import DSS

DSS_MODEL = """
ClearAll
New Circuit.Thevenin bus1=SourceBus pu=1.0 basekv=13.8 model=ideal
New Line.Lb phases=3 bus1=SourceBus bus2=LoadBus length=0.001 units=mi r1=0.01 x1=0.01 r0=0.01 x0=0.01 c1=0 c0=0
New Reactor.r bus1=SourceBus bus2=LoadBus phases=3 kv=4.16 kvar=600
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.text("new xycurve.curve npts=1 x=[1] y=[1]")
    dss.reactors.l_curve = "curve"
    dss.reactors.r_curve = "curve"
    dss.reactors.name = "r"

    print("=== MISMATCH: kV / kvar via API vs text query ===")
    dss.reactors.kv = 10
    dss.reactors.kvar = 1000
    print("  API kv:", dss.reactors.kv, "  ? kV:", dss.text("? Reactor.r.kV").strip())
    print("  API kvar:", dss.reactors.kvar, "  ? kvar:", dss.text("? Reactor.r.kvar").strip())

    print()
    print("=== MISMATCH: Rp via API vs text query ===")
    dss.reactors.rp = 10.0
    print("  API rp:", dss.reactors.rp, "  ? Rp:", dss.text("? Reactor.r.Rp").strip())

    print()
    print("=== MISMATCH: LCurve / RCurve via API vs text query ===")
    print("  API l_curve:", dss.reactors.l_curve, "  ? LCurve:", repr(dss.text("? Reactor.r.LCurve").strip()))
    print("  API r_curve:", dss.reactors.r_curve, "  ? RCurve:", repr(dss.text("? Reactor.r.RCurve").strip()))

    dss.text("save circuit Dir=reactor")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text(
        "Edit Reactor.r kv=10 kvar=1000 Rp=5 LCurve=curve RCurve=curve"
    )
    print("  ? kV:", dss.text("? Reactor.r.kV").strip())
    print("  ? kvar:", dss.text("? Reactor.r.kvar").strip())
    print("  ? Rp:", dss.text("? Reactor.r.Rp").strip())
    print("  ? LCurve:", dss.text("? Reactor.r.LCurve").strip())
    print("  ? RCurve:", dss.text("? Reactor.r.RCurve").strip())


if __name__ == "__main__":
    main()
