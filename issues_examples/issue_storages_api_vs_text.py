# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

from pathlib import Path

from py_dss_interface import DSS

DSS_MODEL = """
ClearAll
New Circuit.Thevenin bus1=SourceBus pu=1.0 basekv=13.8 model=ideal
New Storage.St phases=3 bus1=SourceBus kv=13.8 pf=0.98 kWrated=100 kva=10000 kWhrated=1000 %stored=70 model=1 state=idling
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.storages.name = "St"

    print("=== MISMATCH: kW / kvar via API vs text query ===")
    dss.storages.kvar = 100.0
    print("  API kvar:", dss.storages.kvar, "  ? kvar:", dss.text("? Storage.St.kvar").strip())
    dss.storages.kw = 1200.0
    print("  API kW:", dss.storages.kw, "  ? kW:", dss.text("? Storage.St.kW").strip())


    print()
    print("=== MISMATCH: AmpLimit / AmpLimitGain via API vs text query ===")
    dss.storages.amp_limit = 10
    dss.storages.amp_limit_gain = 10
    print("  API amp_limit:", dss.storages.amp_limit, "  ? AmpLimit:", repr(dss.text("? Storage.St.AmpLimit").strip()))
    print("  API amp_limit_gain:", dss.storages.amp_limit_gain, "  ? AmpLimitGain:", repr(dss.text("? Storage.St.AmpLimitGain").strip()))

    dss.text("save circuit Dir=storage")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit Storage.St kvar=100")
    print("  ? kvar:", dss.text("? Storage.St.kvar").strip())
    dss.text("Edit Storage.St kW=1200")
    print("  ? kW:", dss.text("? Storage.St.kW").strip())
    dss.text("Edit Storage.St AmpLimit=10 AmpLimitGain=10")
    print("  ? AmpLimit:", repr(dss.text("? Storage.St.AmpLimit").strip()))
    print("  ? AmpLimitGain:", repr(dss.text("? Storage.St.AmpLimitGain").strip()))


if __name__ == "__main__":
    main()
