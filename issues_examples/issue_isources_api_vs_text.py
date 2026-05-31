# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

from pathlib import Path

from py_dss_interface import DSS

DSS_MODEL = """
ClearAll
New Circuit.Thevenin bus1=SourceBus pu=1.0 basekv=13.8 model=ideal
New ISource.ISrc bus1=SourceBus amps=100 angle=30 frequency=60
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.isources.name = "ISrc"

    print("=== MISMATCH: amps / angle / frequency via API vs text query ===")
    dss.isources.amps = 300
    dss.isources.angle_deg = 60
    dss.isources.frequency = 50
    print("  API amps:", dss.isources.amps, "  ? amps:", dss.text("? ISource.ISrc.amps").strip())
    print("  API angle_deg:", dss.isources.angle_deg, "  ? angle:", dss.text("? ISource.ISrc.angle").strip())
    print("  API frequency:", dss.isources.frequency, "  ? frequency:", dss.text("? ISource.ISrc.frequency").strip())

    dss.text("save circuit Dir=isource")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit ISource.ISrc amps=300")
    dss.text("Edit ISource.ISrc angle=60")
    dss.text("Edit ISource.ISrc frequency=50")
    print("  ? amps:", dss.text("? ISource.ISrc.amps").strip())
    print("  ? angle:", dss.text("? ISource.ISrc.angle").strip())
    print("  ? frequency:", dss.text("? ISource.ISrc.frequency").strip())


if __name__ == "__main__":
    main()
