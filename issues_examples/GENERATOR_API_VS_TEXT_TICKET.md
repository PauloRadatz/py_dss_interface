### Generator: API writes for `vmin_pu`, `vmax_pu`, `model`, `phases`, `force_on` do not match `dss.text('? …')` or saved circuit

**Environment:** Reproduced with **py-dss-interface** version 2.3.0 using the **Delphi** build of OpenDSS (`OpenDSSDirect.dll`) version 11.0.0.1.

**Problem:** After setting `vmin_pu`, `vmax_pu`, `model`, `phases`, or `force_on` via the py-dss-interface / DLL API, the Python getters show the new values, but `dss.text("? Generator.G1.…")` still returns the original values. **`save circuit`** matches the `?` output, not those API getters.

**Repro:** Run the script below.

---

```python
# -*- coding: utf-8 -*-
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com

from pathlib import Path

from py_dss_interface import DSS

DSS_MODEL = """
ClearAll
New Circuit.Thevenin bus1=SourceBus pu=1.0 basekv=13.8 model=ideal
New Generator.G1 bus1=SourceBus phases=1 kV=2.4 kW=100 kvar=-50 Model=1 Vpu=1 vminpu=0.95 forceon=no
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.generators.name = "G1"

    print("=== MISMATCH: vmin_pu / vmax_pu via API vs text query ===")
    dss.generators.vmin_pu = 0.85
    dss.generators.vmax_pu = 1.2
    print("  API vmin_pu:", dss.generators.vmin_pu, "  ? Vminpu:", dss.text("? Generator.G1.Vminpu").strip())
    print("  API vmax_pu:", dss.generators.vmax_pu, "  ? Vmaxpu:", dss.text("? Generator.G1.Vmaxpu").strip())

    print()
    print("=== MISMATCH: model / phases via API vs text query ===")
    dss.generators.model = 2
    dss.generators.phases = 3
    print("  API model:", dss.generators.model, "  ? model:", dss.text("? Generator.G1.model").strip())
    print("  API phases:", dss.generators.phases, "  ? phases:", dss.text("? Generator.G1.phases").strip())

    print()
    print("=== MISMATCH: force_on via API vs text query ===")
    dss.generators.force_on = 1
    print("  API force_on:", dss.generators.force_on, "  ? ForceON:", dss.text("? Generator.G1.ForceON").strip())

    dss.text("save circuit Dir=generator")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit Generator.G1 Vminpu=0.85 Vmaxpu=1.2")
    dss.text("Edit Generator.G1 model=2 phases=3")
    dss.text("Edit Generator.G1 forceon=yes")
    print("  ? Vminpu:", dss.text("? Generator.G1.Vminpu").strip())
    print("  ? Vmaxpu:", dss.text("? Generator.G1.Vmaxpu").strip())
    print("  ? model:", dss.text("? Generator.G1.model").strip())
    print("  ? phases:", dss.text("? Generator.G1.phases").strip())
    print("  ? ForceON:", dss.text("? Generator.G1.ForceON").strip())


if __name__ == "__main__":
    main()
```
