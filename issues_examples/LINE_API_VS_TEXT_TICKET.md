### Line: API writes for `norm_amps`, `emerg_amps` do not match `dss.text('? …')` or saved circuit

**Environment:** Reproduced with **py-dss-interface** version 2.3.0 using the **Delphi** build of OpenDSS (`OpenDSSDirect.dll`) version 11.0.0.1.

**Problem:** After setting `norm_amps` or `emerg_amps` via the py-dss-interface / DLL API, the Python getters show the new values, but `dss.text("? Line.L1.…")` still returns the original values. **`save circuit`** matches the `?` output, not those API getters.

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
New Line.L1 phases=3 bus1=SourceBus bus2=LoadBus length=1 units=mi
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.circuit.set_active_element("line.l1")

    print("=== MISMATCH: norm_amps / emerg_amps via API vs text query ===")
    dss.lines.norm_amps = 500.0
    dss.lines.emerg_amps = 500.0
    print("  API norm_amps:", dss.lines.norm_amps, "  ? normamps:", dss.text("? Line.L1.normamps").strip())
    print("  API emerg_amps:", dss.lines.emerg_amps, "  ? emergamps:", dss.text("? Line.L1.emergamps").strip())

    dss.text("save circuit Dir=line")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit Line.L1 normamps=500 emergamps=500")
    print("  ? normamps:", dss.text("? Line.L1.normamps").strip())
    print("  ? emergamps:", dss.text("? Line.L1.emergamps").strip())


if __name__ == "__main__":
    main()
```
