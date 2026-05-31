### Monitor: API writes for `terminal`, `mode` do not match `dss.text('? …')` or saved circuit

**Environment:** Reproduced with **py-dss-interface** version 2.3.0 using the **Delphi** build of OpenDSS (`OpenDSSDirect.dll`) version 11.0.0.1.

**Problem:** After setting `terminal` or `mode` via the py-dss-interface / DLL API, the Python getters show the new values, but `dss.text("? Monitor.M1.…")` still returns the original values. **`save circuit`** matches the `?` output, not those API getters.

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
New Line.L1 phases=3 bus1=SourceBus bus2=LoadBus length=0.01 units=mi r1=0.1 x1=0.1 r0=0.1 x0=0.1 c1=0.0 c0=0.0
New Monitor.M1 element=Line.L1 terminal=1 mode=0
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.monitors.name = "M1"

    print("=== MISMATCH: terminal / mode via API vs text query ===")
    dss.monitors.terminal = 2
    dss.monitors.mode = 1
    print("  API terminal:", dss.monitors.terminal, "  ? terminal:", dss.text("? Monitor.M1.terminal").strip())
    print("  API mode:", dss.monitors.mode, "  ? mode:", dss.text("? Monitor.M1.mode").strip())

    dss.text("save circuit Dir=monitor")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit Monitor.M1 terminal=2 mode=1")
    print("  ? terminal:", dss.text("? Monitor.M1.terminal").strip())
    print("  ? mode:", dss.text("? Monitor.M1.mode").strip())


if __name__ == "__main__":
    main()
```
