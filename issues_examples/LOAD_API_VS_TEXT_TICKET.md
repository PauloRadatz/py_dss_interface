### Load: API writes for `model` do not match `dss.text('? …')` or saved circuit

**Environment:** Reproduced with **py-dss-interface** version 2.3.0 using the **Delphi** build of OpenDSS (`OpenDSSDirect.dll`) version 11.0.0.1.

**Problem:** After setting `model` via the py-dss-interface / DLL API, the Python getter shows the new value, but `dss.text("? Load.Ld.…")` still returns the original value. **`save circuit`** matches the `?` output, not that API getter.

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
New Load.Ld bus1=SourceBus.1 phases=1 kV=2.4 kW=10 kvar=5 Model=1
"""


def main() -> None:
    dss = DSS()
    dss.dssinterface.datapath = str(Path(__file__).resolve().parent)
    dss.text(DSS_MODEL)
    dss.dssinterface.allow_forms = 0
    dss.loads.name = "Ld"

    print("=== MISMATCH: model via API vs text query ===")
    dss.loads.model = 2
    print("  API model:", dss.loads.model, "  ? model:", dss.text("? Load.Ld.model").strip())

    dss.text("save circuit Dir=load")

    print()
    print("=== OK: values set via text command (engine matches ?) ===")
    dss.text("Edit Load.Ld model=2")
    print("  ? model:", dss.text("? Load.Ld.model").strip())


if __name__ == "__main__":
    main()
```
