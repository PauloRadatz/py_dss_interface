# API vs `dss.text('? ...')` examples

Standalone scripts with **embedded DSS strings** (no IEEE 13-bus compile). Each follows the same layout: **mismatch** (API vs `?`), **`save circuit Dir=…`**, then **OK** — only those mismatch properties reapplied with **`Edit Class.Name …`** so `?` matches. That contrasts API-only writes with the text/`Edit` path.

Run from the **repository root** so imports resolve:

```text
set PYTHONPATH=src
python issues_examples/issue_generators_api_vs_text.py
```

Or install the package in editable mode (`pip install -e .`) and omit `PYTHONPATH`.

| Script | Topic |
|--------|--------|
| `issue_generators_api_vs_text.py` | Generators |
| `issue_isources_api_vs_text.py` | I sources |
| `issue_lines_api_vs_text.py` | Lines (ampacity) |
| `issue_loads_api_vs_text.py` | Loads |
| `issue_meters_api_vs_text.py` | Energy meters |
| `issue_monitors_api_vs_text.py` | Monitors |
| `issue_reactors_api_vs_text.py` | Reactors |
| `issue_storages_api_vs_text.py` | Storages |
| `issue_vsources_api_vs_text.py` | V sources |

Related: GitHub issue #107 (persistence / direct field writes in OpenDSS DLL).
