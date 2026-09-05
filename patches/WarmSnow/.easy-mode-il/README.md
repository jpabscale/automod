# easy-mode-il

Static IL variant of `.easy-mode`: `Managed$Assembly-CSharp.dll.ilpatch` replaces the
Python byte-patch (`Managed$Assembly-CSharp.dll.py`). Same cooldown changes, expressed
as semantic TOML rules (type/method/value/occurrence) executed by dnlib4j — no hex
offsets, survives game updates that keep the instruction pattern intact.

Verified byte-identical to the `.easy-mode` output on WarmSnow 1.x (PC, Steam).
