# Warm Snow "skill proc-condition highlight legibility" — raw byte patch (Python).
#
# Skill descriptions are rich text stored in a TextAsset inside sharedassets0.assets.
# Proc-condition phrases ("Final blow of a Melee attack:", "[Bleeding]", "Upon Dashing:",
# ...) are wrapped in <color=#C83E48>, a dark crimson that's hard to read against the
# skill-card background. The hex is a literal ASCII substring, so a same-length byte
# replacement (no asset re-encode) is enough:
#   #C83E48 (200, 62, 72)    ->    #FF6674 (255, 102, 116)    much brighter red
import sys

data = bytearray(v["current"])
old = b"C83E48"
new = b"FF6674"
count = data.count(old)
if count == 0:
    sys.stderr.write("No #C83E48 found in sharedassets0.assets - wrong asset version?\n")
    sys.exit(1)
data = data.replace(old, new)
data
