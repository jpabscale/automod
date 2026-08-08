# Warm Snow "player ability cooldowns" — raw byte patch (Python).
#
# Player ability cooldowns are private fields / hardcoded literals in Assembly-CSharp.dll
# (not serialized Unity assets), each compiled as a 5-byte `ldc.r4 <float>` instruction
# (opcode 0x22 + little-endian float). All are reset to 2.0f (00 00 00 40), keeping every
# instruction size identical (no IL shift, no recompile):
#
#   Base flying-sword CD (drawCoolDown / dcd), was 14f:
#     0x127E73  PlayerAnimControl..ctor                        (dcd = 14f)
#     0x23CF04  PlayerAnimControl.<ParameterInit>d__.MoveNext  (drawCoolDown = 14f, new game)
#     0x65656   EnemyControl.LateUpdate -> set_drawCoolDown    (drawCoolDown = 14f, menu)
#
#   Sword-mode overrides of drawCoolDown (were 20-45f):
#     0x126E74 GourdLiquor  0x126F18 UnlimitedSwords  0x126F9A EnergySword
#     0x12701A ThunderDash  0x1270B6 BloodSpray       0x12718C AssassinShenXing
#     0x127203 AssassinMiZong 0x12727F LiangJi        0x127313 HunDun
#     0x127533 SuoHun       0x1275AF PanGuan          0x127637 WanChong  0x127709 KaiTian
#
#   Swordmaster skill cooldowns (were 15f / 4f):
#     0x127C90 0x127C9B  SWORDMASTER_SKILL_GodOfSwords_CoolDown(Timer) = 15f
#     0x127CB1 0x127CBC  SWORDMASTER_SKILL_HeavenlySword_CD(Timer) = 4f
import sys

data = bytearray(v["current"])
# offset -> (label, expected float)
patches = [
    (0x127E73, "dcd.ctor", 14.0),
    (0x23CF04, "dcd.ParameterInit", 14.0),
    (0x65656, "dcd.EnemyControl", 14.0),
    (0x126E74, "GourdLiquor", 28.0),
    (0x126F18, "UnlimitedSwords", 28.0),
    (0x126F9A, "EnergySword", 28.0),
    (0x12701A, "ThunderDash", 20.0),
    (0x1270B6, "BloodSpray", 28.0),
    (0x12718C, "AssassinShenXing", 30.0),
    (0x127203, "AssassinMiZong", 30.0),
    (0x12727F, "LiangJi", 45.0),
    (0x127313, "HunDun", 45.0),
    (0x127533, "SuoHun", 30.0),
    (0x1275AF, "PanGuan", 45.0),
    (0x127637, "WanChong", 30.0),
    (0x127709, "KaiTian", 45.0),
    (0x127C90, "GodOfSwords1", 15.0),
    (0x127C9B, "GodOfSwords2", 15.0),
    (0x127CB1, "HeavenlySword1", 4.0),
    (0x127CBC, "HeavenlySword2", 4.0),
]
import struct
new = struct.pack("<f", 2.0)
for base, label, expected in patches:
    b = bytes(data[base:base + 5])
    if len(b) != 5 or b[0] != 0x22 or struct.unpack("<f", b[1:5])[0] != expected:
        sys.stderr.write("Unexpected bytes at 0x%X (%s): %s - wrong Assembly-CSharp.dll?\n" % (base, label, b.hex()))
        sys.exit(1)
    data[base + 1:base + 5] = new
data
