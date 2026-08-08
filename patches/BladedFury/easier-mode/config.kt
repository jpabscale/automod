// Bladed Fury "easy mode -50%" balance mod (raw .kt asset patch). The game's balance lives
// as JSON documents inside TextAsset objects in the `config` UnityFS bundle. This script
// decodes the bundle via `v.toJson()` and uses JSONPath (jayway, same as the `.@` TOMLs) to
// select the MonsterData / BehaviorNode TextAssets and the monster entries to scale.
//
// The number is the reduction: -50% means monsters deal 50% less (AttackParam/DefenseParam
// at 50%).
//
// Changes:
//   - every monster's AttackParam and DefenseParam at 50% (50% less damage from them)
//   - every monster's Money drop multiplied by 10 (rich)
//   - every scene trap/hazard (Category="LevelItem" behavior node: spikes, bolts, etc.)
//     has its Damage halved — these use their own damage, not MonsterData.AttackParam
val tree = v.toJson() as com.fasterxml.jackson.databind.node.ObjectNode
val mapper = com.fasterxml.jackson.databind.ObjectMapper()
val cfg = com.jayway.jsonpath.Configuration.builder()
  .jsonProvider(com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider())
  .mappingProvider(com.jayway.jsonpath.spi.mapper.JacksonMappingProvider())
  .options(com.jayway.jsonpath.Option.ALWAYS_RETURN_LIST)
  .build()

// MonsterData TextAsset: embedded JSON, monsters keyed by id. Select every object whose
// JSON has the three balance fields and scale them via JSONPath.
val mdNodes = com.jayway.jsonpath.JsonPath.using(cfg).parse(tree)
  .read<com.fasterxml.jackson.databind.node.ArrayNode>("$.Files[*].Asset.Objects[*].Data[?(@.m_Name == 'MonsterData')]")
if (mdNodes.isEmpty) throw RuntimeException("MonsterData TextAsset not found in config bundle")
val data = mdNodes.get(0) as com.fasterxml.jackson.databind.node.ObjectNode
val json = mapper.readTree(data.get("m_Script").asText()) as com.fasterxml.jackson.databind.node.ObjectNode
val monsterNodes = com.jayway.jsonpath.JsonPath.using(cfg).parse(json)
  .read<com.fasterxml.jackson.databind.node.ArrayNode>("$..*[?(@.AttackParam != null && @.DefenseParam != null && @.Money != null)]")
for (i in 0 until monsterNodes.size()) {
  val m = monsterNodes.get(i) as com.fasterxml.jackson.databind.node.ObjectNode
  m.replace("AttackParam", com.fasterxml.jackson.databind.node.DoubleNode.valueOf(m.get("AttackParam").asDouble() * 0.5))
  m.replace("DefenseParam", com.fasterxml.jackson.databind.node.DoubleNode.valueOf(m.get("DefenseParam").asDouble() * 0.5))
  m.replace("Money", com.fasterxml.jackson.databind.node.IntNode.valueOf((m.get("Money").asLong() * 10).toInt()))
}
data.put("m_Script", mapper.writeValueAsString(json))

// BehaviorNode TextAsset: XML, not JSON — select the TextAsset via JSONPath, then halve
// Damage="..." and <m_Damage>...</m_Damage> on every non-player behavior node by walking
// the XML. Player/summon categories (Pl01*, Soul, NPC) are left alone; everything else
// (Em*, Bs*, LevelItem* traps, bosses, lamps) is hostile.
val bnNodes = com.jayway.jsonpath.JsonPath.using(cfg).parse(tree)
  .read<com.fasterxml.jackson.databind.node.ArrayNode>("$.Files[*].Asset.Objects[*].Data[?(@.m_Name == 'BehaviorNode')]")
if (bnNodes.isEmpty) throw RuntimeException("BehaviorNode TextAsset not found in config bundle")
val bdata = bnNodes.get(0) as com.fasterxml.jackson.databind.node.ObjectNode
val xml = bdata.get("m_Script").asText()
// XSLT identity transform (JDK javax.xml.transform): keeps attributes as attributes and
// preserves structure/whitespace, quartering Damage="..." attributes and <m_Damage>
// content on hostile behavior nodes (Category present and not Pl01*/Soul/NPC).
val xsl = """<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="no" omit-xml-declaration="no"/>
  <xsl:template match="@*|node()">
    <xsl:copy><xsl:apply-templates select="@*|node()"/></xsl:copy>
  </xsl:template>
  <xsl:template match="BehaviorNode[@Category][not(starts-with(@Category,'Pl01')) and @Category!='Soul' and @Category!='NPC']/@Damage">
    <xsl:attribute name="Damage"><xsl:value-of select="floor(. div 4)"/></xsl:attribute>
  </xsl:template>
  <xsl:template match="BehaviorNode[@Category][not(starts-with(@Category,'Pl01')) and @Category!='Soul' and @Category!='NPC']//m_Damage/text()[. != '' and translate(., '0123456789', '') = '']">
    <xsl:value-of select="floor(. div 4)"/>
  </xsl:template>
</xsl:stylesheet>"""
val transformer = javax.xml.transform.TransformerFactory.newInstance().newTransformer(javax.xml.transform.stream.StreamSource(java.io.StringReader(xsl)))
val sw = java.io.StringWriter()
transformer.transform(javax.xml.transform.stream.StreamSource(java.io.StringReader(xml)), javax.xml.transform.stream.StreamResult(sw))
bdata.put("m_Script", sw.toString())

v.fromJson(tree)
