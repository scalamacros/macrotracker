### Macrotracker

This is a compiler plugin that tracks things that are going on during macro expansion
and then attaches the accumulated knowledge to expanded trees as a `scala.collection.immutable.Map[String, Any]`.
So far we only remember the symbols that have been touched during expansion, storing them as
`touchedSymbols -> scala.collection.immutable.List[global.Symbol]`.

### How to use

`addCompilerPlugin("org.scalamacros" % "macrotracker" % "0.1.0-SNAPSHOT" cross CrossVersion.full)`.
