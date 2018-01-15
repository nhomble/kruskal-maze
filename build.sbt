enablePlugins(ScalaJSPlugin)

name := "kruskal-maze"

version := "1.0"

scalaVersion := "2.12.4"

scalaJSUseMainModuleInitializer := true
// https://mvnrepository.com/artifact/org.scala-js/scalajs-library
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
