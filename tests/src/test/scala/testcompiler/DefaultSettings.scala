package testcompiler

import scala.tools.nsc.Settings

object DefaultSettings {

  def apply(outputDir: java.io.File) = {
    val settings = new Settings()

    settings.d.value = outputDir.toString

    // Grab scala-library.jar and scala-reflect.jar from boot class path
    // They are required to run scalac and be able to compile macros
    //val scalaLibraryAndReflectJars =
    //  System.getProperty("sun.boot.class.path").split(":").filter(str => str.endsWith("scala-library.jar") || str.endsWith("scala-reflect.jar")).mkString(":")

    val scalaLibraryAndReflectJars = List(
      "/Users/martin/Applications/scala-2.11.0-RC3/lib/scala-library.jar",
      "/Users/martin/Applications/scala-2.11.0-RC3/lib/scala-reflect.jar").mkString(":")

    // TODO Find out how not to hard code these !
    settings.plugin.value = List("/Users/martin/Documents/Projects/Duhemm/scalahost/plugin/target/scala-2.11.0-RC3/scalahost_2.11.0-RC3-0.1.0-SNAPSHOT.jar")

    // `outputDir` has to be on the classpath because we must compile
    // separately macro providers and clients.
    settings.classpath.value = scalaLibraryAndReflectJars + ":" + outputDir

    settings
  }
}