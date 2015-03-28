import sbt._
import Keys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.6",
    crossVersion := CrossVersion.full,
    version := "0.2.0-SNAPSHOT",
    organization := "org.scalamacros",
    description := "Tracks things that are going on during macro expansion",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-feature", "-optimise"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("macrotracker.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    },
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/scalamacros/macrotracker</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalamacros/macrotracker.git</url>
        <connection>scm:git:git://github.com/scalamacros/macrotracker.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalamacros/macrotracker/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>xeno-by</id>
          <name>Eugene Burmako</name>
          <url>http://xeno.by</url>
        </developer>
        <developer>
          <id>Duhemm</id>
          <name>Martin Duhem</name>
          <url>https://github.com/duhemm</url>
        </developer>
      </developers>
    )
  )

  // http://stackoverflow.com/questions/20665007/how-to-publish-only-when-on-master-branch-under-travis-and-sbt-0-13
  val publishOnlyWhenOnMaster = taskKey[Unit]("publish task for Travis (don't publish when building pull requests, only publish when the build is triggered by merge into master)")
  def publishOnlyWhenOnMasterImpl = Def.taskDyn {
    import scala.util.Try
    val travis   = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr       = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch   = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot = version.value.trim.endsWith("SNAPSHOT")
    (travis, pr, branch, snapshot) match {
      case (true, false, "master", true) => publish
      case _                             => Def.task ()
    }
  }

  lazy val publishableSettings = sharedSettings ++ Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= {
      val mavenSettingsFile = System.getProperty("maven.settings.file")
      if (mavenSettingsFile != null) {
        println("Loading Sonatype credentials from " + mavenSettingsFile)
        try {
          import scala.xml._
          val settings = XML.loadFile(mavenSettingsFile)
          def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
          List(Credentials(
            "Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            readServerConfig("username"),
            readServerConfig("password")
          ))
        } catch {
          case ex: Exception =>
            println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
            Nil
        }
      } else {
        (for {
          realm <- sys.env.get("SCALAMACROS_MAVEN_REALM")
          domain <- sys.env.get("SCALAMACROS_MAVEN_DOMAIN")
          user <- sys.env.get("SCALAMACROS_MAVEN_USER")
          password <- sys.env.get("SCALAMACROS_MAVEN_PASSWORD")
        } yield {
          println("Loading Sonatype credentials from environment variables")
          Credentials(realm, domain, user, password)
        }).toList
      }
    }
  )

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("macrotracker.plugin.jar", jar.getAbsolutePath)
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  // This is very similar to usePluginSettings, except that we don't add the plugin
  // to the compiler.
  lazy val rebuildWhenPluginIsChangedSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("macrotracker.plugin.jar", jar.getAbsolutePath)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(dummy)
    }
  )

  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    test in Test := (test in tests in Test).value,
    packagedArtifacts := Map.empty
  ) aggregate (plugin, tests)

  lazy val plugin = Project(
    id   = "macrotracker",
    base = file("plugin")
  ) settings (
    publishableSettings: _*
  ) settings (
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base / "src"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    scalacOptions ++= Seq()
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    scalacOptions ++= Seq()
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    // Running the tests with the plugin causes problem with scalatest's assert()
    // which is a macro =(
    sharedSettings ++ rebuildWhenPluginIsChangedSettings: _*
  ) settings (
    packagedArtifacts := Map.empty,
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    compile in Test := {
      sys.props("sbt.class.directory") = (classDirectory in Test).value.getAbsolutePath
      (compile in Test).value
    }
  ) dependsOn (
    plugin
  )
}
