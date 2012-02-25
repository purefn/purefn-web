import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  lazy val standardSettings: Seq[Sett] = 
    Defaults.defaultSettings ++ Seq[Sett]( 
      organization := "purefn"
    , version := "0.1-SNAPSHOT"
    , scalaVersion := "2.9.1"
    , scalacOptions ++= Seq("-deprecation", "-unchecked", "-Ydependent-method-types")
    )

  lazy val web = 
    Project( 
      id = "purefn-web"
    , base = file(".")
    , settings = standardSettings
    , aggregate = Seq(core, servlet, examples /*, scalacheckBinding, tests*/)
    )

  lazy val core = 
    Project( 
      id = "core"
    , base = file("core")
    , settings = standardSettings ++ Seq[Sett](name := "purefnweb-core")
    )

  lazy val servlet = 
    Project(
      id = "servlet"
    , base = file("servlet")
    , settings = standardSettings ++ Seq[Sett](
        name := "purefnweb-servlet"
      , libraryDependencies += "javax.servlet" % "servlet-api" % "2.5"
      )
    , dependencies = Seq(core)
    )

  lazy val examples = 
    Project(
      id = "examples"
    , base = file("examples")
    , settings = standardSettings ++ Seq[Sett](
        name := "purefnweb-examples"
      , libraryDependencies += "org.eclipse.jetty" % "jetty-servlet" % "7.5.0.v20110901"
      )
    , dependencies = Seq(core, servlet)
    )
}

// vim: set ts=4 sw=4 et:
