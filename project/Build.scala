import sbt._
import Keys._

object ScalaOcmBuild extends Build {
  
  val buildSettings = Defaults.defaultSettings ++ Seq(
    autoCompilerPlugins := true,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies in ThisBuild ++= Seq(
      //scalaVersion("org.scala-lang" % "scala-reflect" % _),
      "org.scala-lang" % "scala-reflect" % "2.10.2",
      "org.specs2" %% "specs2" % "2.2" % "test"
    ),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)
  )
  
  lazy val macro = Project("scala-ocm-macros", file("macro")) settings (buildSettings ++ Seq(
    scalacOptions += "-Ymacro-debug-lite"
    //libraryDependencies += scalaVersion("org.scala-lang" % "scala-compiler" % _),
  ):_*)

  lazy val main = Project("main", file(".")) dependsOn (macro) settings (buildSettings ++ Seq(
    libraryDependencies ++= Seq(
      "javax.jcr" % "jcr" % "2.0",
      "org.apache.jackrabbit" % "jackrabbit-core" % "2.7.0" % "test",
      "commons-io" % "commons-io" % "2.4",
      "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
      "org.slf4j" % "slf4j-simple" % "1.7.5"),
      parallelExecution in Test := false,
      testOptions in Test += Tests.Setup(() => {
        System.setProperty("org.apache.jackrabbit.repository.conf", "classpath:repository.xml")
        System.setProperty("org.apache.jackrabbit.repository.home", "target/repository")
      })
    ):_*)

}