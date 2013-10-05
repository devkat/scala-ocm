import sbt._
import Keys._

object ScalaOcmBuild extends Build {
  
  val buildSettings = Defaults.defaultSettings ++ Seq(
    autoCompilerPlugins := true,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies in ThisBuild ++= Seq(
      //scalaVersion("org.scala-lang" % "scala-reflect" % _),
      "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
      "org.slf4j" % "slf4j-simple" % "1.7.5",
      "javax.jcr" % "jcr" % "2.0",
      "org.specs2" %% "specs2" % "2.2" % "test"
      //"junit" % "junit" % "4.11" % "test"
    )
  )
  
  lazy val common = Project("scala-ocm-common", file("common")) settings (buildSettings ++ Seq(
    libraryDependencies ++= Seq(
    )
  ):_*)

  lazy val macros = Project("scala-ocm-macros", file("macro")) dependsOn(common) settings (buildSettings ++ Seq(
//    scalacOptions += "-Ymacro-debug-lite",
    libraryDependencies in ThisBuild ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.10.2",
      "org.scala-lang" % "scala-reflect" % "2.10.2"
    ),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full),
    parallelExecution in Test := false
  ):_*)

  lazy val core = Project("scala-ocm-core", file("core")) dependsOn (common, macros) settings (buildSettings ++ Seq(
    libraryDependencies ++= Seq(
      "org.apache.jackrabbit" % "jackrabbit-core" % "2.7.0" % "test",
      "commons-io" % "commons-io" % "2.4"
    ),
    testOptions in Test += Tests.Setup(() => {
      System.setProperty("org.apache.jackrabbit.repository.conf", "classpath:repository.xml")
      System.setProperty("org.apache.jackrabbit.repository.home", "target/repository")
    }),
    parallelExecution in Test := false,
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)
    ):_*)

  lazy val root = Project("scala-ocm", file(".")) aggregate(common, macros, core)
  
  override def projects = Seq(root, common, macros, core)
}
