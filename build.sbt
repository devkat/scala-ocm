resolvers += Classpaths.typesafeResolver

organization := "net.devkat"

name := "scala-ocm"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "javax.jcr" % "jcr" % "2.0",
  "org.apache.jackrabbit" % "jackrabbit-core" % "2.7.0" % "test",
  "org.specs2" %% "specs2" % "2.2" % "test",
  "commons-io" % "commons-io" % "2.4",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "org.slf4j" % "slf4j-simple" % "1.7.5"
)

parallelExecution in Test := false

testOptions in Test += Tests.Setup(() => {
    System.setProperty("org.apache.jackrabbit.repository.conf", "classpath:repository.xml")
    System.setProperty("org.apache.jackrabbit.repository.home", "target/repository")
  }
)
