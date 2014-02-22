name := "ScalaGP"

version := "1.0"

scalaVersion := "2.10.1"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "FWBrasil" at "http://fwbrasil.net/maven"
  //,"Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
)


libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.2",
  "com.chuusai" %% "shapeless" % "1.2.4",
/*"net.fwbrasil" % "activate-core" % "1.2",
  "net.fwbrasil" % "activate-jdbc" % "1.2",
  "net.fwbrasil" % "activate-play" % "1.2",*/
//  "net.fwbrasil" % "activate_2.10" % "1.2",
  "org.spire-math" %% "spire" % "0.5.0",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)

//scalaSource in Test <<= baseDirectory(_ /"test")

ScoverageSbtPlugin.instrumentSettings

//scalaSource in Compile <<= baseDirectory(_ /"src")
