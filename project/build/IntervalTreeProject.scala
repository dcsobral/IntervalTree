import sbt._

class IntervalTreeProject(info: ProjectInfo) extends DefaultProject(info)
{
  val sc = "org.scala-tools.testing" % "scalacheck_2.8.0.Beta1-RC5" % "1.7-SNAPSHOT" % "test"
  //val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val snapshots = ScalaToolsSnapshots
  
  override def compileOptions = "-g -unchecked -encoding utf8".split(" ").map(CompileOption).toSeq ++ super.compileOptions
  
}

