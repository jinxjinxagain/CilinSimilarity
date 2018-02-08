name := "CilinSimilarity"

version := "0.1"

scalaVersion := "2.11.8"

resourceDirectory in Compile <<= baseDirectory(_ / "src/main/resources")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
