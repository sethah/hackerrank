package util

object FileUtils {

  def testCase(challenge: String, n: Int): Iterator[String] = {
    val base = "/Users/sethhendrickson/Development/hackerrank/"
    scala.io.Source.fromFile(base + s"testcases/${challenge.toLowerCase()}/testcase$n").getLines()
  }

}
