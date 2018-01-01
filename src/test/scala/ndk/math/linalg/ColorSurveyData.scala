package ndk.math.linalg

import org.scalatest.FunSpec

import scala.io.Source
import scala.util.Try

object ColorSurveyData {
  private def getSourceStream = {
    Source.fromFile(getClass.getResource("/uncommitted/colors/mainsurvey_sqldump.txt").toURI)
  }
  private def getIntOption (input: String): Option[Int] = Try(input.toInt).toOption
  private def getLongOption (input: String): Option[Long] = Try(input.toLong).toOption
  private def getDoubleOption (input: String): Option[Double] = Try(input.toDouble).toOption

  def getNameData: Iterator[Name] = {
    val ID = "id"
    val CNAME = "colorName"
    val UNUM = "numUsers"
    val INUM = "numInstances"
    val pattern = """(?i)INSERT\s+INTO\s+"names"\s*VALUES\(\s*([0-9]+)\s*,\s*'(.*)'\s*,\s*(.*)\s*,\s*(.*)\s*\);"""
      .r(ID, CNAME, UNUM, INUM)

    getSourceStream.getLines()
      .map(pattern.findFirstMatchIn(_))
      .filter(_.isDefined)
      .map(_.get)
      .map(m =>
        Name(
          m.group(ID).toInt,
          m.group(CNAME),
          getIntOption(m.group(UNUM)),
          getIntOption(m.group(INUM))
        )
      )
  }

  def getAnswerData: Iterator[Answer] = {
    val ID = "id"
    val UID = "userId"
    val DATE = "date"
    val R = "r"
    val G = "g"
    val B = "b"
    val CNAME = "colorName"
    val pattern =
      """(?i)INSERT\s+INTO\s+"answers"\s*VALUES\(\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9.]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*'(.*)'\);"""
        .r(ID, UID, DATE, R, G, B, CNAME)
    getSourceStream.getLines()
      .map(pattern.findFirstMatchIn(_))
      .filter(_.isDefined)
      .map(_.get)
      .map { m =>
        Answer(
          m.group(ID).toInt,
          m.group(UID).toLong,
          m.group(DATE).toDouble.toLong,
          m.group(R).toInt,
          m.group(G).toInt,
          m.group(B).toInt,
          m.group(CNAME)
        )
      }
  }
}
case class Name (id: Int, name: String, numUsers: Option[Int], numInstances: Option[Int])
case class Answer (id: Int, uid: Long, date: Long, r: Int, g: Int, b: Int, name: String)
class ColorSurveyDataTests extends FunSpec {
  describe("name table") {
    it("should contain hundreds of thousands of records") {
      assert(ColorSurveyData.getNameData.length > 100000)
    }
  }
  describe("answer table") {
    it("should contain millions of records") {
      assert(ColorSurveyData.getAnswerData.length > 1000000)
    }
  }
}
