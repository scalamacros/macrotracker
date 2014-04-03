package testutils

import org.scalatest.FunSuite
import org.scalatest.fixture
import java.io.File

class TemporaryDirSuite extends fixture.FunSuite {

  private def safeFileName(length: Int): String = {
    import scala.util.Random
    List.fill(length)((Random.nextInt(26) + 65).toChar).mkString
  }

  private def delete(f: File): Unit = {
    if(f.isDirectory) f.listFiles.foreach(delete)
    f.delete
  }

  case class FixtureParam(dir: File)

  def withFixture(test: OneArgTest) = {

    val outputDir = new File(System.getProperty("java.io.tmpdir") + safeFileName(10))
    outputDir.mkdirs()
    val theFixture = FixtureParam(outputDir)

    try {
      withFixture(test.toNoArgTest(theFixture))
    }
    finally delete(outputDir)
  }
}