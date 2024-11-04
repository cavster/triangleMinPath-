import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import java.io.FileNotFoundException


class MinTrianglePathTest extends AnyFlatSpec with Matchers {

  // Helper function to read triangle from a file
  def readTriangleFromFile(filePath: String): List[List[Int]] = {
    Source.fromFile(filePath).getLines().toList
      .filter(_.nonEmpty)
      .map(_.split(" ").map(_.toInt).toList)
  }

  "The minimumPath function" should "calculate the correct minimal path for the triangle in data_small.txt" in {
    val triangle = readTriangleFromFile("data_small.txt")

    val (minPathSum, path) = MinTrianglePath.minimumPath(triangle)

    minPathSum shouldEqual 50
    path shouldEqual  List.fill(50)(1)}

  "The minimumPath function" should "calculate the correct minimal path for a big triangle in data_big.txt" in {
    val triangle = readTriangleFromFile("data_big.txt")

    val (minPathSum, path) = MinTrianglePath.minimumPath(triangle)

    minPathSum shouldEqual 2000
    path shouldEqual List.fill(2000)(1) }


  it should "calculate the correct minimal path for a simple triangle" in {
    val triangle = List(
      List(1),
      List(2, 3),
      List(4, 5, 6)
    )

    val (minPathSum, path) = MinTrianglePath.minimumPath(triangle)

    minPathSum shouldEqual 7 // 1 + 2 + 4
    path shouldEqual List(1, 2, 4)
  }


  it should "return 0 for an empty triangle" in {
    val triangle = List[List[Int]]()

    val (minPathSum, path) = MinTrianglePath.minimumPath(triangle)

    minPathSum shouldEqual 0
    path shouldEqual List()
  }

  it should "throw an exception if the file is not found" in {
    assertThrows[FileNotFoundException] {
      readTriangleFromFile("non_existing_file.txt")
    }
  }
}
