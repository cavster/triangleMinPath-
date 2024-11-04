
case class TriangleNode(value: Int, path: List[Int])

object MinTrianglePath {
  def minimumPath(triangle: List[List[Int]]): (Int, List[Int]) = {
    if (triangle.isEmpty || triangle.head.isEmpty) {
      return (0, List())
    }
    @annotation.tailrec
    def reduceRows(currentRow: List[TriangleNode], remainingRows: List[List[TriangleNode]]): (Int, List[Int]) = {
      remainingRows match {
        case Nil => (currentRow.head.value, currentRow.head.path.reverse)
        case nextRow :: rest =>
          val newRow = nextRow.zip(currentRow.zip(currentRow.tail)).map {
            case (node, (leftNode, rightNode)) =>
              if (leftNode.value < rightNode.value)
                TriangleNode(node.value + leftNode.value, node.value :: leftNode.path)
              else
                TriangleNode(node.value + rightNode.value, node.value :: rightNode.path)
          }
          reduceRows(newRow, rest)
      }
    }

    val triangleWithPaths = triangle.map(_.map(value => TriangleNode(value, List(value))))

  val answer: (Int, List[Int]) =  reduceRows(triangleWithPaths.last, triangleWithPaths.init.reverse)
  val answerWithPathFromTop =  (answer._1,  answer._2.reverse)
    answerWithPathFromTop
  }

  import scala.io.Source

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Please provide a filename as an argument.")
      return
    }

    val triangle = Source.fromFile(args(0)).getLines().toList.map { line =>
      line.split(" ").map(_.toInt).toList
    }

    if (triangle.nonEmpty) {
      val (minPathSum, path) = minimumPath(triangle)
      println(s"Minimal path is: ${path.mkString(" + ")} = $minPathSum")
    } else {
      println("No triangle data was entered.")
    }
  }


}