object Main {
  def nondecreasing_subsequences(original: Seq[Int]): Vector[Vector[Int]] = original.foldLeft[Vector[Vector[Int]]](Vector(Vector.empty[Int]))(
    (acc: Vector[Vector[Int]], curr: Int) =>
      acc.last.lastOption match{
        case None => acc.init :+ Vector(curr)
        case last: Some[Int] => if(curr >= last.get) acc.init :+ (acc.last :+ curr) else acc :+ Vector(curr)
      }
  )
  def main(args: Array[String]): Unit = {
    val original = Seq(3,6,61,6,7,9,1,7,7,2,7,7,2,388,3,72,7)
    val result = nondecreasing_subsequences(original)
    println(result)
  }
}
