object Main {
  def get_links(page: String): List[String] = {
    return page match{
      case "page1" => List("page2", "page5")
      case "page2" => List("page3", "page4")
      case "page5" => List("page3", "page7")
      case _ => List.empty[String]
    }
  }
  
  def get_hops_from(page1: String, page2: String): Option[Int] = {
    if(page1 == page2) return Some(0)
    var visited = Set.empty[String]
    def search(page1: String, page2: String, depth: Int):Option[Int] =
    {
      val newLinks = get_links(page1).toSet &~ visited
      if(newLinks.contains(page2)) return Some(depth + 1)
      else
      {
        val results = newLinks.map{p: String => search(p, page2, depth+1)}.flatten
        return if(results.isEmpty) None else Some(results.min)
      }
    }
    return search(page1, page2, 0)
  }
  def main(args: Array[String]): Unit = {
    val result = get_hops_from("page1", "page7")
    println(result)
  }
}
