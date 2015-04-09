object Main {
  def all_anagram_groups(original: Seq[String]): List[Seq[String]] = original.groupBy(_.replaceAll("\\s", "").toLowerCase.sorted).values.toList
  def main(args: Array[String]): Unit = {
    val original = Seq("pear", "dirty room", "amleth", "reap", "tinsel", "tesla", "hamlet", "dormitory", "listen", "silent")
    val result = all_anagram_groups(original)
    println(result)
  }
}
