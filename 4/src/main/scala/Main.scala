object Main {

  def intValForString(str: String): Int = {
    val numsLT13 = Seq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
    val nfs = numsLT13.zipWithIndex.toMap
    def numForWord(w: String): Int = w match{
      case x if(x.endsWith("teen")) => 10 + nfs(x.take(x.length-4))
      case x if(x.endsWith("ty")) => x match{
          case "twenty" => 20
          case "thirty" => 30
          case "forty" => 40
          case "fifty" => 50
          case xx => 10*nfs(xx.take(x.length-2))
        }
      case _ => nfs(w)
    }
    val words = str.split("\\s+").reverse
    var result = 0;
    var mul = 1
    var lastNum = false
    words.foreach(w=>
      w match{
        case "billion" => {
            if(lastNum) mul = 1000000000 else mul = mul*1000000000
            lastNum = false
          }
        case "million" => {
          if(lastNum) mul = 1000000 else mul = mul*1000000
          lastNum = false
          }
        case "thousand" => {
          if(lastNum) mul = 1000 else mul = mul*1000
          lastNum = false
          }
        case "hundred" => {
          if(lastNum) mul = 100 else mul = mul*100
          lastNum = false
          }
        case _ => {
          result = result + numForWord(w)*mul
          lastNum = true
          }
      }
    )
    return result
  }
  def sort_numerically(input: Seq[String]): Seq[String] = input.sortBy(intValForString).reverse

  def main(args: Array[String]): Unit = {
    val numLiterals = Seq("seventy five", "two hundred forty one", "three thousand", "one million thirty five thousand twelve", "twenty", "five hundred thousand", "two hundred")
    val result = sort_numerically(numLiterals)
    println(result)
  }
}
