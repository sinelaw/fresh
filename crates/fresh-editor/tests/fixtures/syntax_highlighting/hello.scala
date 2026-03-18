object Hello {
  def greet(name: String): String = {
    s"Hello, $name!"
  }

  def main(args: Array[String]): Unit = {
    val message = greet("World")
    println(message)
    val items = List(1, 2, 3)
    items.foreach(i => println(s"Item: $i"))
  }
}
