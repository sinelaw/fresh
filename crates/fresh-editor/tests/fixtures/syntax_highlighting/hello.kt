// Kotlin syntax highlighting test
package hello

fun greet(name: String): String {
    return "Hello, $name!"
}

data class Config(
    val version: String,
    val enabled: Boolean = true,
    val count: Int = 42
)

fun main() {
    val message = greet("World")
    println(message)

    val items = listOf(1, 2, 3, 4, 5)
    items.filter { it > 2 }
         .forEach { println("Item: $it") }
}
