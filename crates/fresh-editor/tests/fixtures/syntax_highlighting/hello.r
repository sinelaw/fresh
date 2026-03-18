# R syntax highlighting test
greet <- function(name) {
  paste("Hello,", name, "!")
}

message <- greet("World")
print(message)

x <- c(1, 2, 3, 4, 5)
mean_x <- mean(x)
if (mean_x > 3) {
  cat("Above threshold\n")
}
