# Elixir syntax highlighting test
defmodule Hello do
  @moduledoc "A greeting module"

  def greet(name) do
    "Hello, #{name}!"
  end

  def main do
    message = greet("World")
    IO.puts(message)

    Enum.each(1..5, fn i ->
      IO.puts("Item: #{i}")
    end)
  end
end
