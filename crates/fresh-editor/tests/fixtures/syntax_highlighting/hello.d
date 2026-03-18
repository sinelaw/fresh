// D syntax highlighting test
import std.stdio;
import std.string;

string greet(string name) {
    return format("Hello, %s!", name);
}

void main() {
    auto message = greet("World");
    writeln(message);
    foreach (i; 0..5) {
        writefln("Item: %d", i);
    }
}
