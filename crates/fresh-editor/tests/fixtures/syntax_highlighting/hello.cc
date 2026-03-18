// C++ .cc extension syntax highlighting test
#include <iostream>
#include <string>
#include <vector>

auto greet(const std::string& name) -> std::string {
    return "Hello, " + name + "!";
}

int main() {
    auto message = greet("World");
    std::cout << message << std::endl;
    
    std::vector<int> items = {1, 2, 3};
    for (const auto& item : items) {
        std::cout << "Item: " << item << "\n";
    }
    return 0;
}
