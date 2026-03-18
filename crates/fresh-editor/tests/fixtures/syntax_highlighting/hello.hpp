// C++ header (.hpp) syntax highlighting test
#pragma once

#include <string>
#include <optional>

namespace hello {

class Greeter {
public:
    explicit Greeter(std::string name);
    [[nodiscard]] std::string greet() const;
    
private:
    std::string name_;
    int count_ = 0;
};

template<typename T>
std::optional<T> maybe_value(bool condition, T value) {
    if (condition) return value;
    return std::nullopt;
}

} // namespace hello
