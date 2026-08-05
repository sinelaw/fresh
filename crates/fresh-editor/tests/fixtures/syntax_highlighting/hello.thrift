// Thrift syntax highlighting test
namespace java com.example.hello
namespace py example.hello

include "shared.thrift"

typedef i64 UserId

enum Status {
  UNKNOWN = 0,
  ACTIVE = 1,
  INACTIVE = 2
}

struct Greeting {
  1: required UserId user_id,
  2: optional string message = "Hello",
  3: list<string> tags,
  4: map<string, i32> scores
}

exception GreetingError {
  1: string reason
}

service GreetingService {
  Greeting sayHello(1: UserId user_id) throws (1: GreetingError error),
  oneway void recordGreeting(1: Greeting greeting)
}

const bool ENABLED = true
