// Dart syntax highlighting test
import 'dart:io';

String greet(String name) {
  return 'Hello, $name!';
}

class Config {
  final String version;
  final bool enabled;
  final int count;

  Config({
    required this.version,
    this.enabled = true,
    this.count = 42,
  });
}

void main() {
  final message = greet('World');
  print(message);

  final items = [1, 2, 3, 4, 5];
  for (final item in items) {
    print('Item: $item');
  }
}
