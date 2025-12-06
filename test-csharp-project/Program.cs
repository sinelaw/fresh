using System;
using System.Collections.Generic;
using System.Linq;

namespace TestProject
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("Hello, Fresh!");

            var calculator = new Calculator();
            int result = calculator.Add(5, 3);
            Console.WriteLine($"5 + 3 = {result}");

            var numbers = new List<int> { 1, 2, 3, 4, 5 };
            var doubled = calculator.DoubleAll(numbers);
            Console.WriteLine($"Doubled: {string.Join(", ", doubled)}");
        }
    }

    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }

        public int Subtract(int a, int b)
        {
            return a - b;
        }

        public List<int> DoubleAll(List<int> numbers)
        {
            return numbers.Select(n => n * 2).ToList();
        }
    }

    public interface IGreeter
    {
        string Greet(string name);
    }

    public class FormalGreeter : IGreeter
    {
        public string Greet(string name)
        {
            return $"Good day, {name}.";
        }
    }

    public class CasualGreeter : IGreeter
    {
        public string Greet(string name)
        {
            return $"Hey {name}!";
        }
    }
}
