using System;
using System.Diagnostics;

using static TestDotNet;

class Program
{
    static Byte op_add(Byte a, Byte b) {
        return (Byte)(a + b);
    }

    static Byte op_sub(Byte a, Byte b) {
        return (Byte)(a - b);
    }

    static void Main(string[] args)
    {
        Debug.Assert(3 == apply_bin_fn(1, 2, op_add));
        Debug.Assert(2 == apply_bin_fn(4, 2, op_sub));
        Console.WriteLine("all tests passed");
    }
}
