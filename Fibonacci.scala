object Fibonacci {
    def fib(n: Int) = {
        def loop(n: Int): Int = {
            if (n < 2) n
            else loop(n - 1) + loop(n - 2)
        }

        loop(n)
    }

    def fib2(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, a: Int, b: Int): Int = {
            if (n == 0) a
            else if (n == 1) b
            else loop(n - 1, b, a + b)
        }

        loop(n, 0, 1)
    }

    def main(args: Array[String]): Unit = {
        println(fib2(20))
    }
}