object Sorted {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(i: Int): Boolean = {
            if (i >= as.length -  1) true
            else if (!ordered(as(i), as(i + 1))) false 
            else loop(i + 1)
        }

        loop(0)
    }

    def compare(a: Int, b: Int): Boolean = {
        if (a < b || a == b) true
        else false
    }

    def main(args: Array[String]): Unit = {
        println(isSorted(Array(1,2,3,4,5), compare))
    }
}