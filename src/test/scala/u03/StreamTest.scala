package u03
import org.junit.*
import org.junit.Assert.*

import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*

class StreamTest:

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    assertEquals(Cons("A", Cons("A", Nil())), Stream.toList(fill(2)("A")))

  @Test def testFibonacci(): Unit =
    val fibonacci: Stream[Int] = getFibonacciSequence
    assertEquals(Cons(1,Cons(1, Cons(2, Cons(3, Cons(5, Nil()))))), Stream.toList(Stream.take(fibonacci)(5)))

  @Test def testInterleave(): Unit =
    val s1 = Stream.take(Stream.iterate(1)(_ + 2))(3)
    val s2 = Stream.take(Stream.iterate(2)(_ + 2))(5)
    assertEquals(Cons (1 , Cons (2 , Cons (3 , Cons (4 , Cons (5 , Cons (6 , Cons (8 , Cons (10 , Nil ())))))))), Stream.toList(Stream.interleave(s1, s2)))
  // Expected output : Cons (1 , Cons (2 , Cons (3 , Cons (4 , Cons (5 , Cons (6 , Cons (8 , Cons (10 , Nil ())))))))

end StreamTest
