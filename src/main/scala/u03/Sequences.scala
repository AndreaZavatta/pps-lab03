package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => skip(t)(n-1)
      case _ => s

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1,t1), Cons(h2,t2)) => Cons((h1,h2), zip(t1, t2))
      case _ => Nil()
    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h1, t1) => Cons(h1,concat(t1, s2))
      case _ => s2

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def helper(s2: Sequence[A], acc: Sequence[A]): Sequence[A] = s2 match
        case Cons(h1, t1) => helper(t1, Cons(h1, acc))
        case _ => acc
      helper(s, Nil())





    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()
    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      @tailrec
      def helper(s2: Sequence[Int], acc: Int): Optional[Int] = s2 match
        case Cons(h, t) => helper(t, if h < acc then h else acc)
        case _ => Optional.Just(acc)
      s match
        case Cons(h, t) => helper(t, h)
        case _ => Optional.Empty()

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def helper(s : Sequence[A], acc: Sequence[A], takeThat: Boolean): Sequence[A] = s match
      case Cons(h,t) => helper(t, if takeThat then concat(acc, Cons(h, Nil())) else acc, !takeThat)
      case _ => acc
      helper(s, Nil(), true)

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h,t) if h == elem => true
      case Cons(h,t) if h != elem => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def helper(s2: Sequence[A], acc: Sequence[A]): Sequence[A] = s2 match
        case Cons(h, t) if contains(acc)(h) => helper(t, acc)
        case Cons(h, t) if !contains(acc)(h) => helper(t, concat(acc, Cons(h, Nil())))
        case _ => acc
      helper(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @tailrec
      def helper(remaining: Sequence[A], currentGroup: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] =
        (remaining, currentGroup) match
          case (Cons(h, t), Cons(h2, t2)) if h == h2 =>
            helper(t, Cons(h, currentGroup), acc)
          case (Cons(h, t), _) =>
            helper(t, Cons(h, Nil()), if currentGroup != Nil() then concat(acc, Cons(currentGroup, Nil())) else acc)
          case (Nil(), _) =>
            if currentGroup != Nil() then concat(acc, Cons(currentGroup, Nil())) else acc

      helper(s, Nil(), Nil())


    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def helper(s2: Sequence[A])(pred2: A => Boolean)(firstSequence: Sequence[A])(secondSequence: Sequence[A]): (Sequence[A], Sequence[A]) = s2 match
        case Cons(h, t) if pred2.apply(h) => helper(t)(pred2)(concat(firstSequence, Cons(h, Nil())))(secondSequence)
        case Cons(h,t) if !pred2.apply(h) => helper(t)(pred2)(firstSequence)(concat(secondSequence, Cons(h, Nil())))
        case _ => (firstSequence, secondSequence)
      helper(s)(pred)(Nil())(Nil())
  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52
