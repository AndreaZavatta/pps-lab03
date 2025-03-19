package u03

import u02.AlgebraicDataTypes.*
import u02.AlgebraicDataTypes.Person.Teacher
import u03.Sequences.*
import u03.Sequences.Sequence.*

import scala.annotation.tailrec
object Exercise2 extends App:
  def getCoursesOfTeachers(seq: Sequence[Person]): Sequence[String] =
    map(filter(seq)(x => x.isInstanceOf[Teacher]))(x => x.asInstanceOf[Teacher].course)

  def getCoursesOfTeachersFlatmap(seq: Sequence[Person]): Sequence[String] =
    flatMap(seq) {
      case t: Teacher => Cons(t.course, Nil())
      case _ => Nil()
    }

  @tailrec
  def foldLeft(sequence: Sequence[Int])(defaultValue: Int)(fun: (Int, Int) => Int): Int = sequence match
    case Cons(h, t) => foldLeft(t)(fun(defaultValue, h))(fun)
    case Nil() => defaultValue

