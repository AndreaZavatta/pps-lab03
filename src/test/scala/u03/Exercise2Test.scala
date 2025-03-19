package u03

import org.junit.*
import org.junit.Assert.*
import u02.AlgebraicDataTypes.*
import u02.AlgebraicDataTypes.Person.{Student, Teacher}
import u03.Sequences.*
import u03.Sequences.Sequence.*

class Exercise2Test:
  import Exercise2.*

  @Test
  def testGetCoursesWithFilterAndMap() =
    val courses = Cons("Italian", Cons("Computer Science", Nil()))
    val people = Cons(Teacher("Paola", "Italian"), Cons(Teacher("Matteo", "Computer Science"), Cons(Student("Marco", 2001), Nil())))
    assertEquals(courses, getCoursesOfTeachers(people))

  @Test
  def testGetCoursesWithFlatmap() =
    val courses = Cons("Italian", Cons("Computer Science", Nil()))
    val people = Cons(Teacher("Paola", "Italian"), Cons(Teacher("Matteo", "Computer Science"), Cons(Student("Marco", 2001), Nil())))
    assertEquals(courses, getCoursesOfTeachersFlatmap(people))

  @Test
  def testFoldLeftSubtraction() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test
  def testFoldLeftSum() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
