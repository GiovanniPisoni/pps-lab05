package ex

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import util.Sequences.Sequence
import Sequence.*
import ex.SchoolModel.*
import BasicSchool.*

object SchoolModel:
  case class TeacherAndCourse(t: Teacher, c: Course)

  type School = SchoolModule
  type Teacher = String
  type Course = String

  trait SchoolModule:
    def courses(): Sequence[Course]
    def teachers(): Sequence[Teacher]
    def setTeacherToCourse(teacher: Teacher, course: Course): School
    def courseOfATeacher(teacher: Teacher): Sequence[Course]
    def hasTeacher(name: String): Boolean
    def hasCourse(name: String): Boolean

  object BasicSchool:
    def emptySchool: School = new BasicSchoolModule()
    def teacher(name: String): Teacher = name
    def course(name: String): Course = name

  private class BasicSchoolModule(private var school: Sequence[TeacherAndCourse] = Nil()) extends SchoolModule:
    def courses(): Sequence[Course] =
      school.map(_ match
        case TeacherAndCourse(_, c) => c
      ).distinct()
    def teachers(): Sequence[Teacher] =
      school.map(_ match
        case TeacherAndCourse(t, _) => t
      ).distinct()
    def setTeacherToCourse(teacher: Teacher, course: Course): School =
      BasicSchoolModule(school.append(TeacherAndCourse(teacher, course)))
    def courseOfATeacher(teacher: Teacher): Sequence[Course] =
      school.flatMap(_ match
        case TeacherAndCourse(t, c) if t == teacher => Sequence.Cons(c, Sequence.Nil())
        case _ => Sequence.Nil()
      ).distinct()
    def hasTeacher(name: Teacher): Boolean = teachers().contains(name)
    def hasCourse(name: Teacher): Boolean = courses().contains(name)

end SchoolModel


class SchoolModelTest:

  import SchoolModel.*

  private val school = emptySchool
  private val john: Teacher = teacher("John")
  private val pablo: Teacher = teacher("Pablo")
  private val math: Course = course("Math")
  private val italian: Course = course("Italian")
  private val school2: School = school.setTeacherToCourse(john, math)
  private val school3: School = school2.setTeacherToCourse(john, italian).setTeacherToCourse(pablo, italian)

  @Test def testTeacher() =
    assertEquals(Cons("John", Nil()), school2.teachers())
    assertEquals(Cons("John", Cons("Pablo", Nil())), school3.teachers())
    assertEquals(Nil(), school.teachers())

  @Test def testCourse() =
    assertEquals(Cons("Math", Nil()), school2.courses())
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.courses())
    assertEquals(Nil(), school.courses())

  @Test def testHasCourse() =
    assertFalse(school.hasCourse("Math"))
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("Italian"))
    assertFalse(school2.hasCourse("English"))
    assertTrue(school3.hasCourse("Math"))
    assertTrue(school3.hasCourse("Italian"))

  @Test def testHasTeacher() =
    assertFalse(school.hasTeacher("John"))
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school3.hasTeacher("John"))
    assertTrue(school3.hasTeacher("Pablo"))

  @Test def testCoursesOfATeacher() =
    assertEquals(Cons("Math", Nil()), school2.courseOfATeacher(john))
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.courseOfATeacher(john))
    assertEquals(Cons("Italian", Nil()), school3.courseOfATeacher(pablo))
    assertEquals(Nil(), school.courseOfATeacher(john))
