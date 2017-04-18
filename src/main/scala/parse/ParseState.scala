package x

sealed trait ParseState[A] {
  def map[B](f: A => B): ParseState[B] =
    this match {
      case ParseOk(input, value) =>
        ParseOk(input, f(value))
      case ParseKo(message) =>
        ParseKo(message)
    }

  def flatMap[B](f: A => ParseState[B]): ParseState[B] =
    this match {
      case ParseOk(input, value) =>
        f(value)
      case ParseKo(message) =>
        ParseKo(message)
    }
}
case class ParseOk[A](input: String, value: A) extends ParseState[A]
case class ParseKo[A](message: String) extends ParseState[A]

object ParseState {
  def ok[A](input: String, value: A): ParseState[A] =
    ParseOk(input, value)

  def ko[A](message: String): ParseState[A] =
    ParseKo(message)

  def sequence[A](states: List[ParseState[A]]): ParseState[List[A]] =
    states match {
      case Nil =>
        ok[List[A]]("", Nil)
      case sh :: st => for {
        h <- sh
        t <- sequence(st)
      } yield h :: t
    }
}
