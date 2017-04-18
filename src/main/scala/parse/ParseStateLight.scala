package ignore

sealed trait ParseState[A]
case class ParseOk[A](input: String, value: A) extends ParseState[A]
case class ParseKo[A](message: String) extends ParseState[A]
