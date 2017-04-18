package complete

import scala.util.{Try, Success, Failure}

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


/**
 * A parser is a function from an input string, to a parse result.
 */
case class Parser[A](run: String => ParseState[A]) {
  /**
   * Return a parser with the function `f` applied to the
   * output of that parser.
   */
  def map[B](f: A => B): Parser[B] =
    Parser(input => run(input).map(f))

  /**
   * Return a parser that feeds its input into this parser, and
   *
   * - if that parser succeeds, apply its result to function f, and
   *   then run the resultant parser with the updated input
   *
   * - if that parser fails with an error, return a parser with
   *   that error
   */
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(input => run(input) match {
      case ParseOk(rest, a) =>
        f(a).run(rest)
      case ParseKo(message) =>
        ParseKo(message)
    })

  /**
   * Anonymous flatMap.
   *
   * Return a parser that feeds its input into this parser, and
   *
   * - if that parser succeeds, run the next parser with the updated input
   *
   * - if that parser fails with an error, return a parser with that error
   */
  def >>>[B](parser: => Parser[B]): Parser[B] =
    flatMap(_ => parser)

  /**
   * Choice.
   *
   * Return a parser that tries the first parser for a successful value.
   *
   *  - if the first parser succeeds then use this parser
   *
   *  - if the second parser succeeds then try the second parser
   */
  def |||(f: => Parser[A]): Parser[A] =
    Parser(input => run(input) match {
      case ParseOk(rest, a) =>
        ParseOk(rest, a)
      case ParseKo(_) =>
        f.run(input)
    })
}

object Parser {
  /**
   * Return a parser that always succeeds with the given value
   * and consumes no input.
   */
  def value[A](a: A): Parser[A] =
    Parser(input => ParseState.ok(input, a))

  /**
   * Return a parser that always fails with the given message.
   */
  def failed[A](message: String): Parser[A] =
    Parser(_ => ParseState.ko(message))

  /**
   * Return a parser that succeeds with a character off the input
   * or fails if the input is empty.
   */
  def character: Parser[Char] =
    Parser(input => input.toList match {
      case Nil =>
        ParseState.ko("Not enough input to consume a character.")
      case h::t =>
        ParseState.ok(t.mkString, h)
    })

  /**
   * Return a parser that continues producing a list of values from the
   * given parser.
   */
  def list[A](parser: Parser[A]): Parser[List[A]] =
    list1(parser) ||| value(Nil)

  /**
   * Return a parser that produces at least one value from the
   * given parser then continues producing a list of values from
   * the given parser (to ultimately produce a non-empty list).
   */
  def list1[A](parser: Parser[A]): Parser[List[A]] = for {
    h <- parser
    t <- list(parser)
  } yield h :: t

  /**
   * Return a parser that produces a character but fails if
   *
   *  - The input is empty, or
   *
   *  - The character does not satisfy the given predicate
   */
  def satisfy(pred: Char => Boolean): Parser[Char] =
    character.flatMap(c =>
      if (pred(c))
        value(c)
      else
        failed("Input failed to match predicate."))


  /**
   * Return a parser that produces a character but fails if
   *
   *  - The input is empty, or
   *
   *  - The character does not match the given character
   */
  def is(char: Char): Parser[Char] =
    satisfy(_ == char)

  /**
   * Return a parser that produces a character between '0' and '9'
   * but fails if
   *
   *  - The input is empty, or
   *
   *  - The produced character is not a digit
   */
  def digit: Parser[Char] =
    satisfy(_.isDigit)

  /**
   * Return a parser that produces zero or a positive integer but fails if
   *
   *  - The input is empty, or
   *
   *  - The input does not produce a value series of digits
   */
  def natural: Parser[Int] =
    list1(digit).flatMap(digits =>
      Try(digits.mkString.toInt) match {
        case Success(n) =>
          value(n)
        case Failure(e) =>
          failed(e.toString)
      })

  /**
   * Return a parser that produces a space character but fails if
   *
   *  - The input is empty, or
   *
   *  - The produced character is not a space
   */
  def space: Parser[Char] =
    is(' ')

  /**
   * Return a parse that produces one of more space characters
   * (consuming until the first non-space) but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not a space
   */
  def spaces1: Parser[String] =
    list1(space).map(_.mkString)

  /**
   * Return a parser that produces a lower-case character but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not lower-case
   */
  def lower: Parser[Char] =
    satisfy(_.isLower)

  /**
   * Return a parser that produces an upper-case character but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not upper-case
   */
  def upper: Parser[Char] =
    satisfy(_.isUpper)

  /**
   * Return a parser that produces an alpha character but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not alpha
   */
  def alpha: Parser[Char] =
    satisfy(_.isLetter)


  def runOn[A](parser: Parser[A], data: List[String]): Either[String, List[A]] =
    ParseState.sequence(data.map(parser.run)) match {
      case ParseKo(message) =>
        Left(message)
      case ParseOk(_, a) =>
        Right(a)
    }
}

/**
 * *Challenge* Parse a naive personel record.
 *
 * We have a set of personel records with a "special" format.
 *
 * Produce a person parser for a record.
 */
object PersonParser {
  /*
   * A data structure representing a person with the following attributes:
   *  - name: non empty string that starts with a capital letter
   *  - age: positive integer
   *  - address: non empty string that starts with a capital letter
   *  - phone: string of digits, dots or hyphens that must start with a
   *           digit and end with a hash (#)
   */
  case class Person(name: String, age: Int, phone: String, address: Address)

  /*
   * A data structure representing an address with the following attributes:
   *  - number: positive integer
   *  - street: non empty string
   */
  case class Address(number: Int, street: String)

  /**
   * Parse a name, which is a non-empty string that starts with a capital letter.
   */
  def nameParser: Parser[String] = for {
    u <- Parser.upper
    r <- Parser.list(Parser.lower)
  } yield (u :: r).mkString


  /**
   * Parse a phone number, which is a string of digits, dots or hyphens that
   * starts with a digit and ends with a hash (#).
   */
  def phoneParser: Parser[String] = for {
    a <- Parser.digit
    b <- Parser.list(Parser.digit ||| Parser.is('.') ||| Parser.is('-'))
    c <- Parser.is('#')
  } yield a.toString + b.mkString + c.toString

  /**
   * An address is a positive street number and a non empty string for the
   * street name.
   */
  def addressParser: Parser[Address] = for {
    n <- Parser.natural
    _ <- Parser.space
    s <- Parser.list1(Parser.alpha)
  } yield Address(n, s.mkString)

  /**
   * An person record is the following parts, each seperated by one or more spaces.
   *
   *  <name> <age> <phone> <address>
   *
   * scala> PersonParser.personParser.run("Homer 39 555.123.939# 742 evergreen")
   *  = Ok(ParseState(,Person(Homer,39,555.123.939#,Address(742,evergreen))))
   */
  def personParser: Parser[Person] = for {
    n <- nameParser
    _ <- Parser.spaces1
    a <- Parser.natural
    _ <- Parser.spaces1
    p <- phoneParser
    _ <- Parser.spaces1
    x <- addressParser
  } yield Person(n, a, p, x)

  def Data = List(
    "Fred 32 123.456-1213# 301 cobblestone"
  , "Barney 31 123.456.1214# 303 cobblestone"
  , "Homer 39 555.123.939# 742 evergreen"
  , "Flanders 39 555.123.939# 744 evergreen"
  )
}
