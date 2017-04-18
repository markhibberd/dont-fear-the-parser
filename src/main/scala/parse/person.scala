package x

/**
 * Challenge* Parse an enterprise personel record.
 *
 * We have a set of personel records with a "special" format.
 *
 *   Fred 32 123.456-1213# 301 cobblestone
 *   Barney 31 123.456.1214# 303 cobblestone
 *   Homer 39 555.123.939# 742 evergreen
 *   Flanders 39 555.123.939# 744 evergreen
 *
 * Lets produce a person parser for a record.
 */
object Enterprise {
  val p = Parser

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
    capital <- p.upper
    rest <- p.list(p.alpha)
  } yield capital.toString + rest.mkString

  /**
   * Parse an age name, which is a positive integer.
   */
  def ageParser: Parser[Int] =
    p.natural

  /**
   * Parse a phone number, which is a string of digits, dots or hyphens that
   * starts with a digit and ends with a hash (#).
   */
  def phoneParser: Parser[String] = for {
    a <- p.digit
    m <- p.list(p.digit ||| p.is('.') ||| p.is('-'))
    x <- p.is('#')
  } yield a.toString + m.mkString + x.toString

  /**
   * An address is a positive street number and a non empty string for the
   * street name.
   */
  def addressParser: Parser[Address] = for {
    n <- p.natural
    _ <- p.spaces1
    a <- p.list1(p.alpha)
  } yield Address(n, a.mkString)


  /**
   * An person record is the following parts, each seperated by one or more spaces.
   *
   *  <name> <age> <phone> <address>
   *
   * scala> PersonParser.personParser.run("Homer 39 555.123.939# 742 evergreen")
   *  = Ok(ParseState(,Person(Homer,39,555.123.939#,Address(742,evergreen))))
   */
  def personParser[A](sep: Parser[A]): Parser[Person] = for {
    name <- nameParser
    _ <- sep
    age <- ageParser
    _ <- sep
    phone <- phoneParser
    _ <- sep
    address <- addressParser
  } yield Person(name, age, phone, address)

  def personParserSpaces =
    personParser(p.spaces1)

  def personParserComma =
    personParser(p.is(','))

  def json = for {
    _ <- p.is('{')
  } yield x

  def Data = List(
    "Fred 32 123.456-1213# 301 cobblestone"
  , "Barney 31 123.456.1214# 303 cobblestone"
  , "Homer 39 555.123.939# 742 evergreen"
  , "Flanders 39 555.123.939# 744 evergreen"
  )
}
