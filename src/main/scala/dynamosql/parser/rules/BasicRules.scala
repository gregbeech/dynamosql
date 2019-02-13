package dynamosql.parser.rules

import org.parboiled2.Parser

trait BasicRules { this: Parser =>
  def wsp = rule { oneOrMore(anyOf(" \t\r\n")) }
  def owsp = rule { zeroOrMore(anyOf(" \t\r\n")) }
  def kw(s: String) = rule { ignoreCase(s) }
}
