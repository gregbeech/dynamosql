package dynamosql.parser

import dynamosql.model._
import org.parboiled2._

class QueryParser(val input: ParserInput) extends Parser {
  def wsp = rule { oneOrMore(anyOf(" \t\r\n")) }
  def owsp = rule { zeroOrMore(anyOf(" \t\r\n")) }

  def simpleName = rule { capture(oneOrMore(noneOf(", .[]"))) ~> Name.apply _ }
  def mapSelector = rule { "." ~ simpleName }
  def listSelector = rule { "[" ~ capture(oneOrMore(CharPredicate.Digit)) ~ "]" ~> (s => Elem(s.toInt)) }
  def selector = rule { mapSelector | listSelector }
  def attrName = rule { simpleName ~ zeroOrMore(selector) ~> ((n, ss) => if (ss.isEmpty) n else Path(n, ss.toList)) }

  def attrBOOL = rule { capture("true" | "false") ~> (s => Value.BOOL(s.toBoolean)) }
  def attrN = rule { capture(optional("-") ~ oneOrMore(CharPredicate.Digit) ~ optional("." ~ oneOrMore(CharPredicate.Digit)) ~ optional("E" ~ oneOrMore(CharPredicate.Digit))) ~> Value.N.apply _ }
  def attrS = rule { "'" ~ capture(oneOrMore(noneOf("'"))) ~ "'" ~> Value.S.apply _ }
  def attrOrdered = rule { attrS | attrN | arg }
  def attrScannable = rule { attrS | arg }
  def attrValue = rule { attrS | attrN | attrBOOL | arg }

  def arg = rule { capture(":" ~ oneOrMore(CharPredicate.AlphaNum)) ~> Value.Arg.apply _ }

  def star = rule { str("*") ~> (() => Select.allAttributes) }
  def countStar = rule { str("COUNT(*)") ~> (() => Select.count) }
  def selectorList = rule { oneOrMore(attrName).separatedBy(owsp ~ "," ~ owsp) ~> Select.specificAttributes _ }
  def fields = rule { star | countStar | selectorList }
  def select = rule { "SELECT" ~ wsp ~ fields }

  def tableOrIndexName: Rule1[String] = rule { capture((3 to 255).times(CharPredicate.AlphaNum | anyOf("_-."))) }
  def index: Rule1[String] = rule { "INDEX" ~ wsp ~ tableOrIndexName }
  def from: Rule1[From] = rule { "FROM" ~ wsp ~ tableOrIndexName ~ optional(wsp ~ index) ~> From.apply _ }

  def eq = rule { "=" ~ owsp ~ attrValue ~> Eq.apply _ }
  def ne = rule { ("<>" | "!=") ~ owsp ~ attrValue ~> Ne.apply _ }
  def gt = rule { ">" ~ owsp ~ attrOrdered ~> Gt.apply _ }
  def lt = rule { "<" ~ owsp ~ attrOrdered ~> Lt.apply _ }
  def gte = rule { ">=" ~ owsp ~ attrOrdered ~> Gte.apply _ }
  def lte = rule { "<=" ~ owsp ~ attrOrdered ~> Lte.apply _ }
  def beginsWith = rule { "BEGINS WITH" ~ wsp ~ attrOrdered ~> BeginsWith.apply _ }
  def between = rule { "BETWEEN" ~ wsp ~ attrOrdered ~ wsp ~ "AND" ~ wsp ~ attrOrdered ~> Between.apply _ }
  def contains = rule { "CONTAINS" ~ owsp ~ attrOrdered ~> Contains.apply _ }
  def exists = rule { str("EXISTS") ~> (() => Exists) }
  def notExists = rule { str("NOT EXISTS") ~> (() => NotExists) }

  def skOp = rule { eq | gt | lt | gte | lte | beginsWith | between }
  def pkCondition = rule { attrName ~ owsp ~ eq ~> PartitionKeyCondition.apply _ }
  def skCondition = rule { attrName ~ owsp ~ skOp ~> SortKeyCondition.apply _ }
  def where = rule { "WHERE" ~ wsp ~ pkCondition ~ optional(wsp ~ "AND" ~ wsp ~ skCondition) ~> Where.apply _ }

  def filterOp = rule { eq | ne | gt | lt | gte | lte | beginsWith | between | contains | exists | notExists }
  def filterCondition = rule { attrName ~ owsp ~ filterOp ~> FilterCondition.apply _ }
  def filter = rule { "FILTER" ~ wsp ~ oneOrMore(filterCondition).separatedBy(wsp ~ "AND" ~ wsp) ~> (xs => Condition.all(xs.toList)) }

  def query: Rule1[Query] = rule { owsp ~ select ~ wsp ~ from ~ wsp ~ where ~ optional(wsp ~ filter) ~ owsp ~> Query.apply _ }
}
