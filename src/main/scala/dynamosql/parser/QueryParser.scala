package dynamosql.parser

import dynamosql.model._
import org.parboiled2._

trait WhitespaceRules { this: Parser =>
  def wsp = rule { oneOrMore(anyOf(" \t\r\n")) }
  def owsp = rule { zeroOrMore(anyOf(" \t\r\n")) }
}

trait PathRules extends WhitespaceRules { this: Parser =>
  def name = rule { capture((1 to 255).times(noneOf(":#.[], \t\r\n"))) ~> Name.apply _ }
  def nameSegment = rule { "." ~ name }
  def indexSegment = rule { "[" ~ capture(oneOrMore(CharPredicate.Digit)) ~ "]" ~> (s => Index(s.toInt)) }
  def segment = rule { nameSegment | indexSegment }
  def path = rule { name ~ zeroOrMore(segment) ~> ((n, ss) => Path(n, ss.toList)) }
  def pathList = rule { oneOrMore(path).separatedBy(owsp ~ "," ~ owsp) ~> Projection.specificAttributes _ }
}

class QueryParser(val input: ParserInput) extends Parser with PathRules {
  def BOOL = rule { capture("true" | "false") ~> (s => Value.Bool(s.toBoolean)) }
  def N = rule { capture(optional("-") ~ oneOrMore(CharPredicate.Digit) ~ optional("." ~ oneOrMore(CharPredicate.Digit)) ~ optional("E" ~ oneOrMore(CharPredicate.Digit))) ~> Value.N.apply _ }
  def S = rule { "'" ~ capture(oneOrMore(noneOf("'"))) ~ "'" ~> Value.S.apply _ }
  def value = rule { S | N | BOOL | arg }
  def orderedValue = rule { S | N | arg }
  def linearValue = rule { S | arg }

  def arg = rule { capture(":" ~ oneOrMore(CharPredicate.AlphaNum)) ~> Value.Arg.apply _ }
  def operand = rule { arg | value | path }
  def orderedOperand = rule { arg | orderedValue | path }
  def linearOperand = rule { arg | linearValue | path }

  def eq = rule { "=" ~ owsp ~ operand ~> Eq.apply _ }
  def ne = rule { ("<>" | "!=") ~ owsp ~ operand ~> Ne.apply _ }
  def gt = rule { ">" ~ owsp ~ orderedOperand ~> Gt.apply _ }
  def lt = rule { "<" ~ owsp ~ orderedOperand ~> Lt.apply _ }
  def gte = rule { ">=" ~ owsp ~ orderedOperand ~> Gte.apply _ }
  def lte = rule { "<=" ~ owsp ~ orderedOperand ~> Lte.apply _ }
  def beginsWith = rule { "BEGINS WITH" ~ wsp ~ linearOperand ~> BeginsWith.apply _ }
  def between = rule { "BETWEEN" ~ wsp ~ orderedOperand ~ wsp ~ "AND" ~ wsp ~ orderedOperand ~> Between.apply _ }
  def contains = rule { "CONTAINS" ~ owsp ~ linearOperand ~> Contains.apply _ }
  def exists = rule { str("EXISTS") ~> (() => Exists) }
  def notExists = rule { str("NOT EXISTS") ~> (() => NotExists) }

  def star = rule { str("*") ~> (() => Projection.allAttributes) }
  def count = rule { str("COUNT(*)") ~> (() => Projection.count) }
  def fields = rule { star | count | pathList }
  def select = rule { "SELECT" ~ wsp ~ fields }

  def tableOrIndexName = rule { capture((3 to 255).times(CharPredicate.AlphaNum | anyOf("_-."))) }
  def index = rule { "INDEX" ~ wsp ~ tableOrIndexName }
  def from = rule { "FROM" ~ wsp ~ tableOrIndexName ~ optional(wsp ~ index) ~> Table.apply _ }

  def skOp = rule { eq | gte | lte | gt | lt | beginsWith | between }
  def pkCondition = rule { path ~ owsp ~ eq ~> PartitionKeyCondition.apply _ }
  def skCondition = rule { path ~ owsp ~ skOp ~> SortKeyCondition.apply _ }
  def where = rule { "WHERE" ~ wsp ~ pkCondition ~ optional(wsp ~ "AND" ~ wsp ~ skCondition) ~> KeyCondition.apply _ }

  def filterOp = rule { eq | ne | gte | lte | gt | lt | beginsWith | between | contains | exists | notExists }
  def filterCondition = rule { path ~ owsp ~ filterOp ~> FilterCondition.apply _ }
  def filter = rule { "FILTER" ~ wsp ~ oneOrMore(filterCondition).separatedBy(wsp ~ "AND" ~ wsp) ~> (xs => Condition.all(xs.toList)) }

  def query = rule { owsp ~ select ~ wsp ~ from ~ wsp ~ where ~ optional(wsp ~ filter) ~ owsp ~> Query.apply _ }
}
