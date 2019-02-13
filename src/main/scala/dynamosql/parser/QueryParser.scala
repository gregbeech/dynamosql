package dynamosql.parser

import dynamosql.model._
import dynamosql.parser.rules.PathRules
import org.parboiled2._

class QueryParser(val input: ParserInput) extends Parser with PathRules {
  def bool = rule { capture(kw("true") | kw("false")) ~> (s => Value.Bool(s.toBoolean)) }
  def n = rule { capture(optional("-") ~ oneOrMore(CharPredicate.Digit) ~ optional("." ~ oneOrMore(CharPredicate.Digit)) ~ optional("E" ~ oneOrMore(CharPredicate.Digit))) ~> Value.N.apply _ }

  def qchar = rule { """\""" ~ anyOf("""'\""") }
  def pchar = rule { noneOf("""'\""") }
  def char = rule { qchar | pchar }
  def s = rule { "'" ~ capture(zeroOrMore(char)) ~ "'" ~> Value.S.unescape _ }

  def arg = rule { capture(":" ~ oneOrMore(CharPredicate.AlphaNum)) ~> Value.Arg.apply _ }
  def linearValue = rule { s | arg }
  def orderedValue = rule { s | n | arg }
  def value = rule { s | n | bool | arg }

  def operand = rule { arg | value | path }
  def orderedOperand = rule { arg | orderedValue | path }
  def linearOperand = rule { arg | linearValue | path }

  def eq = rule { "=" ~ owsp ~ operand ~> Eq.apply _ }
  def ne = rule { ("<>" | "!=") ~ owsp ~ operand ~> Ne.apply _ }
  def gt = rule { ">" ~ owsp ~ orderedOperand ~> Gt.apply _ }
  def lt = rule { "<" ~ owsp ~ orderedOperand ~> Lt.apply _ }
  def gte = rule { ">=" ~ owsp ~ orderedOperand ~> Gte.apply _ }
  def lte = rule { "<=" ~ owsp ~ orderedOperand ~> Lte.apply _ }
  def beginsWith = rule { kw("begins") ~ wsp ~ kw("with") ~ wsp ~ linearOperand ~> BeginsWith.apply _ }
  def between = rule { kw("between") ~ wsp ~ orderedOperand ~ wsp ~ kw("and") ~ wsp ~ orderedOperand ~> Between.apply _ }
  def contains = rule { kw("contains") ~ owsp ~ linearOperand ~> Contains.apply _ }
  def exists = rule { kw("exists") ~ push(Exists) }
  def notExists = rule { kw("not") ~ wsp ~ kw("exists") ~ push(NotExists) }

  def star = rule { str("*") ~> (() => Projection.allAttributes) }
  def count = rule { kw("count(*)") ~> (() => Projection.count) }
  def fields = rule { star | count | pathList }
  def select = rule { kw("select") ~ wsp ~ fields }

  def tableOrIndexName = rule { capture((3 to 255).times(CharPredicate.AlphaNum | anyOf("_-."))) }
  def index = rule { kw("index") ~ wsp ~ tableOrIndexName }
  def from = rule { kw("from") ~ wsp ~ tableOrIndexName ~ optional(wsp ~ index) ~> Table.apply _ }

  def skOp = rule { eq | gte | lte | gt | lt | beginsWith | between }
  def pkCondition = rule { path ~ owsp ~ eq ~> PartitionKeyCondition.apply _ }
  def skCondition = rule { path ~ owsp ~ skOp ~> SortKeyCondition.apply _ }
  def where = rule { kw("where") ~ wsp ~ pkCondition ~ optional(wsp ~ kw("and") ~ wsp ~ skCondition) ~> KeyCondition.apply _ }

  def filterOp = rule { eq | ne | gte | lte | gt | lt | beginsWith | between | contains | exists | notExists }
  def filterCondition = rule { path ~ owsp ~ filterOp ~> FilterCondition.apply _ }
  def filter = rule { kw("filter") ~ wsp ~ oneOrMore(filterCondition).separatedBy(wsp ~ kw("and") ~ wsp) ~> (xs => Condition.all(xs.toList)) }

  def query = rule { owsp ~ select ~ wsp ~ from ~ wsp ~ where ~ optional(wsp ~ filter) ~ owsp ~> Query.apply _ }
}
