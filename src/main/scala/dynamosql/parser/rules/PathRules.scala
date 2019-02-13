package dynamosql.parser.rules

import dynamosql.model.{Index, Name, Path, Projection}
import org.parboiled2.{CharPredicate, Parser}

trait PathRules extends BasicRules { this: Parser =>
  def name = rule { capture((1 to 255).times(noneOf(":#.[], \t\r\n"))) ~> Name.apply _ }
  def nameSegment = rule { "." ~ name }
  def indexSegment = rule { "[" ~ capture(oneOrMore(CharPredicate.Digit)) ~ "]" ~> (s => Index(s.toInt)) }
  def segment = rule { nameSegment | indexSegment }
  def path = rule { name ~ optional((1 to 32).times(segment)) ~> ((n, s) => s match {
    case Some(ss) => Path(n, ss.toList)
    case None => Path(n)
  }) }
  def pathList = rule { oneOrMore(path).separatedBy(owsp ~ "," ~ owsp) ~> Projection.specificAttributes _ }
}
