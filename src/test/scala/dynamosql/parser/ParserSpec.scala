package dynamosql.parser

import org.scalatest.matchers.Matcher
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.{Failure, Success}

trait ParserSpec extends WordSpec with MustMatchers {
  def parser(s: String): QueryParser = new QueryParser(s)
  def consumeInput: Matcher[Any] = matchPattern { case Success(()) => }
  def produce[A](a: A): Matcher[Any] = matchPattern { case Success(`a`) => }
  def parseError: Matcher[Any] = matchPattern { case Failure(_) => }
}
