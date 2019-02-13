package dynamosql.parser

import dynamosql.model.Value.{Bool, N, S}

class QueryParserSpec extends ParserSpec {

  "bool" must {
    "match true" in {
      parser("true").bool.run() must produce(Bool(true))
    }
    "match false" in {
      parser("false").bool.run() must produce(Bool(false))
    }
    "be case insensitive" in {
      parser("TrUe").bool.run() must produce(Bool(true))
      parser("FaLsE").bool.run() must produce(Bool(false))
    }
  }

  "n" must {
    "match non-negative long integers" in {
      parser("0").n.run() must produce(N("0"))
      parser("123").n.run() must produce(N("123"))
      parser("9223372036854775807").n.run() must produce(N("9223372036854775807"))
    }
    "match negative long integers" in {
      parser("-123").n.run() must produce(N("-123"))
      parser("-9223372036854775808").n.run() must produce(N("-9223372036854775808"))
    }
    "match floating point numbers" in {
      parser("0.0").n.run() must produce(N("0.0"))
      parser("1.23").n.run() must produce(N("1.23"))
      parser("92233720.36854775807").n.run() must produce(N("92233720.36854775807"))
    }
    "match negative floating point numbers" in {
      parser("-1.23").n.run() must produce(N("-1.23"))
      parser("-92233720.36854775808").n.run() must produce(N("-92233720.36854775808"))
    }
    "match floating point numbers in exponential notation" in {
      parser("0.0E6").n.run() must produce(N("0.0E6"))
      parser("-1.23E76").n.run() must produce(N("-1.23E76"))
      parser("92233720.36854775807E384").n.run() must produce(N("92233720.36854775807E384"))
    }
  }

  "s" must {
    "match strings contained in single quotes" in {
      parser("'hello world'").s.run() must produce(S("hello world"))
    }
    "match and unescape strings with escaped single quotes" in {
      parser("""'hello \'world\''""").s.run() must produce(S("hello 'world'"))
    }
    "match and unescape strings with escaped backslashes" in {
      parser("""'hello \\world\\'""").s.run() must produce(S("""hello \world\"""))
    }
  }

}
