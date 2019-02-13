package dynamosql.parser.rules

import dynamosql.parser.ParserSpec

class BasicRulesSpec extends ParserSpec {

  "wsp" must {
    "consume whitespace including newlines" in {
      parser(" \n \t \r ").wsp.run() must consumeInput
    }
    "parse error on an empty string" in {
      parser("").wsp.run() must parseError
    }
  }

  "owsp" must {
    "consume whitespace including newlines" in {
      parser(" \n \t \r ").owsp.run() must consumeInput
    }
    "match an empty string" in {
      parser("").owsp.run() must consumeInput
    }
  }

}
