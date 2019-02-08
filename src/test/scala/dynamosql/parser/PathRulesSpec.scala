package dynamosql.parser

import dynamosql.model.Name

class PathRulesSpec extends ParserSpec {

  "name" must {
    "match any unreserved Unicode characters" in {
      parser("Test«ταБЬℓσ»").name.run() must produce(Name("Test«ταБЬℓσ»"))
    }
    "match a maximum of 255 characters" in {
      parser("a" * 256).name.run() must produce(Name("a" * 255))
    }
    "parse error on an empty string" in {
      parser("").name.run() must parseError
    }
    "not match '#' or ':' which are reserved characters" in {
      parser("Foo#").name.run() must produce(Name("Foo"))
      parser("Foo:").name.run() must produce(Name("Foo"))
    }
    "not match '.', '[' or ']' as it would make path matching ambiguous" in {
      parser("Foo.").name.run() must produce(Name("Foo"))
      parser("Foo[").name.run() must produce(Name("Foo"))
      parser("Foo]").name.run() must produce(Name("Foo"))
    }
    "not match ',' or whitespace as it would make path list matching ambiguous" in {
      parser("Foo,").name.run() must produce(Name("Foo"))
      parser("Foo ").name.run() must produce(Name("Foo"))
      parser("Foo\t").name.run() must produce(Name("Foo"))
      parser("Foo\r").name.run() must produce(Name("Foo"))
      parser("Foo\n").name.run() must produce(Name("Foo"))
    }
  }

}
