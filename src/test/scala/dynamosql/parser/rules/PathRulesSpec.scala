package dynamosql.parser.rules

import dynamosql.model.{Index, Name, Path}
import dynamosql.parser.ParserSpec

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

  "segment" must {
    "match a name with a '.' prefix" in {
      parser(".Test«ταБЬℓσ»").segment.run() must produce(Name("Test«ταБЬℓσ»"))
    }
    "match a non-negative integer index surrounded by square brackets" in {
      parser("[0]").segment.run() must produce(Index(0))
      parser("[2147483647]").segment.run() must produce(Index(2147483647))
    }
    "not match a negative index" in {
      parser("[-1]").segment.run() must parseError
    }
    "not match non-integer index" in {
      parser("[1.2]").segment.run() must parseError
    }
    "not match whitespace inside the square brackets" in {
      parser("[0 ]").segment.run() must parseError
      parser("[ 0]").segment.run() must parseError
    }
  }

  "path" must {
    "match a path with a name only" in {
      parser("Test«ταБЬℓσ»").path.run() must produce(Path(Name("Test«ταБЬℓσ»")))
    }
    "match a path with name segments" in {
      parser("Foo.Bar.Baz").path.run() must produce(Path(Name("Foo"), List(Name("Bar"), Name("Baz"))))
    }
    "match a path with index segments" in {
      parser("Foo[1][234]").path.run() must produce(Path(Name("Foo"), List(Index(1), Index(234))))
    }
    "match a path with name and index segments" in {
      parser("Foo[1].Bar[234].Baz").path.run() must produce(Path(Name("Foo"), List(Index(1), Name("Bar"), Index(234), Name("Baz"))))
    }
    "match at most 32 segments following the initial name" in {
      val path = (1 to 35).mkString(".", ".", "")
      val segments = (1 to 32).map(n => Name(n.toString)).toList
      parser(s"Foo$path").path.run()must produce(Path(Name("Foo"), segments))
    }
  }

}
