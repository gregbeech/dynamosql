package dynamosql.model

import java.nio.ByteBuffer

sealed trait AttrName
sealed trait Segment
case class Name(get: String) extends AttrName with Segment
case class Elem(index: Int) extends Segment
case class Path(name: Name, segments: List[Segment]) extends AttrName

sealed trait Value
sealed trait OrderedValue extends Value
sealed trait LinearValue extends OrderedValue

object Value {
  case class Binary(get: ByteBuffer) extends LinearValue // not 'B' as that crosses over with type params
  case class BOOL(get: Boolean) extends Value
  case class N(get: String) extends OrderedValue
  case class S(get: String) extends LinearValue
  case class SS(get: Set[String]) extends Value

  case class Arg(name: String) extends Value
}
