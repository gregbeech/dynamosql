package dynamosql.model

import java.nio.ByteBuffer

sealed trait Operand

sealed trait Segment
case class Name(get: String) extends Segment
case class Elem(index: Int) extends Segment
case class Path(name: Name, segments: List[Segment] = List.empty) extends Operand

sealed trait Value extends Operand
sealed trait OrderedValue extends Value
sealed trait LinearValue extends OrderedValue

object Value {
  case class Arg(name: String) extends Value
  case class Binary(get: ByteBuffer) extends LinearValue // not 'B' as it conflicts with type params
  case class Bool(get: Boolean) extends Value
  case class N(get: String) extends OrderedValue
  case class S(get: String) extends LinearValue
  case class SS(get: Set[String]) extends Value
}
