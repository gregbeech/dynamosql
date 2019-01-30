package dynamosql.model

sealed trait Operation
case class Eq(operand: Value) extends Operation
case class Ne(operand: Value) extends Operation
case class Gt(operand: Value) extends Operation
case class Lt(operand: Value) extends Operation
case class Gte(operand: Value) extends Operation
case class Lte(operand: Value) extends Operation
case class BeginsWith(operand: Value) extends Operation
case class Between(lowerBound: Value, upperBound: Value) extends Operation
case class Contains(operand: Value) extends Operation
case class NotContains(operand: Value) extends Operation
case object Exists extends Operation
case object NotExists extends Operation

case class Condition(attrName: AttrName, operation: Operation)
