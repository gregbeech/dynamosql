package dynamosql.model

sealed trait Operation
sealed trait PartitionKeyOperation extends Operation
sealed trait SortKeyOperation extends Operation
case class Eq(operand: Value) extends PartitionKeyOperation with SortKeyOperation
case class Ne(operand: Value) extends SortKeyOperation
case class Gt(operand: Value) extends SortKeyOperation
case class Lt(operand: Value) extends SortKeyOperation
case class Gte(operand: Value) extends SortKeyOperation
case class Lte(operand: Value) extends SortKeyOperation
case class BeginsWith(operand: Value) extends SortKeyOperation
case class Between(lowerBound: Value, upperBound: Value) extends SortKeyOperation
case class Contains(operand: Value) extends Operation
case class NotContains(operand: Value) extends Operation
case object Exists extends Operation
case object NotExists extends Operation

sealed trait Condition {
  val attrName: AttrName
  val operation: Operation
}
case class PartitionKeyCondition(attrName: AttrName, operation: PartitionKeyOperation) extends Condition
case class SortKeyCondition(attrName: AttrName, operation: SortKeyOperation) extends Condition
case class FilterCondition(attrName: AttrName, operation: Operation) extends Condition
