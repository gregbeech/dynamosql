package dynamosql.model

sealed trait Operation
sealed trait UnaryOperation extends Operation
sealed trait BinaryOperation extends Operation {
  val operand: Value
}
sealed trait TernaryOperation extends Operation {
  val left: Value
  val right: Value
}
sealed trait PartitionKeyOperation extends Operation
sealed trait SortKeyOperation extends Operation
case class Eq(operand: Value) extends BinaryOperation with PartitionKeyOperation with SortKeyOperation
case class Ne(operand: Value) extends BinaryOperation with SortKeyOperation
case class Gt(operand: Value) extends BinaryOperation with SortKeyOperation
case class Lt(operand: Value) extends BinaryOperation with SortKeyOperation
case class Gte(operand: Value) extends BinaryOperation with SortKeyOperation
case class Lte(operand: Value) extends BinaryOperation with SortKeyOperation
case class BeginsWith(operand: Value) extends BinaryOperation with SortKeyOperation
case class Between(left: Value, right: Value) extends TernaryOperation with SortKeyOperation
case class Contains(operand: Value) extends BinaryOperation
case object Exists extends UnaryOperation
case object NotExists extends UnaryOperation

sealed trait Condition
sealed trait AttributeCondition extends Condition {
  val attrName: AttrName
  val operation: Operation
}
case class PartitionKeyCondition(attrName: AttrName, operation: PartitionKeyOperation) extends AttributeCondition
case class SortKeyCondition(attrName: AttrName, operation: SortKeyOperation) extends AttributeCondition
case class FilterCondition(attrName: AttrName, operation: Operation) extends AttributeCondition
case class AndCondition(left: Condition, right: Condition) extends Condition
case class OrCondition(left: Condition, right: Condition) extends Condition

object Condition {
  def all(conditions: List[Condition]): Condition = conditions match {
    case Nil => throw new IllegalArgumentException("No conditions specified")
    case x :: Nil => x
    case x :: xs => AndCondition(x, all(xs))
  }

  def any(conditions: List[Condition]): Condition = conditions match {
    case Nil => throw new IllegalArgumentException("No conditions specified")
    case x :: Nil => x
    case x :: xs => OrCondition(x, any(xs))
  }
}
