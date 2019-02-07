package dynamosql.model

sealed trait Operation
sealed trait SortKeyOperation extends Operation
sealed trait PartitionKeyOperation extends SortKeyOperation
case class Eq(operand: Operand) extends PartitionKeyOperation
case class Ne(operand: Operand) extends SortKeyOperation
case class Gt(operand: Operand) extends SortKeyOperation
case class Lt(operand: Operand) extends SortKeyOperation
case class Gte(operand: Operand) extends SortKeyOperation
case class Lte(operand: Operand) extends SortKeyOperation
case class BeginsWith(operand: Operand) extends SortKeyOperation
case class Between(left: Operand, right: Operand) extends SortKeyOperation
case class Contains(operand: Operand) extends Operation
case object Exists extends Operation
case object NotExists extends Operation

sealed trait Condition
case class And(left: Condition, right: Condition) extends Condition
case class Or(left: Condition, right: Condition) extends Condition
case class Not(condition: Condition) extends Condition

sealed trait AttributeCondition extends Condition {
  val path: Path
  val operation: Operation
}
case class PartitionKeyCondition(path: Path, operation: PartitionKeyOperation) extends AttributeCondition
case class SortKeyCondition(path: Path, operation: SortKeyOperation) extends AttributeCondition
case class KeyCondition(partition: PartitionKeyCondition, sort: Option[SortKeyCondition]) extends Condition
case class FilterCondition(path: Path, operation: Operation) extends AttributeCondition

object Condition {
  def all(conditions: List[Condition]): Condition = conditions match {
    case Nil => throw new IllegalArgumentException("No conditions specified")
    case x :: Nil => x
    case x :: xs => And(x, all(xs))
  }

  def any(conditions: List[Condition]): Condition = conditions match {
    case Nil => throw new IllegalArgumentException("No conditions specified")
    case x :: Nil => x
    case x :: xs => Or(x, any(xs))
  }
}
