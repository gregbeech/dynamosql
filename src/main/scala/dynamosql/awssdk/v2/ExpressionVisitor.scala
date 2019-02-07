package dynamosql.awssdk.v2

import dynamosql.model._

object ExpressionVisitor {
  def visit(condition: Condition)(implicit context: SubstitutionContext): String = condition match {
    case cond: AttributeCondition => visit(cond)
    case KeyCondition(partition, Some(sort)) => visit(And(partition, sort))
    case KeyCondition(partition, None) => visit(partition)
    case And(left, right) => s"(${visit(left)}) and (${visit(right)})"
    case Or(left, right) => s"(${visit(left)}) or (${visit(right)})"
    case Not(cond) => s"not (${visit(cond)})"
  }

  private def visit(cond: AttributeCondition)(implicit context: SubstitutionContext): String = {
    val path = visit(cond.path)
    cond.operation match {
      case Eq(operand) => s"$path = ${visit(operand)}"
      case Ne(operand) => s"$path <> ${visit(operand)}"
      case Gt(operand) => s"$path > ${visit(operand)}"
      case Lt(operand) => s"$path < ${visit(operand)}"
      case Gte(operand) => s"$path >= ${visit(operand)}"
      case Lte(operand) => s"$path <= ${visit(operand)}"
      case BeginsWith(operand) => s"$path begins_with(${visit(operand)})"
      case Between(lower, upper) => s"$path between ${visit(lower)} and ${visit(upper)}"
      case Contains(operand) => s"contains($path, ${visit(operand)})"
      case Exists => s"attribute_exists($path)"
      case NotExists => s"attribute_not_exists($path)"
    }
  }

  private def visit(operand: Operand)(implicit context: SubstitutionContext): String = operand match {
    case path: Path => visit(path)
    case value: Value => visit(value)
  }

  private def visit(path: Path)(implicit context: SubstitutionContext): String =
    (context.name(path.name.get) :: path.segments.map(visit)).mkString(".")

  private def visit(segment: Segment)(implicit context: SubstitutionContext): String = segment match {
    case Name(name) => context.name(name)
    case Elem(index) => s"[$index]"
  }

  private def visit(value: Value)(implicit context: SubstitutionContext): String = context.value(value)
}
