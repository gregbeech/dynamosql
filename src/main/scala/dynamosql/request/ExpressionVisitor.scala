package dynamosql.request

import dynamosql.model._

object ExpressionVisitor {
  def visit(condition: Condition)(implicit context: SubstitutionContext): String = condition match {
    case c: AttributeCondition => visit(c)
    case AndCondition(left, right) => Seq(visit(left), visit(right)).map(s => s"($s)").mkString(" and ")
    case OrCondition(left, right) => Seq(visit(left), visit(right)).map(s => s"($s)").mkString(" or ")
  }

  private def visit(cond: AttributeCondition)(implicit context: SubstitutionContext): String = {
    val name = visit(cond.attrName)
    cond.operation match {
      case Eq(value) => s"$name = ${visit(value)}"
      case Ne(value) => s"$name <> ${visit(value)}"
      case Gt(value) => s"$name > ${visit(value)}"
      case Lt(value) => s"$name < ${visit(value)}"
      case Gte(value) => s"$name >= ${visit(value)}"
      case Lte(value) => s"$name <= ${visit(value)}"
      case BeginsWith(value) => s"$name begins_with(${visit(value)})"
      case Between(lower, upper) => s"$name between ${visit(lower)} and ${visit(upper)}"
      case Contains(value) => s"contains($name, ${visit(value)})"
      case Exists => s"attribute_exists($name)"
      case NotExists => s"attribute_not_exists($name)"
    }
  }

  private def visit(attrName: AttrName)(implicit context: SubstitutionContext): String = attrName match {
    case Name(name) => context.name(name)
    case Path(Name(name), segments) => (context.name(name) :: segments.map(visit)).mkString(".")
  }

  private def visit(segment: Segment)(implicit context: SubstitutionContext): String = segment match {
    case Name(name) => context.name(name)
    case Elem(index) => s"[$index]"
  }

  private def visit(value: Value)(implicit context: SubstitutionContext): String = context.value(value)
}
