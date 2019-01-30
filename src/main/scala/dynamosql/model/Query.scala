package dynamosql.model

case class Where(pkCondition: Condition, skCondition: Option[Condition])
case class Filter(conditions: Seq[Condition])
case class Query(select: Select, from: From, where: Where, filter: Option[Filter])
