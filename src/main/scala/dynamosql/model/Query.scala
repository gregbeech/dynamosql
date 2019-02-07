package dynamosql.model

case class Where(pkCondition: AttributeCondition, skCondition: Option[AttributeCondition]) {
  def toCondition: Condition = skCondition match {
    case Some(cond) => AndCondition(pkCondition, cond)
    case None => pkCondition
  }
}

case class Query(select: Select, from: From, where: Where, filter: Option[Condition])
