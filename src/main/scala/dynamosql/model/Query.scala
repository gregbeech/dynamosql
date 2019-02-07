package dynamosql.model

case class Table(name: String, index: Option[String] = None)

case class Query(projection: Projection, table: Table, keyCondition: KeyCondition, filterCondition: Option[Condition])

case class ParameterisedQuery(query: Query, args: Map[String, Value])
