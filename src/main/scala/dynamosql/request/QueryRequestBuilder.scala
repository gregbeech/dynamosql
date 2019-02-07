package dynamosql.request

import dynamosql.model.{ParameterisedQuery, _}
import software.amazon.awssdk.services.dynamodb.model.QueryRequest

import scala.collection.JavaConverters._

object QueryRequestBuilder {
  def build(pq: ParameterisedQuery): QueryRequest = {
    implicit val context: SubstitutionContext = new SubstitutionContext()

    val keyCondition = ExpressionVisitor.visit(pq.query.where.toCondition)
    val filterCondition = pq.query.filter.map(ExpressionVisitor.visit)

    builder(pq.query.from)
      .keyConditionExpression(keyCondition)
      .filterExpression(filterCondition.orNull)
      .expressionAttributeNames(context.nameMap.asJava)
      .expressionAttributeValues(context.valueMap.mapValues(_.toAttributeValue(pq.args)).asJava)
      .build()
  }

  private def builder(from: From): QueryRequest.Builder = from match {
    case Table(tableName) => QueryRequest.builder.tableName(tableName)
    case Index(indexName, tableName) => QueryRequest.builder.tableName(tableName).indexName(indexName)
  }
}