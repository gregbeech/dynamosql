package dynamosql.awssdk.v2

import dynamosql.awssdk.v2.syntax._
import dynamosql.expr.{ExpressionVisitor, SubstitutionContext}
import dynamosql.model.{ParameterisedQuery, _}
import software.amazon.awssdk.services.dynamodb.model.QueryRequest

import scala.collection.JavaConverters._

object QueryRequestBuilder {
  def build(pq: ParameterisedQuery): QueryRequest = {
    implicit val context: SubstitutionContext = new SubstitutionContext()

    val keyCondition = ExpressionVisitor.visit(pq.query.keyCondition)
    val filterCondition = pq.query.filterCondition.map(ExpressionVisitor.visit)

    builder(pq.query.table)
      .keyConditionExpression(keyCondition)
      .filterExpression(filterCondition.orNull)
      .expressionAttributeNames(context.nameMap.asJava)
      .expressionAttributeValues(context.valueMap.mapValues(_.toAttributeValue(pq.args)).asJava)
      .build()
  }

  private def builder(table: Table): QueryRequest.Builder = table match {
    case Table(name, Some(index)) => QueryRequest.builder.tableName(name).indexName(index)
    case Table(name, None) => QueryRequest.builder.tableName(name)
  }
}
