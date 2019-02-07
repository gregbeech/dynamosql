package dynamosql.awssdk.v2

import dynamosql.model.{ParameterisedQuery, Value}
import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, QueryRequest}

object syntax {
  implicit class RichParameterisedQuery(private val query: ParameterisedQuery) extends AnyVal {
    def toRequest: QueryRequest = QueryRequestBuilder.build(query)
  }
  
  implicit class RichValue(private val value: Value) extends AnyVal {
    def toAttributeValue(args: Map[String, Value] = Map.empty): AttributeValue = AttributeValueBuilder.build(value, args)
  }
}
