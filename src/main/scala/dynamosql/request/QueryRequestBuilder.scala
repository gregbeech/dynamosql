package dynamosql.request

import com.amazonaws.services.dynamodbv2.model.QueryRequest
import com.amazonaws.services.dynamodbv2.xspec.ExpressionSpecBuilder
import dynamosql.model.{From, Index, Query, Table}

object QueryRequestBuilder {

  def build(query: Query): QueryRequest = {
    val expr = new ExpressionSpecBuilder()



    val request = initRequest(query.from)



    request
  }

  private def initRequest(from: From): QueryRequest = from match {
    case Table(tableName) => new QueryRequest(tableName)
    case Index(indexName, tableName) => new QueryRequest(tableName).withIndexName(indexName)
  }



}
