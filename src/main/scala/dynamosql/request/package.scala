package dynamosql

import com.amazonaws.services.dynamodbv2.model.QueryRequest

package object request {

  implicit class RichParameterisedQuery(val query: ParameterisedQuery) extends AnyVal {
    def toRequest: QueryRequest = QueryRequestBuilder.build(query)
  }

}
