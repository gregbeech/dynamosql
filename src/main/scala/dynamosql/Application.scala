package dynamosql

import java.net.URI

import cats.implicits._
import dynamosql.request._
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import scala.compat.java8.FutureConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Application {
  import dynamosql.syntax._

  case class CredentialId(get: String)
  case class RowVersion(get: Int)

  implicit val credentialIdInput: Input[CredentialId] = Input[String].contramap(id => s"Credential#${id.get}")
  implicit val rowVersionInput: Input[RowVersion] = Input[String].contramap(id => f"Version#${id.get}%07d")

  def main(args: Array[String]): Unit = {
    val id = CredentialId("6aa39188877008f8e2f576f559efe00f")
    val sk = RowVersion(0)
    val limit = 10
    val query = query"""
      SELECT *
      FROM identity.test.Entities
      WHERE Id = $id AND SK >= $sk
      FILTER Version = 1 AND GSI2PK CONTAINS 'fc4912035cf0c5eb'
      LIMIT $limit
    """

    val request = query.toRequest
    val client = DynamoDbAsyncClient.builder.endpointOverride(new URI("http://localhost:8000")).build
    val result = Await.result(client.query(request).toScala, Duration.Inf)
    println(result)
  }
}
