package dynamosql

import java.net.URI
import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.implicits._
import dynamosql.syntax._
import dynamosql.awssdk.v2.syntax._
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import scala.compat.java8.FutureConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class CredentialId(get: String)
case class RowVersion(get: Int)

object Application {
  implicit val credentialIdInput: Input[CredentialId] = Input[String].contramap(id => s"Credential#${id.get}")
  implicit val rowVersionInput: Input[RowVersion] = Input[String].contramap(v => f"Version#${v.get}%07d")
  implicit val instantInput: Input[Instant] = Input[Long].contramap(_.getEpochSecond)

  def main(args: Array[String]): Unit = {
    val id = CredentialId("223999b6dc3a7355a4f5b5e597b1cca9")
    val sk = RowVersion(0)
    val since = Instant.now.minus(14, ChronoUnit.DAYS)
    val limit = 10
    val query = query"""
      select *
      from identity.test.Entities
      where Id = $id and SK >= $sk
      filter CreatedAt > $since
      limit $limit
    """

    val request = query.toRequest
    val client = DynamoDbAsyncClient.builder.endpointOverride(new URI("http://localhost:8000")).build
    val result = Await.result(client.query(request).toScala, Duration.Inf)
    println(result)
  }
}
