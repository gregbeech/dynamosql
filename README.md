# DynamoSQL

A very early PoC which will, in all likelihood, never become more than that as I'll probably never have the time to either complete it or support it.

The idea is that it lets you create DynamoDB requests using a SQL-like dialect and use typeclasses to format the parameter values, something like this:

```scala
case class WidgetId(get: String)
case class RowVersion(get: Int)

object Application {
  implicit val widgetIdInput: Input[WidgetId] = Input[String].contramap(id => s"Widget#${id.get}")
  implicit val rowVersionInput: Input[RowVersion] = Input[String].contramap(v => f"Version#${v.get}%07d")
  implicit val instantInput: Input[Instant] = Input[Long].contramap(_.getEpochSecond)

  def main(args: Array[String]): Unit = {
    val id = WidgetId("6aa39188877008f8e2f576f559efe00f")
    val version = RowVersion(5)
    val since = Instant.now.minus(14, ChronoUnit.DAYS)
    val limit = 10
    val query = query"""
      SELECT *
      FROM Entities
      WHERE PK = $id AND SK >= $version
      FILTER UpdatedAt > $since
      LIMIT $limit
    """

    val client = DynamoDbAsyncClient.builder.endpointOverride(new URI("http://localhost:8000")).build
    val result = Await.result(client.query(query.toRequest).toScala, Duration.Inf)
    println(result)
  }
}
```

This is mostly an experiment in "can you do this?" rather than "is this a good idea?". Although there are bits in it that I think are a good idea, like using typeclasses to format the values. And expression building is a lot less clunky than the AWS SDK.
