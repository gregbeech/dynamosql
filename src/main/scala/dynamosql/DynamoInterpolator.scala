package dynamosql

import dynamosql.model.ParameterisedQuery
import dynamosql.parser.QueryParser
import shapeless.ProductArgs

import scala.util.{Failure, Success}

final class DynamoInterpolator(sc: StringContext) {
  private def mkQuery[A](a: A)(implicit ev: Param[A]): ParameterisedQuery = {
    val sql = sc.parts.zipWithIndex.map {
      case (part, 0) => part
      case (part, n) => s":${n - 1}$part"
    }.mkString
    new QueryParser(sql).query.run() match {
      case Success(query) => ParameterisedQuery(query, ev.write.toList(a).zipWithIndex.map {
        case (v, n) => s":$n" -> v
      }.toMap)
      case Failure(error) => throw error
    }
  }

  object query extends ProductArgs {
    def applyProduct[A: Param](a: A): ParameterisedQuery = mkQuery(a)
  }
}

trait ToDynamoInterpolator {
  implicit def toDynamoInterpolator(sc: StringContext): DynamoInterpolator = new DynamoInterpolator(sc)
}
