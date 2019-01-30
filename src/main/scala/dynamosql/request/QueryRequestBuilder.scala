package dynamosql.request

import java.util

import com.amazonaws.services.dynamodbv2.model.{AttributeValue, QueryRequest}
import com.amazonaws.services.dynamodbv2.xspec.{ExpressionSpecBuilder => xb}
import com.amazonaws.services.dynamodbv2.xspec
import dynamosql.{ParameterisedQuery, model}
import dynamosql.model._
import dynamosql.model.Value._

import scala.annotation.tailrec

object QueryRequestBuilder {
  def build(pq: ParameterisedQuery): QueryRequest = {
    val expr = new xb()
      .withKeyCondition(mkCondition(pq.query.where, pq.args))
      .buildForQuery()

    val request = init(pq.query.from)
      .withKeyConditionExpression(expr.getKeyConditionExpression)
      .withExpressionAttributeNames(expr.getNameMap)
      .withExpressionAttributeValues(toAttributeValues(expr.getValueMap))


    request
  }

  private def init(from: From): QueryRequest = from match {
    case Table(tableName) => new QueryRequest(tableName)
    case Index(indexName, tableName) => new QueryRequest(tableName).withIndexName(indexName)
  }

  private def mkCondition(where: Where, args: Map[String, Value]): xspec.Condition = where match {
    case Where(pk, None) => mkCondition(pk, args)
    case Where(pk, Some(sk)) => mkCondition(pk, args).and(mkCondition(sk, args))
  }

  private def mkCondition(cond: model.Condition, args: Map[String, Value]): xspec.Condition = cond.operation match {
    case Eq(value) => mkEqCondition(cond, value, args)
    case other => throw new NotImplementedError(other.toString)
  }

  @tailrec private def mkEqCondition(cond: model.Condition, value: Value, args: Map[String, Value]): xspec.Condition =
    value match {
      case S(s) => xb.S(str(cond.attrName)).eq(s)
      case Arg(name) => mkEqCondition(cond, args(name), args)
      case other => throw new NotImplementedError(other.toString)
    }

  private def str(attrName: AttrName): String = attrName match {
    case Name(name) => name
    case Path(Name(name), segments) => name + str(segments)
  }

  private def str(segments: List[Segment]): String = segments match {
    case Nil => ""
    case Elem(index) :: xs => s"[$index]" + str(xs)
    case Name(name) :: xs => s".$name" + str(xs)
  }

  protected def toAttributeValues(attributes: util.Map[String, AnyRef]): util.Map[String, AttributeValue] = {
    val values = new util.HashMap[String, AttributeValue](attributes.size)
    attributes.forEach((key, value) => values.put(key, toAttributeValue(value)))
    values
  }

  protected def toAttributeValue(value: AnyRef): AttributeValue = value match {
    case v: AttributeValue => v
    case v: String => new AttributeValue(v)
    case v: Number => new AttributeValue().withN(v.toString)
    case v: util.Map[String, AnyRef] @unchecked => new AttributeValue().withM(toAttributeValues(v))
    case v => throw new NotImplementedError(s"Add a new case for values of type $v")
  }
}