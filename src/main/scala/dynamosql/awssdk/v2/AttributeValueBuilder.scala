package dynamosql.awssdk.v2

import dynamosql.model.Value
import dynamosql.model.Value._
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.collection.JavaConverters._

object AttributeValueBuilder {
  def build(value: Value, args: Map[String, Value] = Map.empty): AttributeValue = value match {
    case Binary(b) => AttributeValue.builder.b(SdkBytes.fromByteBuffer(b)).build
    case Bool(bool) => AttributeValue.builder.bool(bool).build
    case N(n) => AttributeValue.builder.n(n).build
    case S(s) => AttributeValue.builder.s(s).build
    case SS(ss) => AttributeValue.builder.ss(ss.asJavaCollection).build
    case Arg(name) => build(args.getOrElse(name, throw new IllegalArgumentException(s"Arg $name not supplied")), args)
  }
}
