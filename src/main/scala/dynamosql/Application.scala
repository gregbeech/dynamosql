package dynamosql

import java.nio.ByteBuffer

import cats._
import cats.data._
import cats.implicits._
import dynamosql.model._
import dynamosql.model.Value._
import dynamosql.parser.QueryParser
import dynamosql.request.QueryRequestBuilder
import shapeless.ProductArgs
import shapeless.{::, HList, HNil, Lazy}

import scala.util.{Failure, Success}


trait Input[A] {
  def value(a: A): Value
}

object Input {
  implicit val InputIsContravariant: Contravariant[Input] = new Contravariant[Input] {
    override def contramap[A, B](fa: Input[A])(f: B => A): Input[B] = b => fa.value(f(b))
  }

  implicit val booleanInput: Input[Boolean] = a => BOOL(a)
  implicit val byteBufferInput: Input[ByteBuffer] = a => Binary(a)
  implicit val byteArrayInput: Input[Array[Byte]] = byteBufferInput.contramap(ByteBuffer.wrap)
  implicit val bigDecimalInput: Input[BigDecimal] = a => N(a.toString)
  implicit val bigIntInput: Input[BigInt] = a => N(a.toString)
  implicit val byteInput: Input[Byte] = a => N(a.toString)
  implicit val doubleInput: Input[Double] = a => N(a.toString)
  implicit val floatInput: Input[Float] = a => N(a.toString)
  implicit val intInput: Input[Int] = a => N(a.toString)
  implicit val longInput: Input[Long] = a => N(a.toString)
  implicit val stringInput: Input[String] = a => S(a)
  implicit val stringSetInput: Input[Set[String]] = a => SS(a)

  def apply[A](implicit a: Input[A]): Input[A] = a
}


final class Write[A](val inputs: List[Input[_]], val toList: A => List[Value]) {
//  def product[B](fb: Write[B]): Write[(A, B)] = new Write(
//    length + fb.length,
//    { case (ps, n, (a, b)) => unsafeSet(ps, n, a); fb.unsafeSet(ps, n + length, b) },
//  )
}

object Write {
//  def apply[A](implicit A: Write[A]): Write[A] = A

//  implicit val semigroupalInstanceForWrite: Semigroupal[Write] = new Semigroupal[Write] {
//    def product[A, B](fa: Write[A], fb: Write[B]): Write[(A, B)] = fa.product(fb)
//  }

  def product[H, T <: HList](implicit H: Write[H], T: Write[T]): Write[H :: T] = new Write(
    H.inputs ++ T.inputs,
    { case h :: t => H.toList(h) ++ T.toList(t) }
  )

  def emptyProduct: Write[HNil] = new Write[HNil](Nil, _ => Nil)
}

final class Param[A](val write: Write[A])

object Param {
  implicit val ParamHNil: Param[HNil] = new Param[HNil](
    Write.emptyProduct
  )

  implicit def ParamHList[H, T <: HList](implicit ph: Param[H], pt: Param[T]): Param[H :: T] = new Param[H :: T](
    Write.product[H, T](ph.write, pt.write)
  )

  implicit def ParamValue[A](implicit ev: Input[A]): Param[A] = new Param[A](
    new Write(List(ev), v => List(ev.value(v)))
  )
}

case class ParameterisedQuery(query: Query, args: Map[String, Value])

final class DynamoInterpolator(sc: StringContext) {
  private def mkQuery[A](a: A)(implicit ev: Param[A]): ParameterisedQuery = {
    val sql = sc.parts.zipWithIndex.map {
      case (part, 0) => part
      case (part, n) => s":${n - 1}$part"
    }.mkString
    println(sql)
    println(ev.write.toList(a))
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

object syntax extends ToDynamoInterpolator

object Application {
  import dynamosql.syntax._

  case class WidgetId(get: String)

  implicit val widgetIdInput: Input[WidgetId] = Input[String].contramap(id => s"Widget#${id.get}")

  def main(args: Array[String]): Unit = {
    val id = WidgetId("1234")
    val sk = "details"
    val limit = 10
    val pq = query"SELECT * FROM Entities WHERE Id = $id AND SK[0].X = $sk LIMIT $limit"
    println(pq)

    val qr = QueryRequestBuilder.build(pq)
    println(qr)

//    SELECT [* | attr,... | COUNT(*)]
//    FROM tableName [INDEX indexName | WITH CONSISTENT_READ]
//    WHERE foo = $foo AND bar BEGINS_WITH $bar
//    FILTER baz > $baz
//    ORDER {ASC | DESC}
//    LIMIT $limit

    println(new QueryParser("-123.456E10").attrN.run())

    val qp1 = new QueryParser("SELECT foo, bar, baz").select.run()
    val qp2 = new QueryParser("FROM my.table INDEX my.index").from.run()
    val qp3 = new QueryParser("WHERE k1 = 's1' AND k2 = -123.456E10").where.run()
    val qp4 = new QueryParser("FILTER k1 <> true AND k2 BETWEEN 's2' AND 's3' AND k3 BEGINS WITH 123.5").filter.run()
    println(qp1)
    println(qp2)
    println(qp3)
    println(qp4)

    val qp = new QueryParser(
      """
        SELECT foo, bar, baz
        FROM my.table INDEX my.index
        WHERE k1 = 's1' AND k2 BEGINS WITH 'foo'
        FILTER k3 NOT CONTAINS 'bar'
      """).query.run()
    println(qp)
  }
}
