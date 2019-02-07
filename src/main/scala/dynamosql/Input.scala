package dynamosql

import java.nio.ByteBuffer

import cats._
import cats.implicits._
import dynamosql.model.Value
import dynamosql.model.Value._

trait Input[A] {
  def value(a: A): Value
}

object Input {
  implicit val InputIsContravariant: Contravariant[Input] = new Contravariant[Input] {
    override def contramap[A, B](fa: Input[A])(f: B => A): Input[B] = b => fa.value(f(b))
  }

  implicit val booleanInput: Input[Boolean] = a => Bool(a)
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
