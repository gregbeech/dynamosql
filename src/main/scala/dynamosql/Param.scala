package dynamosql

import shapeless.{::, HList, HNil}

final class Param[A](val write: Write[A])

object Param {
  implicit val ParamHNil: Param[HNil] = new Param[HNil](
    Write.empty
  )

  implicit def ParamHList[H, T <: HList](implicit ph: Param[H], pt: Param[T]): Param[H :: T] = new Param[H :: T](
    Write.product[H, T](ph.write, pt.write)
  )

  implicit def ParamValue[A](implicit ev: Input[A]): Param[A] = new Param[A](
    new Write(List(ev), v => List(ev.value(v)))
  )
}
