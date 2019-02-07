package dynamosql

import dynamosql.model.Value
import shapeless.{::, HList, HNil}

final class Write[A](val inputs: List[Input[_]], val toList: A => List[Value])

object Write {
  def product[H, T <: HList](implicit H: Write[H], T: Write[T]): Write[H :: T] = new Write(
    H.inputs ++ T.inputs,
    { case h :: t => H.toList(h) ++ T.toList(t) }
  )

  def emptyProduct: Write[HNil] = new Write[HNil](Nil, _ => Nil)
}