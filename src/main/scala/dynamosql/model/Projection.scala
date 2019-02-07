package dynamosql.model

sealed trait Projection
case object AllAttributes extends Projection
case class SpecificAttributes(attributes: Seq[Path]) extends Projection
case object Count extends Projection

object Projection {
  def allAttributes: Projection = AllAttributes
  def specificAttributes(attributes: Seq[Path]): Projection = SpecificAttributes(attributes)
  def count: Projection = Count
}
