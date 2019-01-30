package dynamosql.model

sealed trait Select
case object AllAttributes extends Select
case class SpecificAttributes(names: Seq[AttrName]) extends Select
case object Count extends Select

object Select {
  def allAttributes: Select = AllAttributes
  def specificAttributes(names: Seq[AttrName]): Select = SpecificAttributes(names)
  def count: Select = Count
}
