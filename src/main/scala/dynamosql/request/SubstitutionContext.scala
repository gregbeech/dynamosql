package dynamosql.request

import dynamosql.model.Value

import scala.collection.mutable

class SubstitutionContext(args: Map[String, Value] = Map.empty) {
  private[this] val names: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap.empty
  private[this] val values: mutable.LinkedHashMap[Value, String] = mutable.LinkedHashMap.empty

  def name(name: String): String = names.getOrElse(name, {
    val sub = s"#${names.size}"
    names += (name -> sub)
    sub
  })

  def value(value: Value): String = values.getOrElse(value, {
    val sub = s":${args.size + values.size}"
    values += (value -> sub)
    sub
  })

  def nameMap: Map[String, String] = names.map(_.swap).toMap
  def valueMap: Map[String, Value] = values.map(_.swap).toMap
}
