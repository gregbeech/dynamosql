package dynamosql.model

sealed trait From
case class Table(name: String) extends From
case class Index(name: String, tableName: String) extends From

object From {
  def apply(tableName: String, indexName: Option[String]): From = indexName match {
    case Some(name) => Index(name, tableName)
    case None => Table(tableName)
  }
  def table(name: String): From = Table(name)
  def index(name: String, tableName: String): From = Index(name, tableName)
}
