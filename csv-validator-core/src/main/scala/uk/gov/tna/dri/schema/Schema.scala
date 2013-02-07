package uk.gov.tna.dri.schema

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition])

class GlobalDirectives(sepDirective: Option[SeparatorDirective], quotedDirective: Option[QuotedDirective], totColsDirective: Option[TotalColumnsDirective])

trait GlobalDirective
case class SeparatorDirective(separatorChar: Char) extends GlobalDirective
case class QuotedDirective() extends GlobalDirective
case class TotalColumnsDirective(numOfColumns: Int) extends GlobalDirective
case class NoHeaderDirective() extends GlobalDirective
case class IgnoreColumnNameCaseDirective() extends GlobalDirective

class Body()

class ColumnDirectives
class ColumnRules

abstract class StringProvider(val value: String) {
  def getColumnValue(colToValMap: Map[String, String]): String
}

case class ColumnTypeProvider(override val value: String) extends StringProvider(value) {
  def getColumnValue(colToValMap: Map[String, String]): String = colToValMap(value.tail)
}

case class LiteralTypeProvider(override val value: String) extends StringProvider(value) {
  def getColumnValue(colToValMap: Map[String, String]): String = value
}
