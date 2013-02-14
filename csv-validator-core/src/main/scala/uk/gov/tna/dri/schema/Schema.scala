package uk.gov.tna.dri.schema

import util.Try
import uk.gov.tna.dri.metadata.Row
import javax.security.auth.login.FailedLoginException

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition]) {
  require(totalColumns == columnDefinitions.length, s"totalColumns: ${totalColumns} must be the same as the number of column definitions: ${columnDefinitions.size}")

  val errors = checkColumnDefinitionsHasValidNames(columnDefinitions)
  val errMsg = errors.map( e => s"Column: ${e._1.id} has invalid cross reference ${prettyInRules(e._2)}" )
  require( errors.length == 0, errMsg.mkString("\n") )

  def checkColumnDefinitionsHasValidNames(columnDefinitions: List[ColumnDefinition]): List[(ColumnDefinition, List[Rule])] = {
    val errs:List[(ColumnDefinition, List[Rule])] =  columnDefinitions.map( columnDef => (columnDef, filterRules(columnDef))).filter( x => x._2.length > 0 )
    errs
  }

  def filterRules(columnDef:ColumnDefinition ): List[Rule] = { // List of failing rules
    columnDef.rules.filter( rule =>  {
      findColumnReference(rule) match {
        case Some(name) => {
          val found = columnDefinitions.exists( col =>  col.id == name )

          !found
        }
        case None => false
      }
    })
  }

  def findColumnReference( rule:Rule ): Option[String] = rule match {
    case IsRule(s) => findColumnName(s)
    case NotRule(s) => findColumnName(s)
    case InRule(s) => findColumnName(s)
    case StartsRule(s) => findColumnName(s)
    case EndsRule(s) => findColumnName(s)
    case _ => None
  }

  def findColumnName( s:StringProvider ): Option[String] = {
    s match {
      case ColumnTypeProvider(name) => {
        Some(name)
      }
      case _ => None
    }
  }

  def prettyInRules(inRule: List[Rule]): String = {
    inRule.map{
      case rule:IsRule => s"${rule.name}: ${rule.inVal.value}"
      case rule:NotRule => s"${rule.name}: ${rule.inVal.value}"
      case rule:InRule => s"${rule.name}: ${rule.inVal.value}"
      case rule:StartsRule => s"${rule.name}: ${rule.inVal.value}"
      case rule:EndsRule => s"${rule.name}: ${rule.inVal.value}"
      case _ => ""
    }.mkString(",")
  }

}


case class ColumnDefinition(id: String, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil) {
  def contains(columnDirective: ColumnDirective) = Try(directives.contains(columnDirective)).getOrElse(false)
}

abstract class StringProvider(val value: String) {
  def referenceValue(columnIndex: Int, row: Row, schema: Schema): String
}

case class ColumnTypeProvider(override val value: String) extends StringProvider(value) {
  def referenceValue(columnIndex: Int, row: Row, schema: Schema): String = {
    val referencedIndex = schema.columnDefinitions.indexWhere(_.id == value)
    row.cells(referencedIndex).value
  }
}

case class LiteralTypeProvider(override val value: String) extends StringProvider(value) {
  def referenceValue(columnIndex: Int, row: Row, schema: Schema): String = value
}

trait ColumnDirective

case class Optional() extends ColumnDirective

case class IgnoreCase() extends ColumnDirective