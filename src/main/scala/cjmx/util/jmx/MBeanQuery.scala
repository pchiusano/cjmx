package cjmx.util.jmx

import javax.management.{Attribute, ObjectName, QueryExp}

//TODO: QueryExpr needs to change,

/** Represents a where for MBeans that match an expression. */
case class MBeanQuery(from: Map[String, (Option[ObjectName], Option[QueryExp])],
                      where: Option[Seq[(String,Attribute)] => Boolean] = None)

object MBeanQuery {
  private val unnamed = "#0" // used when user does not name a query clause

  def All = MBeanQuery(Map(unnamed -> ((None,None))), None)

  def Single(from: ObjectName, where: Option[QueryExp] = None): MBeanQuery =
    MBeanQuery(Map(unnamed -> ((Some(from), where))))
}

