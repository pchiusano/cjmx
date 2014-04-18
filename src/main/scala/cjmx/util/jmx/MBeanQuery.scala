package cjmx.util.jmx

import javax.management.{Attribute, ObjectName, QueryExp}

/** Represents a where for MBeans that match an expression. */
case class MBeanQuery(
  from: Map[String, (Option[ObjectName], Option[QueryExp])],
  where: Option[Map[String, MBeanResult] => Boolean] = None)

object MBeanQuery {
  private val unnamed = "#0" // used when user does not name a query clause

  def All = MBeanQuery(Map(unnamed -> ((None,None))))

  def Single(from: ObjectName, where: Option[QueryExp] = None): MBeanQuery =
    MBeanQuery(Map(unnamed -> ((Some(from), where))))
}

