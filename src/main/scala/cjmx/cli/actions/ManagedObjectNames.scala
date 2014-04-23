package cjmx.cli
package actions

import scalaz.syntax.validation._

import javax.management.{ObjectName, QueryExp}

import cjmx.util.jmx.{JMXConnection, Beans => B}


case class ManagedObjectNames(query: B.Query) extends SimpleConnectedAction {
  def act(context: ActionContext, connection: JMXConnection) = {
    val names = connection.mbeanServer.toScala.queryNames(query).toList.sorted
    context.formatter.formatNames(names)
  }
}

