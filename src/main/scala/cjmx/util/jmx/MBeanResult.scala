package cjmx.util.jmx

import javax.management.{Attribute, ObjectName}

case class MBeanResult(name: ObjectName,
                       attributes: Map[String,AnyRef]) {

  def get(attrName: String): Option[AnyRef] =
    attributes.get(attrName)
}

object MBeanResult {
  def apply(name: ObjectName, attributes: Seq[Attribute]): MBeanResult =
    MBeanResult(name, attributes.map(a => a.getName -> a.getValue).toMap)
}
