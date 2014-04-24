package cjmx.util.jmx

import javax.management.{MBeanServerConnection,ObjectName,QueryExp,Query=>Q,StringValueExp,ValueExp}
import scala.collection.JavaConverters._

/** Typed API for interacting with MBeans. */
object Beans extends ToRichMBeanServerConnection {

  private[cjmx] val unnamed = SubqueryName("#0") // used when user does not name the subquery

  // newtyping these to keep them straight

  /** The name of a subquery, which may return multiple `Result` values. */
  case class SubqueryName(get: String)

  /**
   * A subquery, which may return multiple `Result` values.
   * Example `mbeans 'java.nio:*' where Name='mapped' select *` would be represented as:
   * `Subquery(unnamed, new ObjectName("java.nio:*"), Some(Name='mapped'))`.
   */
  case class Subquery(name: SubqueryName, pattern: ObjectName, where: Option[QueryExp]) {
    def run(conn: MBeanServerConnection): Set[ObjectName] =
      conn.queryNames(pattern, where.orNull).asScala.toSet
  }

  /**
   * Represents a single field extracted from one or more beans.
   * Where possible tries to run the entire expression remotely.
   */
  trait Field[Exp] {
    import Field._

    def combine(b: Field[Exp])(remotely: (Exp,Exp) => Exp,
                               locally: (AnyRef,AnyRef) => Option[AnyRef]): Field[Exp] = (this,b) match {
      case (Remote(s1, e1, f1), Remote(s2, e2, f2)) if s1.orElse(s2) == s2 =>
        val f3 = (res: Results) => for { a <- f1(res); b <- f2(res); c <- locally(a,b) } yield c
        Remote(s1, remotely(e1,e2), f3)
      case _ =>
        val f1 = this.locally
        val f2 = b.locally
        Local { (res: Results) => for { a <- f1(res); b <- f2(res); c <- locally(a,b) } yield c }
    }

    def combineNumeric(b: Field[ValueExp])(
        remotely: (ValueExp,ValueExp) => ValueExp,
        locally: (BigDecimal,BigDecimal) => BigDecimal)(
        implicit E: Exp =:= ValueExp): Field[ValueExp] =
      this.asInstanceOf[Field[ValueExp]].combine(b)(remotely, (a,b) => {
        for { a <- cjmx.util.Math.liftToBigDecimal(a)
              b <- cjmx.util.Math.liftToBigDecimal(b)
            } yield (a + b)
      })

    def +(b: Field[ValueExp])(implicit E: Exp =:= ValueExp): Field[ValueExp] =
      this.combineNumeric(b)(Q.plus, _ + _)
    def -(b: Field[ValueExp])(implicit E: Exp =:= ValueExp): Field[ValueExp] =
      this.combineNumeric(b)(Q.minus, _ - _)
    def *(b: Field[ValueExp])(implicit E: Exp =:= ValueExp): Field[ValueExp] =
      this.combineNumeric(b)(Q.times, _ * _)
    def /(b: Field[ValueExp])(implicit E: Exp =:= ValueExp): Field[ValueExp] =
      this.combineNumeric(b)(Q.div, _ / _)

    def locally: Results => Option[AnyRef] = this match {
      case Local(f) => f
      case Remote(_, _, f) => f
    }
  }

  object Field {
    /** A field expression that will be evaluated remotely. */
    case class Remote[Exp](
      subquery: Option[SubqueryName], // None indicates this is a literal
      expression: Exp,
      locally: Results => Option[AnyRef]) extends Field[Exp]

    /** A field expression that will evaluated client side. */
    case class Local[Exp](f: Results => Option[AnyRef]) extends Field[Exp]

    // def lit(s: String)
  }

  case class Where(restrictions: Map[SubqueryName, QueryExp],
                   clientSideFilter: Results => Set[ObjectName] = _.names) {

    def apply(q: Query): Query =
      sys.error("TODO: apply the restrictions server side, and delegate the rest to be run client side")

    def and(w2: Where): Where =
      sys.error("TODO")

    def or(w: Where): Where =
      sys.error("TODO")

    def not: Where =
      sys.error("TODO")
  }

  object Where {
    def id = Where(Map(), _.names)

    def fromQueryExp(exp: QueryExp, subquery: SubqueryName = unnamed): Where =
      Where(Map(subquery -> exp))

    def isInstanceOf(s: StringValueExp): Where =
      Where(Map(unnamed -> Q.isInstanceOf(s)))
  }

  case class Query(subqueries: Map[SubqueryName, Subquery],
                   where: Results => Set[ObjectName] = (rs => rs.names),
                   project: Results => Seq[Result] = (rs => rs.results)) {

    def restrict(where2: Results => Set[ObjectName]): Query =
      Query(subqueries, res => where(res) intersect where2(res), project)

    def run(conn: MBeanServerConnection): Seq[Result] = {
      val objNames = subqueries.values.map(_.run(conn)).foldLeft(Set[ObjectName]())(_ ++ _)
      val results = Results {
        objNames.map { name =>
          val info = conn.mbeanInfo(name)
          ???
        }
        ???
      }
      ???
    }

    def names: Set[ObjectName] =
      subqueries.values.map(_.pattern).toSet

    def subqueryNames: Map[SubqueryName, ObjectName] =
      subqueries.mapValues(_.pattern)
  }

  object Query {
    val All = Query(Map(unnamed -> Subquery(unnamed, ObjectName.WILDCARD, None)))

    def Single(name: Option[ObjectName], where: Option[QueryExp]): Query =
      Query(Map(unnamed -> Subquery(unnamed, name.getOrElse(ObjectName.WILDCARD), where)))

    def byName(pattern: String): Query =
      Single(Some(new ObjectName(pattern)), None)

    def byName(name: ObjectName): Query =
      Single(Some(name), None)
  }

  /**
   * The name of a property in a `javax.management.ObjectName`. For instance,
   * the `ObjectName` "java.nio:type=BufferPool,name=direct" has two properties, `type`,
   * which has a value of `"BufferPool"`, and `name`, which has a value of `"direct"`.
   */
  case class ObjectNameKey(key: String) {
    def get(name: ObjectName): Option[String] = ???
  }

  /**
   * The name of an attribute of a bean. Example: {{{
   *
   * java.nio:type=BufferPool,name=mapped
   * ------------------------------------
   * Name: mapped
   * Count: 0
   * TotalCapacity: 0
   * MemoryUsed: 0
   * ObjectName: java.nio:type=BufferPool,name=mapped*
   *
   * }}}
   *
   * Has the following attributes: "Name", "Count", "TotalCapacity", ...
   */
  case class AttributeName(name: String) {
    def get(result: Result): Option[AnyRef] = result.attributes.get(this)
  }

  /**
   * A single MBean result. Example: {{{
   *
   * java.nio:type=BufferPool,name=mapped
   * ------------------------------------
   * Name: mapped
   * Count: 0
   * TotalCapacity: 0
   * MemoryUsed: 0
   * ObjectName: java.nio:type=BufferPool,name=mapped*
   *
   * }}}
   *
   * The `ObjectName` is `"java.nio:type=BufferPool,name=mapped"`, and the
   * map of attributes will be: `Map(AttributeName("Name") -> "mapped", ...)`
   */
  case class Result(name: ObjectName,
                    attributes: Map[AttributeName,AnyRef]) {

    /** Extract a property from the `ObjectName` for this `Result`. */
    def getKeyProperty(k: ObjectNameKey): Option[String] =
      Option(name.getKeyProperty(k.key))

    /** Extract an attribute value for the given attribute name. */
    def getAttribute(k: AttributeName): Option[AnyRef] =
      attributes.get(k)
  }

  case class Results(subqueries: Map[SubqueryName, Seq[Result]]) {
    def names: Set[ObjectName] = subqueries.values.flatMap(results => results.map(_.name)).toSet
    def results: Seq[Result] = subqueries.values.flatten.toSeq
  }
}
