package haruko.lang

import org.graalvm.collections.Pair

import scala.annotation.tailrec

object Core {
  private def runtimeMathCheck(seq: Seq[Object]): Unit = {
    seq.foreach {
      case x if x.isInstanceOf[Number] =>
      case o => throw new RuntimeException("Cannot do math on non-number: " + o.toString)
    }
  }

  private def binaryMathOp(a: Number, b: Number, op: Char): Object = {
    (a, b, op) match {
      case (x, y, _) if x.isInstanceOf[Long] && y.isInstanceOf[Long] =>
        op match {
          case '+' => (x.longValue() + y.longValue()).asInstanceOf[Object]
          case '-' => (x.longValue() - y.longValue()).asInstanceOf[Object]
          case '*' => (x.longValue() * y.longValue()).asInstanceOf[Object]
          case '/' => (x.doubleValue() / y.doubleValue()).asInstanceOf[Object]
          case '<' => (x.longValue() < y.longValue()).asInstanceOf[Object]
          case '>' => (x.longValue() > y.longValue()).asInstanceOf[Object]
          case '^' => (x.longValue() <= y.longValue()).asInstanceOf[Object]
          case '?' => (x.longValue() >= y.longValue()).asInstanceOf[Object]
        }
      case (x, y, _) if x.isInstanceOf[Double] || y.isInstanceOf[Double] =>
        op match {
          case '+' => (x.doubleValue() + y.doubleValue()).asInstanceOf[Object]
          case '-' => (x.doubleValue() - y.doubleValue()).asInstanceOf[Object]
          case '*' => (x.doubleValue() * y.doubleValue()).asInstanceOf[Object]
          case '/' => (x.doubleValue() / y.doubleValue()).asInstanceOf[Object]
          case '<' => (x.doubleValue() < y.doubleValue()).asInstanceOf[Object]
          case '>' => (x.doubleValue() > y.doubleValue()).asInstanceOf[Object]
          case '^' => (x.doubleValue() <= y.doubleValue()).asInstanceOf[Object]
          case '?' => (x.doubleValue() >= y.doubleValue()).asInstanceOf[Object]
        }
      case _ => throw new IllegalArgumentException("Unsupported number type")
    }
  }

  private def verifyIfBool(o: Object): Unit = {
    if (!o.isInstanceOf[Boolean])
      throw new IllegalArgumentException("Expected boolean, found: " + o.toString)
  }

  def prln(o: Object) : Unit = {
    println(o)
  }

  def prln$(objects: Array[Object]): Unit = {
    objects.foreach(prln)
  }

  def add$(objects: Array[Object]) : Object = {
    runtimeMathCheck(objects.toSeq)

    var result: Number = 0L.asInstanceOf[Number]
    objects.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '+').asInstanceOf[Number]
    })

    result
  }

  def add(a: Object, b: Object) : Object = {
    runtimeMathCheck(List(a,b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '+')
  }

  def inc(a: Object) : Object = {
    add(a, Long.box(1))
  }

  def dec(a: Object) : Object = {
    sub(a, Long.box(1))
  }

  def mul$(objects: Array[Object]): Object = {
    runtimeMathCheck(objects.toSeq)

    var result: Number = 1L.asInstanceOf[Number]
    objects.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '*').asInstanceOf[Number]
    })

    result
  }

  def mul(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '*')
  }

  def sub$(objects: Array[Object]): Object = {
    runtimeMathCheck(objects.toSeq)

    var result: Number = objects(0).asInstanceOf[Number]
    objects.tail.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '-').asInstanceOf[Number]
    })

    result
  }

  def sub(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '-')
  }

  def div$(objects: Array[Object]): Object = {
    runtimeMathCheck(objects.toSeq)

    var result: Number = objects(0).asInstanceOf[Number]
    objects.tail.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '/').asInstanceOf[Number]
    })

    result
  }

  def div(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '/')
  }

  def gt(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '>')
  }

  def lt(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '<')
  }

  def geq(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '^')
  }

  def leq(a: Object, b: Object): Object = {
    runtimeMathCheck(List(a, b))
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '?')
  }

  def and$(objects: Array[Object]): Object = {
    var result = Boolean.box(true)
    objects.foreach( x => {
      result = and(result, x).asInstanceOf[Boolean]
      if (!result)
        return result
    })

    result
  }

  def and(a: Object, b: Object): Object = {
    verifyIfBool(a)
    verifyIfBool(b)
    Boolean.box(a.asInstanceOf[Boolean] && b.asInstanceOf[Boolean])
  }

  def or$(objects: Array[Object]): Object = {
    var result = Boolean.box(false)
    objects.foreach(x => {
      result = or(result, x).asInstanceOf[Boolean]
      if (result)
        return result
    })
    result
  }

  def or(a: Object, b: Object): Object = {
    verifyIfBool(a)
    verifyIfBool(b)
    Boolean.box(a.asInstanceOf[Boolean] || b.asInstanceOf[Boolean])
  }

  def not(o: Object) : Object = {
    verifyIfBool(o)
    Boolean.box(!o.asInstanceOf[Boolean])
  }
  
  def eq(a: Object, b: Object): Object = {
    Boolean.box(a.equals(b))
  }
  
  def neq(a: Object, b: Object): Object = {
    Boolean.box(!a.equals(b))
  }
}
