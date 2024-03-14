package haruko.lang

case class HList(value: List[Object]) extends Object {
  override def toString: String = value.toString()
}

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
          case '/' => (x.longValue() / y.longValue()).asInstanceOf[Object]
          case '<' => (x.longValue() < y.longValue()).asInstanceOf[Object]
          case '>' => (x.longValue() > y.longValue()).asInstanceOf[Object]
          case '^' => (x.longValue() <= y.longValue()).asInstanceOf[Object]
          case '?' => (x.longValue() >= y.longValue()).asInstanceOf[Object]
          case 'p' => Math.pow(x.doubleValue(), y.doubleValue()).longValue().asInstanceOf[Object]
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
          case 'p' => Math.pow(x.doubleValue(), y.doubleValue()).asInstanceOf[Object]
        }
      case _ => throw new IllegalArgumentException("Unsupported number type")
    }
  }

  private def verifyIfBool(o: Object): Unit = {
    if (!o.isInstanceOf[Boolean])
      throw new IllegalArgumentException("Expected boolean, found: " + o.toString)
  }

  def prln(o: Object) : Object = {
    println(o)
    null
  }

  def prln$(objects: Array[Object]): Object = {
    objects.foreach(o => pr(o + " "))
    print("\n")
    System.out.flush()
    null
  }

  def cat(a: Object, b: Object): Object = {
    a.toString + b.toString
  }

  def cat$(objects: Array[Object]) : Object = {
    objects.reduce((a, b) => a.toString + b)
  }

  def pr(o: Object) : Object = {
    print(o)
    System.out.flush()
    null
  }

  def pr$(objects: Array[Object]) : Object = {
    objects.foreach(pr)
    System.out.flush()
    null
  }

  def add$(objects: Array[Object]) : Object = {
    var result: Number = 0L.asInstanceOf[Number]
    objects.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '+').asInstanceOf[Number]
    })

    result
  }

  def add(a: Object, b: Object) : Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '+')
  }

  def inc(a: Object) : Object = {
    add(a, Long.box(1))
  }

  def dec(a: Object) : Object = {
    sub(a, Long.box(1))
  }

  def pow(a: Object, b: Object) : Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], 'p')
  }

  def mul$(objects: Array[Object]): Object = {
    var result: Number = 1L.asInstanceOf[Number]
    objects.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '*').asInstanceOf[Number]
    })

    result
  }

  def mul(a: Object, b: Object): Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '*')
  }

  def sub$(objects: Array[Object]): Object = {
    var result: Number = objects(0).asInstanceOf[Number]
    objects.tail.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '-').asInstanceOf[Number]
    })

    result
  }

  def sub(a: Object, b: Object): Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '-')
  }

  def div$(objects: Array[Object]): Object = {
    var result: Number = objects(0).asInstanceOf[Number]
    objects.tail.foreach(x => {
      result = binaryMathOp(result, x.asInstanceOf[Number], '/').asInstanceOf[Number]
    })

    result
  }

  def div(a: Object, b: Object): Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '/')
  }

  def gt(a: Object, b: Object): Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '>')
  }

  def lt(a: Object, b: Object): Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '<')
  }

  def geq(a: Object, b: Object): Object = {
    binaryMathOp(a.asInstanceOf[Number], b.asInstanceOf[Number], '^')
  }

  def leq(a: Object, b: Object): Object = {
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

  def mod(a: Object, b: Object) : Object = {
    if (b.equals(0))
      throw new IllegalArgumentException("Division by zero")
    (a, b) match {
      case (x, y) if x.isInstanceOf[Long] && y.isInstanceOf[Long] =>
        (x.asInstanceOf[Long] % y.asInstanceOf[Long]).asInstanceOf[Object]
      case _ => throw new IllegalArgumentException("Mod on non integers")
    }
  }

  def list$(objects: Array[Object]) : Object = {
    HList(objects.toList)
  }

  def app(list: Object, o: Object) : Object = {
    assert(list.isInstanceOf[HList], "Cannot append to non-list object")
    o match {
      case list1: HList => HList(list.asInstanceOf[HList].value.appendedAll(list1.value))
      case _ => HList(list.asInstanceOf[HList].value.appended(o))
    }
  }

  def head(list: Object) : Object = {
    assert(list.isInstanceOf[HList], "Cannot find head of non-list object")
    if (list.asInstanceOf[HList].value.isEmpty)
      null
    else
      list.asInstanceOf[HList].value.head
  }

  def tail(list: Object): Object = {
    assert(list.isInstanceOf[HList], "Cannot find tail of non-list object")
    if (list.asInstanceOf[HList].value.isEmpty)
      list
    else
      HList(list.asInstanceOf[HList].value.tail)
  }

  def len(o: Object) : Object = {
    assert(o.isInstanceOf[HList] || o.isInstanceOf[String])
    if (o.isInstanceOf[String])
      Long.box(o.toString.length).asInstanceOf[Object]
    else
      Long.box(o.asInstanceOf[HList].value.length).asInstanceOf[Object]
  }

  def get(o: Object, i: Object): Object = {
    assert(o.isInstanceOf[HList])
    assert(i.asInstanceOf[Long] < o.asInstanceOf[HList].value.length)
    o.asInstanceOf[HList].value(i.asInstanceOf[Long].intValue())
  }

  def empty(o: Object) : Object = {
    assert(o.isInstanceOf[HList])
    Boolean.box(o.asInstanceOf[HList].value.isEmpty)
  }

  def slice(lst: Object, i1: Object, i2: Object) : Object = {
    assert(lst.isInstanceOf[HList])
    assert(lst.asInstanceOf[HList].value.nonEmpty)
    assert(i1.isInstanceOf[Long] && i2.isInstanceOf[Long])
    HList(lst.asInstanceOf[HList].value.slice(i1.asInstanceOf[Long].intValue(), i2.asInstanceOf[Long].intValue()))
  }
}
