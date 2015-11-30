// As a first step, we can try to do the same thing like just before in Python:

sealed trait ResultOrError[+T]
case class Result[+T](value: T) extends ResultOrError[T]
case class Error[+T](error: Exception) extends ResultOrError[T]

object Tests {
  def wrapExpr[T](body: () => T) = {
    try {
      Result(body())
    } catch {
      case e: Exception => Error(e)
    }
  }

  // Try:
  //   scala> wrapExpr(() => 39 + 3)
  //   res0: Product with Serializable with ResultOrError[Int] = Result(42)
  //   
  //   scala> wrapExpr(() => 39 / 0)
  //   res1: Product with Serializable with ResultOrError[Int] = Error(java.lang.ArithmeticException: / by zero)
  //
  // Although, this still doesn't look so much nicer.  Fortunately, the Scala inventors have expected
  // this happening.  Watch this:



  def wrapExpr2[T](body: => T) = {
    try {
      Result(body)
    } catch {
      case e: Exception => Error(e)
    }
  }

  // That leads to:
  //   scala> wrapExpr2(39 / 0)
  //   res2: Product with Serializable with ResultOrError[Int] = Error(java.lang.ArithmeticException: / by zero)
  // Internally, `wrapExpr2` does exactly the same thing as `wrapExpr`; it's just syntactic sugar 
  // around creating an anonymous function and calling it at every usage.  However, we need to be 
  // careful with that behaviour:

  def wrapExpr3(expr: => Int) = {
    try {
      if (expr > 0)
        Result(expr + 40)
      else
        Result(0)
    } catch {
      case e: Exception => Error(e)
    }
  }

  // OK, we can use this as before:
  //   scala> wrapExpr3(10)
  //   res4: Product with Serializable with ResultOrError[Int] = Result(50)
  //   scala> wrapExpr3(-10)
  //   res5: Product with Serializable with ResultOrError[Int] = Result(0)
  // But: strange things happen...
  //   scala> wrapExpr3({println("[Log] 10"); 10})
  //   [Log] 10
  //   [Log] 10
  //   res7: Product with Serializable with ResultOrError[Int] = Result(50)
  //   scala> wrapExpr3({println("[Log] 10"); 10 / 0})
  //     [Log] 10
  //   res10: Product with Serializable with ResultOrError[Int] = Error(java.lang.ArithmeticException: / by zero)

  // Why? Lets desugar this:
  def wrapExpr4(expr: () => Int) = {
    try {
      if (expr() > 0)
        Result(expr() + 40)
      else
        Result(0)
    } catch {
      case e: Exception => Error(e)
    }
  }

  // now, we clearly see that of course the logging will happen twice (or only once, if we 
  // include an error) -- the function gets called twice.
  //   scala> wrapExpr4(() => {println("[Log] 10"); 10})
  //   [Log] 10
  //   [Log] 10
  //   res8: Product with Serializable with ResultOrError[Int] = Result(50)
  // In this version, we see that immediately -- but before, it might be 
  // unclear to the user.
  
  // As a solution, we can use a similar technique to do some lazy caching: a so-called delay
  // object:

  class Delayed[+T](thunk: => T) {
    private[this] var cached: Option[T] = None

    def force: T = cached match {
      case Some(value) => value
      case None => {
        val value = thunk
        cached = Some(value)
        value
      }
    }
  }

  def wrapExpr5(expr: => Int) = {
    val cachedExpr = new Delayed(expr)
    try {
      if (cachedExpr.force > 0)
        Result(cachedExpr.force + 40)
      else
        Result(0)
    } catch {
      case e: Exception => Error(e)
    }
  }

  // That solves the problem:
  //   scala> wrapExpr5({println("[Log] 10"); 10 / 0})
  //   [Log] 10
  //   res4: Product with Serializable with ResultOrError[Int] = Error(java.lang.ArithmeticException: / by zero)
  //   scala> wrapExpr5({println("[Log] 10"); 10})
  //   [Log] 10
  //   res5: Product with Serializable with ResultOrError[Int] = Result(50)

  // Still, it does look even worse than the variants before.  Fortunately, there is again
  // some help by the language:

  def wrapExpr6(expr: => Int) = {
    lazy val cachedExpr = expr
    try {
      if (cachedExpr > 0)
        Result(cachedExpr + 40)
      else
        Result(0)
    } catch {
      case e: Exception => Error(e)
    }
  }

  // Note the usage of the `lazy val`.  This is in fact only sugar to something like the 
  // `Delayed` object created above.  We could neither replace this directly by a `val` (since
  // then we would loose non-strictness), nor by a `def` (since then we would have the double 
  // evaluation problem again) -- that is the reason why `lazy val` exists as an extra construct.
}


