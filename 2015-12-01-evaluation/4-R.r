// Our standard example, again. R already uses thunks for function parameters, no
// no need to wrap anything:
wrapExpr <- function(body) {
  tryCatch(
    list(ok=body), 
    error = function(e) list(error=e)
  )
}

// Now we can retry our results:
//   > wrapExpr(log(42))
//   $ok
//   [1] 3.73767
//   > wrapExpr(log("42")) // using logarithm on strings, since we don't want NaN but an error
//   $error
//   <simpleError in log("42"): non-numeric argument to mathematical function>
// So this behaves as "expected". But -- R uses proper thunks, not only functions, 
// for parameter passing, so we get "thunk caching" for free:

wrapExpr2 <- function(i) {
  tryCatch(
    if (i > 0) list(ok=(i+40)) else list(ok=0),
    error = function(e) list(error=e)
  )
}

wrong <- function() {
  print("hello")
  log("bla")
}

// So: `wrong()` is only evalated once in the call, and thus, "Logged" appears only once!
// > wrapExpr2(wrong())
// [1] "hello"
// $error
// <simpleError in log("bla"): non-numeric argument to mathematical function>



// But it gets better: in an R thunk (called "promise"), not only the closure, but the actual
// expression is stored...

assert <- function(condition) {
  condition.expr <- deparse(substitute(condition))
  if (!condition) {
    stop(sprintf("Assertion '%s' violated!", condition.expr))
  }   
}

// > assert(2 + 5 == 5)
// Error in assert(2 + 5 == 5) : Assertion '2 + 5 == 5' violated!

// We observe two things: that we can do what we want; and that the language already does that.
// Using that, we can restore the non-caching behaviour of Scala by-name args:

wrapExpr3 <- function(i) {
  i.expr <- substitute(i)
  forced.i <- function() { 
    eval(i.expr)
  }
  tryCatch(
    if (forced.i() > 0) list(ok=(forced.i()+40)) else list(ok=0),
    error = function(e) list(error=e)
  )
}

// Which results in:
//  > wrapExpr3({print("hello"); 42})
//  [1] "hello"
//  [1] "hello"
//  $ok
//  [1] 82

// Although there are some weird things going on:
twice3 <- function(i) {
  eval.subst <- function() eval(substitute(i))

  print("zero")
  substitute(i)
  print("one")
  substitute(i)
  print("two")
  eval(substitute(i))
  print("three")
  eval(substitute(i))
  print("four")
  eval.subst()
  print("five")
  eval.subst()
  print("six")
  i
}

// Which leads to this:
//   > twice3({print("hi"); 10})
//   [1] "one"
//   [1] "two"
//   [1] "hi"
//   [1] "three"
//   [1] "hi"
//   [1] "four"
//   [1] "hi"
//   [1] "five"
//   [1] "six"
//   [1] 10
// I have no explanation for the last part; see http://stackoverflow.com/q/34006356/1346276.




// Now, having access to the passed-in experssions, we could do weirder things -- this amounts
// to a system combining call-by-need and (runtime?) macros:

//   > q <- quote(2 + 2)
//   > q[[1]] <- `-`
//   > q
//   .Primitive("-")(2, 2)
//   > eval(q)
//   [1] 0

// And this form of evaluation is used very often in the libraries (which was the reason 
// the functionality was included at all). For example, it is common to pass so-called 
// formala object; one of the most frequent examples of this is the `lm` function, which fits
// a linear model:
//   > df <- data.frame(weight=seq(100), diameter=2*seq(100) + rnorm(100, sd=5), length = seq(100)*rnorm(100, mean=10, sd=10))
//   > lm(weight ~ diameter + length, data=df)
//   Call:
//   lm(formula = weight ~ diameter + length, data = df)
//   Coefficients:
//   (Intercept)     diameter       length  
//      0.729708     0.491137     0.000794  

// This supports special syntax in the passed expression, based on analyzing its literal
// contents:
//   > lm(weight ~ diameter + length - 1, data=df)
//   Call:
//   lm(formula = weight ~ diameter + length - 1, data = df)
//   Coefficients:
//     diameter      length  
//    0.5019645  -0.0004001 

