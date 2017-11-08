# CHAPTER 4 Handling errors without exceptions

- Throwing exceptions is a side effect.
- Exceptions break referential transparency.
- We can represent failures and exceptions with ordinary values, and we can write higher-order functions that abstract out common patterns of error handling and recovery.
- The meaning of RT expressions *does not depend on context* and may be reasoned about locally, whereas the meaning of non-RT expressions is *context-dependent* and requires more global reasoning.
- Two main problems with exceptions:
  - *Exceptions break RT and introduce context dependence*, moving us away from the simple reasoning of the substitution model and making it possible to write confusing exception-based code. This is the source of the folklore advice that exceptions should be used only for error handling, not for control flow.
  - *Exceptions are not type-safe*. Exceptions may occur, and the compiler will certainly not force callers of a function to make a decision about how to handle those exceptions. If we forget to check for an exception in this function, this won’t be detected until runtime.
- The primary benefit of exceptions: they allow us to *consolidate and centralize error-handling logic*, rather than being forced to distribute this logic throughout our codebase.
- Why do not return `null`?
  - It allows errors to silently propagate — the caller can forget to check this condition and won’t be alerted by the compiler, which might result in subsequent code not working properly. Often the error won’t be detected until much later in the code.
  - Besides being error-prone, it results in a fair amount of boilerplate code at call sites, with explicit if statements to check whether the caller has received a "real" result. This boilerplate is magnified if you happen to be calling several functions, each of which uses error codes that must be checked and aggregated in some way.
  - It’s not applicable to polymorphic code. For some output types, we might not even have a sentinel value of that type even if we wanted to! Consider a function like `max`, which finds the maximum value in a sequence according to a custom comparison function: `def max[A](xs:Seq[A])(greater:(A,A)=>Boolean): A`. If the input is empty, we can’t invent a value of type `A`. Nor can `null` be used here, since `null` is only valid for non-primitive types, and `A` may in fact be a primitive like `Double` or `Int`.
  - It demands a special policy or calling convention of callers — proper use of the function would require that callers do something other than call mean and make use of the result. Giving functions special policies like this makes it difficult to pass them to higher-order functions, which must treat all arguments uniformly.
- Why not provide fallback function?
  - It requires that immediate callers have direct knowledge of how to handle the undefined case and limits them to the returning type. What if mean is called as part of a larger computation and we’d like to abort that computation if mean is undefined? Or perhaps we’d like to take some completely different branch in the larger computation in this case? Simply passing an fallback parameter doesn’t give us this freedom.
- `Option` has two cases: it can be defined, in which case it will be a `Some`, or it can be undefined, in which case it will be `None`.







