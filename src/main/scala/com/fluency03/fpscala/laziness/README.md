# CHAPTER 5: Strictness and laziness

- A function is *non-strict* (or, less formally, *lazy*) just means that the function may choose not to evaluate one or more of its arguments.
- Even though if is a built-in language construct in Scala, it can be thought of as a function accepting three parameters:
  - a condition of type Boolean;
  - an expression of some type A to return in the case that the condition is true;
  - another expression of the same type A to return if the condition is false;
- An argument that’s passed unevaluated to a function will be evaluated once for each place it’s referenced in the body of the function. That is, Scala won’t (by default) cache the result of evaluating an argument.
- Adding the *lazy* keyword to a *val* declaration will cause Scala to delay evaluation of the right-hand side of that *lazy val* declaration until it’s first referenced. It will also cache the result so that subsequent references to it don’t trigger repeated evaluation.
- As a final bit of terminology, we say that a non-strict function in Scala takes its arguments *by name* rather than *by value*.
- Laziness lets us separate the description of an expression from the evaluation of that expression.
- The `unfold` function is an example of what’s sometimes called a *corecursive* function.
- Whereas a recursive function consumes data, a corecursive function *produces* data.

















