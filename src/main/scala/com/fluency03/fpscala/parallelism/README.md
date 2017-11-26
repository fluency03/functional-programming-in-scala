# CHAPTER 7: Purely functional parallelism

- not only how to write a library for purely functional parallelism, but how to approach the problem of designing a purely functional library.
- IndexedSeq is a superclass of random-access sequences like Vector in the standard library. Unlike lists, these sequences provide an efficient splitAt method for dividing them into two parts at a particular index.
    
    ```java
    trait Runnable { def run: Unit }
    
    class Thread(r: Runnable) {
      def start: Unit
      def join: Unit
    }
    ```
    
- Problems of `Runnable` and `Thread`:
  - None of the methods return a meaningful value. If we want to get any information out of a `Runnable`, it has to have some side effect, like mutating some state that we can inspect.
  - `Thread` maps directly onto operating system threads, which are a scarce resource. It would be preferable to create as many “logical threads” as is natural for our problem, and later deal with mapping these onto actual OS threads.
- It can be handled by:

    ```java
    class ExecutorService {
      def submit[A](a: Callable[A]): Future[A]
    }
    trait Future[A] {
      def get: A
    }
    ```
- Good abstracting over physical threads, but they are still in lower level of abstraction.
- A call to `Future.get`, for example, blocks the calling thread until the `ExecutorService` has finished executing it, and its API provides no means of composing futures.
- We can see that `unit` has a definite side effect, but only *with regard* to `get`.
- That is, `unit` simply returns a `Par[Int]` in this case, representing an asynchronous computation. But as soon as we pass that `Par` to `get`, we explicitly wait for it, exposing the side effect.
- We want to be able to combine asynchronous computations without waiting for them to finish.


- A function like `fork` solves the problem of instantiating our parallel computations too strictly, but more fundamentally it puts the parallelism explicitly under programmer control. 
  - we need some way to indicate that the results of the two parallel tasks should be combined;
  - we have the choice of whether a particular task should be performed asynchronously;
- We know we want fork to signal that its argument gets evaluated in a separate logical thread.

- In functional programming it’s easy, and expected, to factor out common functionality into generic, reusable components that can be composed. Side effects hurt compo- sitionality, but more generally, any hidden or out-of-band assumption or behavior that prevents us from treating our components (be they functions or anything else) as black boxes makes composition difficult or impossible.
- You can try to fix your implementation such that the law holds, or you can refine your law a bit, to state more explicitly the conditions under which it holds (you could simply stipulate that you require thread pools that can grow unbounded).
- Even this is a good exercise — it forces you to document invariants or assumptions that were previously implicit.
- The essential problem with the current representation `Par.scala` is that we can’t get a value out of a `Future` without the current thread blocking on its get method.

**non-blocking Par**

- An `A => Unit`? Such a function can only be useful for executing some side effect using the given `A`, as we certainly aren’t using the returned result. 
- Are we still doing functional programming in using a type like `Future`? Yes, but we’re making use of a common technique of using side effects as an implementation detail for a purely functional API.
- Side effects employed can’t be observed by outside code.
- It should be noted that `run` blocks the calling thread while waiting for the latch. It’s not possible to write an implementation of run that doesn’t block.
- Since it needs to return a value of type `A`, it has to wait for that value to become available before it can return.
- We want users of our API to avoid calling run until they definitely want to wait for a result.
- We could even go so far as to remove run from our API altogether and expose the apply method on `Par` instead so that users can register asynchronous callbacks. 








