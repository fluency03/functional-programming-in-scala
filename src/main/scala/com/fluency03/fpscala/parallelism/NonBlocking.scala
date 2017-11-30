package com.fluency03.fpscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

/**
 * A representation of Par that doesn’t leak resources this way has to be non-blocking in the sense
 * that the implementations of fork and map2 must never call a method that blocks the current thread
 * like Future.get.
 */
object NonBlocking {

  sealed trait Future[A] {
    /**
     * The apply method is declared private to the fpinscala.parallelism package,
     * which means that it can only be accessed by code within that package.
     */
    private[parallelism] def apply(k: A => Unit): Unit
  }

  // Par looks the same, but we’re using our new non-blocking Future instead of the one in java.util.concurrent.
  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      // A mutable, thread-safe reference to use for storing the result.
      val ref = new AtomicReference[A]
      // A java.util.concurrent.CountDownLatch allows threads to wait until its
      // countDown method is called a certain number of times. Here the countDown method
      // will be called once when we’ve received the value of type A from p,
      // and we want the run implementation to block until that happens.
      val latch = new CountDownLatch(1)
      // When we receive the value, sets the result and releases the latch.
      p(es) { a => ref.set(a); latch.countDown() }
      // Waits until the result becomes available and the latch is released.
      latch.await()
      // Once we’ve passed the latch, we know ref has been set, and we return its value.
      ref.get
    }

    def unit[A](a: A): Par[A] =
      _ => new Future[A] {
        // Simply passes the value to the continuation. Note that the ExecutorService isn’t needed.
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        // eval forks off evaluation of a and returns immediately.
        // The callback will be invoked asynchronously on another thread.
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    // A helper function to evaluate an action asynchronously using some ExecutorService.
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call: Unit = r
      })

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking
     * continuation-passing-style APIs.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] =
      _ => new Future[A] {
        def apply(k: A => Unit): Unit = f(k)
      }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          // Two mutable vars are used to store the two results.
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`

          // An actor that awaits both results, combines them with f, and passes the result to cb.
          val actor = Actor[Either[A, B]](es) {
            // If the A result came in first, stores it in ar and waits for the B.
            // If the A result came last and we already have our B, calls f with both
            // results and passes the resulting C to the callback, cb.
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            // Analogously, if the B result came in first, stores it in br and waits for the A.
            // If the B result came last and we already have our A, calls f with both results
            // and passes the resulting C to the callback, cb.
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }

          // Passes the actor as a continuation to both sides.
          // On the A side, we wrap the result in Left, and on the B side, we wrap it in Right.
          // These are the constructors of the Either data type, and they serve to indicate
          // to the actor where the result came from.
          p(es)(a => actor ! Left(a))
          p2(es)(b => actor ! Right(b))
        }
      }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit = {
          pa(es)(a => eval(es) {cb(f(a)) })
        }
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRecursive[A](ps: List[Par[A]]): Par[List[A]] =
      ps match {
        case Nil => unit(Nil)
        case x :: xs => map2(x, fork(sequenceRecursive(xs)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      as match {
        case IndexedSeq() => unit(Vector())
        case IndexedSeq(one) => map(one)(Vector(_))
        case _ +: _ =>
          val (l, r) = as.splitAt(as.length / 2)
          map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = ???


    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = ???


    def choiceByChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = ???


    def choiceMap[K, V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] = ???


    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = ???


    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = ???


    def choiceByFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] = ???



    def choiceNByFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] = ???


    def join[A](a: Par[Par[A]]): Par[A] = ???


    def joinByFlatMap[A](a: Par[Par[A]]): Par[A] = ???



    def flatMapByJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = ???






  }

}

