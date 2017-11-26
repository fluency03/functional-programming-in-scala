package com.fluency03.fpscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

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

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???





  }

}

