package com.fluency03.fpscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  /**
   * Creates a computation that immediately results in the value a.
   * unit promotes a constant value to a parallel computation.
   *
   * unit is represented as a function that returns a UnitFuture,
   * which is a simple implementation of Future that just wraps a constant value.
   * It doesn’t use the ExecutorService at all. It’s always done and can’t be cancelled.
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def get[A](a: Par[A]): A = ???

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  /**
   * map2 combines the results of two parallel computations with a binary function.
   *
   * map2 doesn’t evaluate the call to f in a separate logical thread, in accord with our design choice
   * of having fork be the sole function in the API for controlling parallelism. We can always do
   * fork(map2(a,b)(f)) if we want the evaluation of f to occur in a separate thread.
   *
   * This implementation of map2 does not respect timeouts. It simply passes the ExecutorService on to
   * both Par values, waits for the results of the Futures af and bf, applies f to them, and wraps them
   * in a UnitFuture. In order to respect timeouts, we’d need a new Future implementation that records
   * the amount of time spent evaluating af, and then subtracts that time from the available time
   * allocated for evaluating bf.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  /**
   * fork marks a computation for concurrent evaluation. The evaluation won’t actually occur until forced by run.
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  /**
   * lazyUnit wraps its unevaluated argument in a Par and marks it for concurrent evaluation.
   *
   * This is the simplest and most natural implementation of fork, but there are some problems with it —
   * for one, the outer Callable will block waiting for the “inner” task to complete. Since this blocking
   * occupies a thread in our thread pool, or whatever resource backs the ExecutorService, this implies
   * that we’re losing out on some potential parallelism. Essentially, we’re using two threads when one
   * should suffice. This is a symptom of a more serious problem with the implementation.
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * run extracts a value from a Par by actually performing the computation.
   * since Par is represented by a function that needs an ExecutorService,
   * the creation of the Future doesn’t actually happen until this ExectorService is provided.
   */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)





  def asyncF[A,B](f: A => B): A => Par[B] = ???






}
