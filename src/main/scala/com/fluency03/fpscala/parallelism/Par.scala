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
    (es: ExecutorService) => es.submit(new Callable[A] {
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
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  /**
   * run extracts a value from a Par by actually performing the computation.
   * since Par is represented by a function that needs an ExecutorService,
   * the creation of the Future doesn’t actually happen until this ExectorService is provided.
   */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // TODO (fluency03): whether this is ok?
  def parMapByLazyUnit[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    lazyUnit(ps.map(f))

  // TODO (fluency03): difference betweeen using foldLeft and foldRight?
  def sequenceByFoldLeft[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(List[A]()))((acc: Par[List[A]], in: Par[A]) => map2(in, acc)(_ :: _))

  def sequenceByFoldRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((in: Par[A], acc: Par[List[A]]) => map2(in, acc)(_ :: _))

  def sequenceRecursive[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(Nil)
      // TODO (fluency03): why using fork here?
      case x :: xs => map2(x, fork(sequenceRecursive(xs)))(_ :: _)
    }

  // TODO (fluency03): difference of using fork outside vs using fork on recursion as above?
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    as match {
      case IndexedSeq() => unit(IndexedSeq())
      case IndexedSeq(one) => map(one)(IndexedSeq(_))
      case x +: xs =>
        val (l,r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  // TODO (fluency03): why fork plus asyncF?
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // TODO (fluency03): whether this is ok?
  def parFilterByLazyUnit[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    lazyUnit(as.filter(f))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get








}

/**
 * Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
 * We could prevent this using synchronization, but it isn't needed for our purposes here
 * (also, repeated evaluation of pure values won't affect results).
 */
case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
  @volatile var cache: Option[C] = None
  def isDone: Boolean = cache.isDefined
  def isCancelled: Boolean = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean): Boolean =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get: C = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime
      val aTime = stop - start
      val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}