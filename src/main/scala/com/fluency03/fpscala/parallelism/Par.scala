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
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
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
    es => {
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

  /**
   * Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
   * We could prevent this using synchronization, but it isn't needed for our purposes here
   * (also, repeated evaluation of pure values won't affect results).
   */
  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
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

//  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
//    if (as.isEmpty) unit(Vector())
//    else if (as.length == 1) map(as.head)(a => Vector(a))
//    else {
//      val (l,r) = as.splitAt(as.length/2)
//      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
//    }
//  }

  // TODO (fluency03): difference of using fork outside vs using fork on recursion as above?
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    as match {
      case IndexedSeq() => unit(Vector())
      case IndexedSeq(one) => map(one)(Vector(_))
      case _ +: _ =>
        val (l, r) = as.splitAt(as.length / 2)
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

  def parallel[IN, ACC](ints: IndexedSeq[IN], init: IN)(g: IN => ACC)(f: (ACC, ACC) => ACC): Par[ACC] =
    if (ints.length <= 1)
      unit(g(ints.headOption getOrElse init))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(parallel(l, init)(g)(f)), fork(parallel(r, init)(g)(f)))(f)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    parallel[Int, Int](ints, Integer.MIN_VALUE)(i => i)(_ max _)

  def countWords(l: List[String]): Par[Int] =
    parallel(l.toIndexedSeq, "") {
      case "" => 0
      case s: String => s.split(" ").length
      case _ => 0
    }(_ + _)

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = ???


  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = ???


  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = ???


  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // Notice we are blocking on the result of cond.
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(run(es)(n).get))

  def choiceByChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 1 else 0))(List(ifFalse, ifTrue))

  def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
    es => run(es)(ps(run(es)(p).get))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es)(choices(run(es)(pa).get))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => run(es)(f(run(es)(a).get))

  def choiceByFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    flatMap(p)(b => if (b) t else f)

  def choiceNByFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(i => choices(i))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinByFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(p => p)

  def flatMapByJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

}



