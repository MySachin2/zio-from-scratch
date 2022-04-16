import ZIO.succeedNow

import scala.concurrent.ExecutionContext

sealed trait ZIO[+A] {

  def zip[B](that: => ZIO[B]): ZIO[(A, B)] = {
    zipWith(that)((a, b) => (a, b))
  }

  def zipWith[B, C](that: => ZIO[B])(f: (A, B) => C) : ZIO[C] = {
    for {
      a <- this
      b <- that
    } yield f(a, b)
  }
  
  def repeat(n: Int): ZIO[Unit] = {
    if (n <= 0) ZIO.succeedNow(this)
    else this *> repeat(n-1)
  }

  def *>[B](that: => ZIO[B]) = this zipRight that

  def zipRight[B](that: => ZIO[B]): ZIO[B] = {
    zipWith(that)((_, b) => b)
  }

  def map[B](f: A => B): ZIO[B] = {
    flatMap(f andThen succeedNow)
  }

  def flatMap[B](f: A => ZIO[B]): ZIO[B] = {
    ZIO.FlatMap(this, f)
  }

  def fork: ZIO[Fiber[A]] = ZIO.Fork(this)


  final def run(callback: A => Unit): Unit = {

    type Erased = ZIO[Any]
    type Cont = Any => Erased
    type ErasedCallback = Any => Any

    val stack = new scala.collection.mutable.Stack[Cont]()


    def erase[A](zio: ZIO[A]): Erased = zio

    def eraseCallback[A](cb: A => Unit): ErasedCallback = cb.asInstanceOf[ErasedCallback]

    var currentZIO = erase(this)

    var loop = true

    def complete(value: Any): Unit = {
      if(stack.isEmpty) {
        loop = false
        callback(value.asInstanceOf[A])
      }
      else {
        val cont = stack.pop()
        currentZIO = cont(value)
      }
    }

    def resume(): Unit = {
      loop = true
      run()
    }

    def run(): Unit = {
      while (loop) {
        currentZIO match {
          case ZIO.Succeed(value) => complete(value)
          case ZIO.Effect(thunk) => complete(thunk())
          case ZIO.FlatMap(zio, cont) => {
            stack.push(cont)
            currentZIO = zio
          }
          case ZIO.Async(register) => {
            loop = false
            if (stack.isEmpty) {
              register(eraseCallback(callback))
            }
            else {
              register { a =>
                currentZIO = ZIO.succeedNow(a)
                resume()
              }
            }
          }
        }
      }
    }
    run()
  }
}

sealed trait Fiber[+A] {
  def join: ZIO[A]
  def start(): Unit
}

class FiberImpl[A](zio: ZIO[A]) extends Fiber[A] {
  var maybeResult: Option[A] = None
  var callbacks = List.empty[ A => Any]

  override def start(): Unit = {
    ExecutionContext.global.execute(() => {
      zio.run {
        a => maybeResult = Some(a)
          callbacks.foreach((callback) => callback(a))
      }
    })
  }

  override def join: ZIO[A] = {
    maybeResult match {
      case Some(a) => ZIO.succeedNow(a)
      case None => ZIO.async((complete) => {
        callbacks = complete :: callbacks
      })
    }
  }
}

object ZIO {
  def succeedNow[A](value: A): ZIO[A] = ZIO.Succeed(value)
  def succeed[A](value: => A): ZIO[A] = ZIO.Effect(() => value): Effect[A]

  def async[A](register: (A => Any) => Any): ZIO[A] = {
    ZIO.Async(register)
  }
  
  case class Succeed[A](value: A) extends ZIO[A] {
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
  }

  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A,B)] {

  }

  case class Map[A, B](inp: ZIO[A], f: A => B) extends ZIO[B] {

  }

  case class FlatMap[A, B](inp: ZIO[A], f: A => ZIO[B]) extends ZIO[B]

  case class Async[A](register: (A => Any) => Any) extends ZIO[A] {

  }
  
  case class Fork[A](zio: ZIO[A]) extends ZIO[Fiber[A]] {
  }
}
