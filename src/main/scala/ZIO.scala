import scala.concurrent.ExecutionContext

sealed trait ZIO[+A] {

  def zip[B](that: ZIO[B]): ZIO[(A, B)] = {
    zipWith(that)((a, b) => (a, b))
  }

  def zipWith[B, C](that: ZIO[B])(f: (A, B) => C) : ZIO[C] = {
    for {
      a <- this
      b <- that
    } yield f(a, b)
  }
  
  def repeat(n: Int): ZIO[Unit] = {
    if n <= 0 then ZIO.succeedNow(this)
    else this *> repeat(n-1)
  }

  def *>[B](that: ZIO[B]) = this zipRight that

  def zipRight[B](that: ZIO[B]): ZIO[B] = {
    zipWith(that)((_, b) => b)
  }

  def map[B](f: A => B): ZIO[B] = {
    ZIO.Map(this, f)
  }

  def flatMap[B](f: A => ZIO[B]): ZIO[B] = {
    ZIO.FlatMap(this, f)
  }

  def fork: ZIO[Fiber[A]] = ZIO.Fork(this)

   def run(callback: A => Unit): Unit
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
    override def run(callback: A => Unit): Unit = callback(value)
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit = callback(f())
  }

  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A,B)] {
    override def run(callback: ((A,B)) => Unit): Unit = {
      for {
        a <- left
        b <- right
      } yield (a, b)
    }
  }

  case class Map[A, B](inp: ZIO[A], f: A => B) extends ZIO[B] {
    override def run(callback: B => Unit): Unit = {
      inp.run((a)=> {
        callback(f(a))
      })
    }
  }

  case class FlatMap[A, B](inp: ZIO[A], f: A => ZIO[B]) extends ZIO[B] {
    override def run(callback: B => Unit): Unit = {
      inp.run((a)=> {
        f(a).run((b) =>
          callback(b)
        )
      })
    }
  }

  case class Async[A](register: (A => Any) => Any) extends ZIO[A] {
    override def run(callback: A => Unit): Unit = {
      register(callback)
    }
  }
  
  case class Fork[A](zio: ZIO[A]) extends ZIO[Fiber[A]] {
    override def run(callback: Fiber[A] => Unit): Unit = {
      val fiber = new FiberImpl[A](zio)
      fiber.start()
      callback(fiber)
    }
  }
}
