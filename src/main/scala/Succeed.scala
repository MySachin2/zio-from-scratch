import Fork.forkedZio
import SucceedAgain.printLine

import scala.util.Random

case class Person(name: String, age: Int)

object Person {
  val peter = Person("Peter", 88)
}

trait ZIOApp {
  def run: ZIO[Any]

  def main(args: Array[String]): Unit = {
    run.run { result =>
      println(f"The result was ${result}")
    }
    //Thread.sleep(3000)
  }
}

object SucceedNow extends ZIOApp {
  val peterZIO: ZIO[Person] = ZIO.succeedNow(Person.peter)

  override def run: ZIO[Person] = peterZIO
}

object SucceedNowUhOh extends ZIOApp {
  val howdyZio: ZIO[Unit] = ZIO.succeedNow(println("Yo"))

  override def run: ZIO[Unit] = ZIO.succeedNow(1)
}

object Succeed extends ZIOApp {
  val howdyZio: ZIO[Unit] = ZIO.succeed(println("Yo"))

  override def run: ZIO[Unit] = ZIO.succeedNow(1)
}

object SucceedAgain extends ZIOApp {
  def printLine(msg: String): ZIO[Unit] = ZIO.succeed(println(msg))

  override def run: ZIO[Unit] = printLine("Yeah")
}

object Zip extends ZIOApp {
  val zippedZio: ZIO[(Int, String)] = ZIO.succeed(19) zip ZIO.succeed("Ninety Six")

  override def run: ZIO[(Int, String)] = zippedZio
}

object Map extends ZIOApp {
  val valZio: ZIO[Int] = ZIO.succeed(20)
  val mappedZio: ZIO[Int] = valZio.map((i) => i * 2)

  override def run: ZIO[Int] = mappedZio
}

object MapUhOh extends ZIOApp {
  val valZio: ZIO[String] = ZIO.succeed("Yeah")
  def printLine(message: String): ZIO[Unit] = ZIO.succeed(println(message))

  override def run: ZIO[ZIO[Unit]] = valZio.map(printLine)
}

object FlatMap extends ZIOApp {
  val valZio: ZIO[String] = ZIO.succeed("Yeah")
  def printLine(message: String): ZIO[Unit] = ZIO.succeed(println(message))

  override def run: ZIO[Unit] = valZio.flatMap(printLine)
}

object ForComp extends ZIOApp {
  val valZio: ZIO[String] = ZIO.succeed("Yeah")
  def printLine(message: String): ZIO[Unit] = ZIO.succeed(println(message))
  val flatMappedZio = for {
    msg <- valZio
    _ <- printLine(msg)
  } yield ()

  override def run: ZIO[Unit] = flatMappedZio
}

object Async extends ZIOApp {
  val asyncZio = ZIO.async[Int] {
    complete => {
      printLine("Team")
      Thread.sleep(1000)
      complete(20)
    }
  }

  override def run: ZIO[Int] = asyncZio
}

object Fork extends ZIOApp {
  val asyncZio = ZIO.async[Int] {
    complete => {
      printLine("Team")
      Thread.sleep(1000)
      complete(Random.nextInt(909))
    }
  }

  def printLine(message: String): ZIO[Unit] = ZIO.succeed(println(message))

  val forkedZio = for {
    fiber <- asyncZio.fork
    fiber2 <- asyncZio.fork
    _ <- printLine("Nice")
    int <- fiber.join
    int2 <- fiber2.join
  } yield f"My Ints ${int}, ${int2}"


  override def run: ZIO[String] = forkedZio
}

object ZipOps extends ZIOApp {

  val zioA: ZIO[String] = ZIO.succeed("-Zoom-")
  val zioB: ZIO[Int] = ZIO.succeed(2)

  val f: (String, Int) => String = (str, i) => str * i

  //override def run: ZIO[(String, Int)] = zioA.zip(zioB)

  //override def run: ZIO[String] = zioA.zipWith(zioB, f)

  override def run: ZIO[Int] = zioA *> zioB
}

object StackSafety extends ZIOApp {
  val zio: ZIO[Unit] = ZIO.succeed(println("Howdy")).repeat(100)

  override def run: ZIO[Unit] = zio
}