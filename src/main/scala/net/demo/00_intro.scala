package net.demo

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import zio._
import zio.duration._

/*
 * INTRODUCTION
 *
 * _Functional Design_ is a technique for modeling solutions to problems in some
 * domain using _immutable values_, with a small but rich set of primitives
 * for constructing, transforming, and composing solutions in order to solve
 * even the most complex problems easily, with high testability and
 * reasonability.
 *
 * Functional Design embraces the composability, immutability, and static
 * type safety afforded by functional programming, and applies it to solve
 * real world problems, close to business domains, all without the esoteric
 * type classes, category theory jargon, confusing and undiscoverable implicits,
 * monad transformers, and other constructs common in the early days of
 * functional Scala.
 *
 * Functional Design has been used for a number of open source Scala libraries,
 * including ZIO. The techniques provide very powerful, concise, and type-safe
 * solutions to many complex problems within given domains.
 *
 * In this section, you will get a taste for the kind of expressive power,
 * compositionality, declarativity, and type-safety that Functional Design
 * can provide.
 */

object tour {

  object effect {

    /** The shard function creates N workers reading from a Queue, if one of
      * them fails, then wait for the other ones to process their current item,
      * but terminate all the workers.
      *
      * Returns the first error, or never return, if there is no error.
      */
    def shard[A](
      queue: Queue[A],
      count: Int,
      worker: A => Task[Unit]
    ): ZIO[Any, Throwable, Nothing] /*UIO[Throwable]*/ = {
      //Task[Nothing]
      val qworker: Task[Nothing] /* ZIO[Any, Throwable, Nothing]*/ =
        ZIO.uninterruptible {
          for {
            a <- ZIO.interruptible(queue.take) //to prevent sticking here
            _ <- worker(a) //.onError(_ => queue.offer(a)) //onError is a binary op
          } yield ()
        }.forever

      // ZIO[Any, Throwable, Nothing]
      val qworkers: List[ZIO[Any, Nothing, Throwable]] = //List[Task[Throwable]] =
        List.fill(count)(qworker.flip)

      (for {
        fiber <- ZIO.forkAll(qworkers) //If one of them fails, the others will be interrupted instantaneously.
        list  <- fiber.join //wait for all of them to finish
      } yield list.head).flip
    }

    def publishForever(
      queue: Queue[Long],
      seqNum: Long = 0L
    ): ZIO[Any with zio.clock.Clock, Nothing, Unit] =
      queue
        .offer(seqNum)
        .timeout(zio.duration.Duration(5, TimeUnit.SECONDS)) //couldn't publish a message for 5 sec, consider it is done
        .flatMap {
          case None =>
            UIO(
              println(s"${Thread.currentThread.getName}: timeout")
            ) *> queue.shutdown //anyone who takes or offers will be interrupted
          case Some(timeout) =>
            if (timeout)
              UIO(println(s"${Thread.currentThread.getName}: offered:$seqNum")) *> publishForever(queue, seqNum + 1L)
            else UIO(println(s"${Thread.currentThread.getName}: re-offered:$seqNum")) *> publishForever(queue, seqNum)
        }

    val parallelism = 1 << 3
    val schedule    = Schedule.fixed(5.seconds) && Schedule.recurs(ThreadLocalRandom.current().nextInt(8))

    for {
      //_ <- queue.offer(ThreadLocalRandom.current().nextInt(0, 50)).forever.fork
      //_ <- queue.take.flatMap().forever.fork
      queue <- Queue.bounded[Long](parallelism)

      pub <- publishForever(queue).fork

      shards <- shard(
        queue,
        2,
        { ind: Long =>
          Task {
            ZIO
              .effectTotal(println(s"$ind. Now:${System.currentTimeMillis} "))
              .repeat(schedule)
          }
        }
      )

      /*_ <- ZIO.foreachPar(List(1,2))(i => shard(queue, i, { ind: Int => Task {
         ZIO
           .effectTotal(println(s"$ind. Now:${System.currentTimeMillis} "))
            .repeat(schedule)
        }
      })).fork*/

      _ <- pub.join *> UIO(println("Exit publisher"))
      _ <- queue.awaitShutdown
    } yield ()

  }

  object schedule {
    type Response

    /** Constructs a schedule that will use exponential falloff, starting from
      * 10 milliseconds, until the delay becomes 60 seconds, when it will switch
      * to using a fixed spacing of 60 seconds between recurrences, until it
      * reaches 100 recurrences at that spacing, at which point the schedule
      * will no longer recur.
      */
    val schedule =
      (Schedule.exponential(10.millis).whileOutput(_ < 60.seconds) andThen
        (Schedule.fixed(60.seconds) && Schedule.recurs(100))).jittered

    def flakyRequest(url: String): Task[Response] = ???

    lazy val example = flakyRequest("https://google.com").retry(schedule)
  }
}
