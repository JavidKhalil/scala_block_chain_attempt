package io.qw.rt.sc

sealed trait ConsoleModule[+A]

final case class End[A](value: () => A) extends ConsoleModule[A]

final case class PrintResult[A](line: String, rest: ConsoleModule[A]) extends ConsoleModule[A]

final case class ReadInput[A](rest: String => ConsoleModule[A]) extends ConsoleModule[A]

object ConsoleModule {
   /**
    * Interpreting point of functional effect
    *
    * @param program
    * @tparam A
    * @return
    */
  def interpretator[A](program: ConsoleModule[A]): A = program match {
    case End(value) =>
      value()
    case PrintResult(line, next) =>
      println(line)
      interpretator(next)
    case ReadInput(next) =>
      interpretator(next(scala.io.StdIn.readLine()))
  }

   /**
    * three constructors for effect each effect type, which implementing trait ConsoleModule
    *
    * @param a
    * @tparam A
    * @return
    */
  def succeed[A](a: => A): ConsoleModule[A] = End(() => a)
  def printLine(line: String): ConsoleModule[Unit] =
    PrintResult(line, succeed(()))
  val readLine: ConsoleModule[String] =
    ReadInput(line => succeed(line))


  /**
    * syntax implicit for ConsoleModule
    *
    * @param self
    * @tparam A
    */
  implicit class ConsoleModuleSyntax[+A](self: ConsoleModule[A]) {
    def map[B](f: A => B): ConsoleModule[B] =
      flatMap(a => succeed(f(a)))
    def flatMap[B](f: A => ConsoleModule[B]): ConsoleModule[B] =
      self match {
        case End(value) => f(value())
        case PrintResult(line, next) =>
          PrintResult(line, next.flatMap(f))
        case ReadInput(next) =>
          ReadInput(line => next(line).flatMap(f))
      }
  }

}