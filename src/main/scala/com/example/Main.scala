package com.example

import cats._

object Counter {
  import cats.free.{ Free => Free_}

  sealed trait Algebra[A]
  case class Add1() extends Algebra[Unit]
  case class Count() extends Algebra[Int]

  type Free[A] = Free_[Algebra, A]

  import cats.free.Free.liftF

  def count: Free[Int] =
    liftF[Algebra, Int](Count())

  def add1: Free[Unit] =
    liftF[Algebra, Unit](Add1())

  import cats.data.{ State => State_ }

  type State[A] = State_[Int, A]

  // pure
  val compiler: Algebra ~> State = new (Algebra ~> State) {
    def apply[A](fa: Algebra[A]): State[A] =
      fa match {
        case Add1() => State_.modify(_ + 1)
        case Count() => State_.get
      }
  }
}

object Friend {
  import cats.free.{ Free => Free_}

  sealed trait Algebra[A]
  case class Greet(name: String) extends Algebra[Unit]
  case class Name() extends Algebra[String]

  type Free[A] = Free_[Algebra, A]
}

class Friend(myName: String) {
  import cats.free.Free.liftF

  def greet(yourName: String): Friend.Free[Unit] =
    liftF[Friend.Algebra, Unit](Friend.Greet(yourName))

  def name: Friend.Free[String] =
    liftF[Friend.Algebra, String](Friend.Name())

  // impure
  def compiler: Friend.Algebra ~> Id  =
    new (Friend.Algebra ~> Id) {
      def apply[A](fa: Friend.Algebra[A]): Id[A] =
        fa match {
          case Friend.Greet(yourName) =>
            println(s"Hello ${yourName}, my name is ${myName}!")
            ()
          case Friend.Name() =>
            myName
        }
    }
}

object Main {
  val alice: Friend = new Friend("Alice")

  def program1: Friend.Free[String] =
    for {
      _ <- alice.greet("Bob")
      n <- alice.name
    } yield n

  def run1 = program1.foldMap(alice.compiler)

  def program2: Counter.Free[Int] =
    for {
      _ <- Counter.add1
      _ <- Counter.add1
      n <- Counter.count
    } yield n

  def run2 = program2.foldMap(Counter.compiler).runA(0).value
}
