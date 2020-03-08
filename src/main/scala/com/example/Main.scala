package com.example

import cats._

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

  def impureCompiler: Friend.Algebra ~> Id  =
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
  import cats._
  import cats.free.Free
  import cats.free._
  import cats.data._
  import cats.implicits._

  val alice: Friend = new Friend("Alice")

  def program: Friend.Free[String] =
    for {
      _ <- alice.greet("Bob")
      n <- alice.name
    } yield n

  def run = program.foldMap(alice.impureCompiler)
}
