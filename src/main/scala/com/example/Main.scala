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

case class CounterFriend(myName: String, count: Int)

object CounterFriend {
  import cats.free.{ Free => Free_}

  sealed trait Algebra[A]
  case class Greet(name: String) extends Algebra[Unit]
  case class Name() extends Algebra[String]
  case class Count() extends Algebra[Int]

  type Free[A] = Free_[Algebra, A]

  import cats.free.Free.liftF

  def greet(yourName: String): Free[Unit] =
    liftF[Algebra, Unit](Greet(yourName))

  def name: Free[String] =
    liftF[Algebra, String](Name())

  def count: Free[Int] =
    liftF[Algebra, Int](Count())

  import cats.data.{ State => State_ }

  type State[A] = State_[CounterFriend, A]

  // mixed purity
  val compiler: Algebra ~> State = new (Algebra ~> State) {
    def apply[A](fa: Algebra[A]): State[A] =
      fa match {
        case Greet(yourName) =>
          State_.modify(cf => {
            println(s"Hello ${yourName}, my name is ${cf.myName}!")
            // TODO - find a better way to pass Counter state
            // cf.count should not need to be called
            CounterFriend(
              cf.myName,
              Counter.add1.flatMap(Unit => Counter.count).foldMap(Counter.compiler).runA(cf.count).value
            )
          })
        case Name() =>
          State_.inspect(_.myName)
        case Count() =>
          State_.inspect(_.count)
      }
  }
}

//
// To run these examples:
//
// shell> sbt console
//
// scala> import com.example.Main
// import com.example.Main
//
// scala> Main.friendRun
// Hello Bob, my name is Alice!
// res1: cats.Id[String] = Alice
//
// scala> Main.counterRun
// res2: Int = 2
//
// scala> Main.counterFriendRun
// Hello Alice, my name is Bob!
// Hello Carol, my name is Bob!
// res3: Int = 2
//
object Main {
  val alice: Friend = new Friend("Alice")

  def friendProgram: Friend.Free[String] =
    for {
      _ <- alice.greet("Bob")
      n <- alice.name
    } yield n

  def friendRun = friendProgram.foldMap(alice.compiler)

  def counterProgram: Counter.Free[Int] =
    for {
      _ <- Counter.add1
      _ <- Counter.add1
      n <- Counter.count
    } yield n

  // Note that the pure approach is a unique in that the state is simply an Int
  // as opposed to a Counter containing an Int
  def counterRun = counterProgram.foldMap(Counter.compiler).runA(0).value

  val bob: CounterFriend = CounterFriend("Bob", 0)

  def counterFriendProgram: CounterFriend.Free[Int] =
    for {
      _ <- CounterFriend.greet("Alice")
      a <- CounterFriend.count
      _ <- CounterFriend.greet("Carol")
      b <- CounterFriend.count
    } yield b

  def counterFriendRun = counterFriendProgram.foldMap(CounterFriend.compiler).runA(bob).value
}
