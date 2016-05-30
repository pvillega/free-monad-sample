/*
 * Copyright 2016 Pere Villega
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.perevillega.freemonad

import java.util.UUID

import cats.free.Free
import cats.{Id, ~>}
import cats.std.list._
import cats.syntax.traverse._
import freek._

// We will re-implement our full language, as defined in OrdersSample.scala, in this file
// We will do so using the FreeK library (https://github.com/ProjectSeptemberInc/freek)
// This will help us compare these different ways of building a program using Free
//
// Notice we use `import freek._` at the top to import everything we may need. Feel free(k)
// (pun intended) to make that import more restrictive
//
// IMPORTANT: check `build.sbt` for the line that imports compiler plugin `si2712fix-plugin`
// Without that compiler plugin you will get errors due to implicits not being resolved
// The SI-2712 fix has been integrated to Scala 2.12 so it won't be needed in the future
object FreeKSample extends App {
  // Before we start with the code, I'd recommend you reading the Readme file of FreeK
  // at https://github.com/ProjectSeptemberInc/freek/blob/master/README.md as it provides
  // a lot of insight on motivation and the use case

  // Some type alias to start, so we keep the code similar
  type Symbol = String
  type Response = String
  type UserId = String
  type JobId = String
  type Action = String
  type Values = String
  type SourceId = String
  type MessageId = String
  type ChannelId = String
  type Condition = String
  type Payload = List[Symbol]

  // Let's define the languages as per the pattern in the README of FreeK
  // where we create an object and define a DSL language in it
  object Orders {
    sealed trait DSL[A]
    final case class ListStocks() extends DSL[List[Symbol]]
    final case class Buy(stock: Symbol, amount: Int) extends DSL[Response]
    final case class Sell(stock: Symbol, amount: Int) extends DSL[Response]
  }

  // Defining the interpreter for Order, slightly differently
  object OrderInterpreter extends (Orders.DSL ~> Id) {
    import Orders._

    def apply[A](a: Orders.DSL[A]) = a match {
      case ListStocks() =>
        println(s"Getting list of stocks: FB, TWTR")
        List("FB", "TWTR")
      case Buy(stock, amount) =>
        println(s"Buying $amount of $stock")
        "ok"
      case Sell(stock, amount) =>
        println(s"Selling $amount of $stock")
        "ok"
    }
  }

  // Log dsl
  object Log {
    sealed trait DSL[A]
    final case class Info(msg: String) extends DSL[Unit]
    final case class Error(msg: String) extends DSL[Unit]
  }

  // Defining the interpreter for Log
  object LogInterpreter extends (Log.DSL ~> Id) {
    import Log._

    def apply[A](a: Log.DSL[A]) = a match {
      case Info(msg) =>
        println(s"[Info] - $msg")
      case Error(msg) =>
        println(s"[Error] - $msg")
    }
  }

  // Audit dsl
  object Audit {
    sealed trait DSL[A]
    final case class UserAction(user: UserId, action: String, values: List[Values]) extends DSL[Unit]
    final case class SystemAction(job: JobId, action: String, values: List[Values]) extends DSL[Unit]
  }

  // Audit interpreter
  object AuditInterpreter extends (Audit.DSL ~> Id) {
    import Audit._

    def apply[A](a: Audit.DSL[A]) = a match {
      case UserAction(user, action, values) =>
        println(s"[USER Action] - user $user called $action with values $values")
      case SystemAction(job, action, values) =>
        println(s"[SYSTEM Action] - $job called $action with values $values")
    }
  }

  // Messaging dsl
  object Messaging {
    sealed trait DSL[A]
    final case class Publish(channelId: ChannelId, source: SourceId, messageId: MessageId, message: String) extends DSL[Response]
    final case class Subscribe(channelId: ChannelId, filterBy: Condition) extends DSL[Payload]
  }

  // Messaging interpreter
  object MessagingInterpreter extends (Messaging.DSL ~> Id) {
    import Messaging._

    def apply[A](a: Messaging.DSL[A]) = a match {
      case Publish(channelId, source, messageId, message) =>
        println(s"Publish [$channelId] From: [$source] Id: [$messageId] Payload: [$message]")
        "ok"
      case Subscribe(channelId, filterBy) =>
        val payload = "Event fired"
        println(s"Received message from [$channelId] (filter: [$filterBy]): [$payload]")
        List(payload)
    }
  }

  // Now we recreate our program
  object Program {
    // we import the objects so we have access to the case classes defined in them
    import Audit._
    import Log._
    import Orders._

    // We define the type of our program. Traditionally this would be a series of Coproduct.
    // As FreeK documentation explains, the operator :|: and FXNil are all
    // part of a specialized implementation of Shapeless Coproduct for higher-kinded structures
    type PRG[A] = (Log.DSL :|: Audit.DSL :|: Orders.DSL :|: FXNil)#Cop[A]

    // We define our program.
    // Note that the return type is Free[PRG, A]
    // where PRG is the type we defined above, the Coproduct, that contains all our DSLs
    // and A is the return type of our program, in our case `Response`.
    //
    // Also note that we are not using any support method, but our case classes as defined in the objects
    // for our DSLs.
    // The addition at the end, `.freek[PRG]`, is the part that does the magic of lifting our DSL into a Free
    // This hides all the boilerplate code we had to create in `OrdersSample.scala` to generate Free we could compose
    val program: Free[PRG, Response] = for {
      _ <- Info("I'm going to trade smartly").freek[PRG]
      _ <- UserAction("ID102", "buy", List("APPL", "100")).freek[PRG]
      _ <- Buy("APPL", 200).freek[PRG]
      _ <- Info("I'm going to trade even more smartly").freek[PRG]
      _ <- UserAction("ID102", "buy", List("MSFT", "100")).freek[PRG]
      _ <- Buy("MSFT", 100).freek[PRG]
      _ <- UserAction("ID102", "sell", List("GOOG", "100")).freek[PRG]
      rsp <- Sell("GOOG", 300).freek[PRG]
      _ <- SystemAction("BACKOFFICE", "tradesCheck", List("ID102", "lastTrades")).freek[PRG]
      _ <- Error("Wait, what?!").freek[PRG]
    } yield rsp

    // In our original program we also defined a program that had to use `TraverseU` due to a
    // return type in the Free Monad being a list of item. Let's create the same program using
    // FreeK. `TraverseU` will work as method `.freeK[A]` returns a Monad we can use in traverse
    val programWithList: Free[PRG, Response] = for {
      st <- ListStocks().freek[PRG]
      _ <- st.traverseU(Buy(_, 100).freek[PRG])
      rsp <- Sell("GOOG", 100).freek[PRG]
    } yield rsp


    // We define our interpreter from PRG to Id.
    // We can do that by composing the interpreters to Id we have defined for each DSL
    // NOTE: for it to build you need an interpreter for each element of PRG and in the same order as defined for PRG
    val interpreter: Interpreter[PRG, Id] = LogInterpreter :|: AuditInterpreter :|: OrderInterpreter

    // Now we replicate the interpreter between Orders and Messaging that we had defined originally
    // so our Orders can be defined in terms of Messaging operations
    // Note: the implementation of ListStocks() is not exactly the same as we had originally, as
    // we have replaced the for-comprehension by direct calls to the case classes
    object OrdersToMessagesInterpreter extends (Orders.DSL ~> Messaging.DSL) {
      import Orders._
      import Messaging._

      def apply[A](a: Orders.DSL[A]) = a match {
        case ListStocks() =>
          //TODO: this needs a bit more work to make it fully equivalent
          Publish("001", "Orders", UUID.randomUUID().toString, "Get Stocks List")
          Subscribe("001", "*")
        case Buy(stock, amount) =>
          Publish("001", "Orders", UUID.randomUUID().toString, s"Buy $stock $amount")
        case Sell(stock, amount) =>
          Publish("001", "Orders", UUID.randomUUID().toString, s"Sell $stock $amount")
      }
    }

    // We also define a second interpreter that will run orders via an Order-to-Message interpreter
    // Note how we use `compose` from `NaturalTransformation` to achieve this step and replace our `OrderInterpreter`
    val interpreterWithMessaging: Interpreter[PRG, Id] =
      LogInterpreter :|: AuditInterpreter :|: (MessagingInterpreter compose OrdersToMessagesInterpreter)

  }

  // We expose our programs to interpret them
  import Program._

  // Run the program with our interpreter
  println(s"Use interpreter on `program`: ${program.foldMap(interpreter.nat)}")
  println()

  // Run the program including a list of stocks with our interpreter
  println(s"Use interpreter on `program with List`: ${programWithList.foldMap(interpreter.nat)}")
  println()

  // Run the program with our interpreter that transforms Orders to Messages
  println(s"Use interpreter with Messaging on `program`: ${program.foldMap(interpreterWithMessaging.nat)}")
  println()

  // Ta-da! It works. And much less boilerplate than in the 'pure Cats' version.
  // Yes, a lot is happening behind the scenes with implicits and such, and it may
  // fell like 'magic', but with it being type-safe magic... it's fine :)

}
