package com.perevillega.freemonad

import java.util.UUID

import cats.data.{Coproduct, Xor}
import cats.free.Free._
import cats.free.{Free, Inject}
import cats.std.list._
import cats.syntax.traverse._
import cats.syntax.xor._
import cats.{Id, ~>}

//Object Orders contains a Free Monad example with some comments to explain what are we doing as we go
//Warning: this is not aimed to deep category theory knowledge, its focus is use cases
object Orders {
  // Define some alias so the code looks more readable later
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
  type Payload = String

  // Simple scenario: language to buy/sell stock, so we create our `Orders` language.
  // We start from methods buy/sell, and we transform the signatures to case classes (looks same but without any meaning associated)
  sealed trait Orders[A]

  case class Buy(stock: Symbol, amount: Int) extends Orders[Response]

  case class Sell(stock: Symbol, amount: Int) extends Orders[Response]

  // we define the language, but we can't use it as a monad. The following won't compile:
  // > for { stock <- ListStocks() } yield stock
  // we need to lift the DSL into a monad

  //First create free monad type (cats does the magic here)
  type OrdersF[A] = Free[Orders, A]

  //Then lift to monad (boilerplate, similar pattern for most implementations)
  def buy(stock: Symbol, amount: Int): OrdersF[Response] =
    liftF[Orders, Response](Buy(stock, amount))

  def sell(stock: Symbol, amount: Int): OrdersF[Response] =
    liftF[Orders, Response](Sell(stock, amount))

  // We now have monads. Proof, we can flatMap them!
  val flatMapThat = buy("APPL", 100).flatMap(rsp => sell("GOOG", 100))

  //We can now build our program with extremely complex business logic
  val smartTrade: OrdersF[Response] = for {
    _ <- buy("APPL", 50)
    _ <- buy("MSFT", 10)
    rsp <- sell("GOOG", 200)
  } yield rsp

  // But this does nothing, by itself. We compile a for-comprehension, but it still has no logic associated
  // How to do something with it?
  // We need an interpreter that tells the code what to do. An interpreter is a natural transformation to Monad.
  // Ignore `natural trasnformation`, it's not relevant right now, assume it's magic.
  // What is the simplest Monad we know of? Id Monad. So we'll use it.
  // Note: on each case we need to return an element of the type we expect as 'result', so `Response` or equivalent.
  def orderPrinter: Orders ~> Id =
    new (Orders ~> Id) {
      def apply[A](fa: Orders[A]): Id[A] = fa match {
        case ListStocks() => // don't worry about this case by now, keep reading!
          println(s"Getting list of stocks: FB, TWTR")
          // Warning: if we use NIL here, the for comprehension will fall onto the failure side!
          // Same with Xor.Left and other values that may `fail fast` in a for-comprehension. Be careful!
          List("FB", "TWTR")
        case Buy(stock, amount) =>
          println(s"Buying $amount of $stock")
          "ok"
        case Sell(stock, amount) =>
          println(s"Selling $amount of $stock")
          "ok"
      }
    }

  // If we execute this code (see Main object) we will see the `println` output.
  // Note that we decide what happens with our code inside the interpreter.
  // If you remember, you define the actions of a Monad in its flatMap method (intrinsic behaviour of the Monad).
  // Think of the interpreter (natural transformation) as something similar.

  //In a proper system our interpreter is most likely to use stuff like `Xor` for results
  //But our Natural transformation requires Monads `with one hole` so we have to `cheat` a bit by creating a
  //support type with only `one hole`
  type ErrorOr[A] = Xor[String, A]

  def xorInterpreter: Orders ~> ErrorOr =
    new (Orders ~> ErrorOr) {
      def apply[A](fa: Orders[A]): ErrorOr[A] = fa match {
        case ListStocks() =>
          List("FB", "TWTR").right
        case Buy(stock, amount) =>
          s"$stock - $amount".right
        case Sell(stock, amount) =>
          "Why are you selling that?".left
      }
    }

  //If we execute this code (see Main object) we see it works and the for-comprehension fails on Sell (Xor.left as result)

  // Let's extend our language by adding a new method `listStock` that returns `List[Stock]`
  // Implement case class and lift to Free
  case class ListStocks() extends Orders[List[Symbol]]

  def listStocks(): OrdersF[List[Symbol]] =
    liftF[Orders, List[Symbol]](ListStocks())

  // But the code below doesn't work!
  /*
   val smartTradeWithList: Free[Orders, String] = for {
    st <- listStocks()
    _ <- buy(st, 100)
    rsp <- sell("GOOG", 100)
  } yield rsp
   */

  // We hit a problem: We return a Monad (OrdersF[List[Symbol]]) when we call `listStocks`
  // So the for-comprehension doesn't iterate over elements of the list (Order[List[Stock]]), but returns the full list
  // as the left side of the comprehension. But we want to iterate over the list, how to do that?
  // Confession: got a bit stuck. Solution obtained thanks to Julien Truffaut - https://twitter.com/julientruffaut
  // We need to use `traverseU` in here
  // Julien says: List has an instance of Traverse and `OrdersF ` has an instance of `Applicative` because it is a Free monad
  // Traverse will iterate over the list and apply `buy` on each step

  // That is the right fix and enables us to do:
  val smartTradeWithList: Free[Orders, String] = for {
    st <- listStocks()
    _ <- st.traverseU(buy(_, 100))
    rsp <- sell("GOOG", 100)
  } yield rsp

  // ok, we have this working. Our DSL works and it uses a language that can be understood by business people
  // (you can add some method to hide the `traverseU` syntax if needed)
  // Beer time? :)


  // Let's grow our application a bit. If we want to go to production, we need to add Logs to it.
  // But logging, that's a second language completely different than `Orders`!
  // Let's define it:
  sealed trait Log[A]

  case class Info(msg: String) extends Log[Unit]

  case class Error(msg: String) extends Log[Unit]

  //We are in `Free` world, to let's lift `Log` to `Free`
  type LogF[A] = Free[Log, A]

  def info(msg: String): LogF[Unit] =
    liftF[Log, Unit](Info(msg))

  def error(msg: String): LogF[Unit] =
    liftF[Log, Unit](Error(msg))

  //And let's add logs to our trading logic
  /*
    val smartTradeWithLogs = for {
    _ <- info("I'm going to trade smartly")
    _ <- buy("APPL", 100)
    _ <- info("I'm going to trade even more smartly")
    _ <- buy("MSFT", 100)
    rsp <- sell("GOOG", 100)
    _ <- error("Wait, what?!")
  } yield rsp
  */
  // Wait, this doesn't build! Why? Ah, we have 2 different monads, which flatMap doesn't like.
  // Remember the signature of flatMap:  def flatMap(a: F[A])(f: A => F[B]): F[B]
  // We can't change our Type F in the middle of flatMap
  // But we want to use both Free in our code, otherwise it won't be too useful. Let's fix that

  // We use some smart constructors to create a monad that can be combined with others
  // This is an alternative to the `old way` of using `liftF` to lift the case class into Free.
  // Both can coexist together, no problem or you can use only one (depends on scenario)
  class OrderI[F[_]](implicit I: Inject[Orders, F]) {
    def buyI(stock: Symbol, amount: Int): Free[F, Response] = Free.inject[Orders, F](Buy(stock, amount))

    def sellI(stock: Symbol, amount: Int): Free[F, Response] = Free.inject[Orders, F](Sell(stock, amount))
  }

  // We need this implicit to convert to the proper instance when required
  implicit def orderI[F[_]](implicit I: Inject[Orders, F]): OrderI[F] = new OrderI[F]

  // We repeat the process with Log. It's kind of *boilerplate*, another way to lift case class to Free
  class LogI[F[_]](implicit I: Inject[Log, F]) {
    def infoI(msg: String): Free[F, Unit] = Free.inject[Log, F](Info(msg))

    def errorI(msg: String): Free[F, Unit] = Free.inject[Log, F](Error(msg))
  }

  //another implicit necessary to convert to the proper instance
  implicit def logI[F[_]](implicit I: Inject[Log, F]): LogI[F] = new LogI[F]

  // Ok, so we have a way to lift both languages into Free.
  // Now we need to be able to use both interpreters at once. As per Cats documentation, CoProducts to the rescue
  // CoProduct is outside scope, sorry!
  type TradeApp[A] = Coproduct[Orders, Log, A]

  // Now let's define our business logic again. Note that now we need to use the classes we defined to lift to Monad
  // Also note we pass the CoProduct as parameter to that class. This will trigger a type-resolution chain
  // (via Inject[Log, F] and Inject[Orders, F] that will make this work
  def smartTradeWithLogs(implicit O: OrderI[TradeApp], L: LogI[TradeApp]): Free[TradeApp, Response] = {
    import L._
    import O._

    // Look, ma, both monads at once!
    for {
      _ <- infoI("I'm going to trade smartly")
      _ <- buyI("APPL", 100)
      _ <- infoI("I'm going to trade even more smartly")
      _ <- buyI("MSFT", 100)
      rsp <- sellI("GOOG", 100)
      _ <- errorI("Wait, what?!")
    } yield rsp
  }

  // Ok, it builds! But who's interpreting Log? We didn't create any interpreter!
  // We can fix that. Let's build an interpreter from Log to Id, as before, that will use println for output
  def logPrinter: Log ~> Id =
    new (Log ~> Id) {
      def apply[A](fa: Log[A]): Id[A] = fa match {
        case Info(msg) => println(s"[Info] - $msg")
        case Error(msg) => println(s"[Error] - $msg")
      }
    }

  // Now we have 2 interpreters, both from a Monad to Id.
  // We can compose the interpreters we want to use so the program knows what to do according to the Monad found
  // As we defined our composite Free Monad as a CoProduct (TradeApp) we declare a Natural Transformation
  // from TradeApp ~> Id.
  // The implementation can use Natural Transformation `or` method to compose our existing printers
  // Note that although we are creating a new interpreter, we are reusing existing ones to do so!
  def composedInterpreter: TradeApp ~> Id = orderPrinter or logPrinter

  // With this, we can run it. See Main object at the bottom of the file :)
  // What have we achieved? We have mixed 2 different business languages into our program, each one with its own interpreter
  // We are reusing the interpreters defined for each individual language, so we have not increased the surface area
  // affected by implementation changes in an interpreter
  // And we can still swap interpreters as required (for testing, etc)

  // 2 languages is nice. But what if we want more than just 2 languages? Let's see how it would work.
  // Let's say our api, being related to stocks, needs auditing. Not logging, auditing, a new language.
  // So we define a new language (the details themselves are irrelevant, we care about mixing them)
  sealed trait Audit[A]

  case class UserActionAudit(user: UserId, action: String, values: List[Values]) extends Audit[Unit]

  case class SystemActionAudit(job: JobId, action: String, values: List[Values]) extends Audit[Unit]

  // We build another class to lift to a Monad
  class AuditI[F[_]](implicit I: Inject[Audit, F]) {
    def userAction(user: UserId, action: String, values: List[Values]): Free[F, Unit] = Free.inject[Audit, F](UserActionAudit(user, action, values))

    def systemAction(job: JobId, action: String, values: List[Values]): Free[F, Unit] = Free.inject[Audit, F](SystemActionAudit(job, action, values))
  }
  // implicit still necessary
  implicit def auditI[F[_]](implicit I: Inject[Audit, F]): AuditI[F] = new AuditI[F]

  // And we add a basic interpreter to Id that prints to console, as usual
  def auditPrinter: Audit ~> Id =
    new (Audit ~> Id) {
      def apply[A](fa: Audit[A]): Id[A] = fa match {
        case UserActionAudit(user, action, values) => println(s"[USER Action] - user $user called $action with values $values")
        case SystemActionAudit(job, action, values) => println(s"[SYSTEM Action] - $job called $action with values $values")
      }
    }

  // Up to here, all as expected. But now we have to define the CoProduct, and... we have a problem!
  // CoPodruct is Coproduct[F[_], G[_], A] so how do we mix Orders, Log, and Audit? They are 3 languages for 2 slots!
  // What can we do?
  // Oh, wait, we defined `TradeApp[A]` as a CoProduct of `Audit` and `Log`, but `TradeApp[A]`... is a Monad! (a Free Monad)
  // So we could potentially use that as one of the *holes* of the CoProduct, and `Audit` as the other.
  // Will it work? Let's try
  type AuditableTradeApp[A] = Coproduct[Audit, TradeApp, A]
  // NOTE: in this CoProduct (AuditableTradeApp) the order of the elements inside the CoProduct matters, our old CoProduct `TradeApp[A]`
  // MUST be on the G[_] side for the to compile. Otherwise, the implicits defined by Cats in object 'InjectInstances' won't match the types
  // and the compiler won't construct the implicits we need to compile all this.

  // We do the same with the natural transformation, we chain our new `auditPrinter` and our previous `composedInterpreter`
  // (that resolves the TradeApp CoProduct).
  // Order matters due to `or` types (see its implementation to understand why)
  def auditableInterpreter: AuditableTradeApp ~> Id = auditPrinter or composedInterpreter

  // To make sure this not only compiles but works, we build a program that uses all the elements
  // We need to add all the implicits in here and pass our new CoProduct as parameter
  def smartTradeWithAuditsAndLogs(implicit O: OrderI[AuditableTradeApp], L: LogI[AuditableTradeApp], A: AuditI[AuditableTradeApp]): Free[AuditableTradeApp, Response] = {
    import A._
    import L._
    import O._

    for {
      _ <- infoI("I'm going to trade smartly")
      _ <- userAction("ID102", "buy", List("APPL", "100"))
      _ <- buyI("APPL", 200)
      _ <- infoI("I'm going to trade even more smartly")
      _ <- userAction("ID102", "buy", List("MSFT", "100"))
      _ <- buyI("MSFT", 100)
      _ <- userAction("ID102", "sell", List("GOOG", "100"))
      rsp <- sellI("GOOG", 300)
      _ <- systemAction("BACKOFFICE", "tradesCheck", List("ID102", "lastTrades"))
      _ <- errorI("Wait, what?!")
    } yield rsp
  }

  // If you run Main you'll see this works and we see the expected output.
  // As you can see chaining several languages becomes slightly verbose (need to chain several CoProducts)
  // On the other hand, you have to define that only once, and hte interpreters are still using pre-existing
  // natural transformations, so it's another case of *define once, use many times*. And it works.
  // There may be other ways to achieve, but if that's so I don't know them. If you do please let me know.

  // So, at this stage, we have several dsl working together. Quite neat.
  // A logical next step is to apply that pattern to different levels of abstraction in our application.
  // For example, we can assume our interpreters will need to interact with some 3rd party system, via http or messages or some other way.
  // Also, it's very likely we will have several systems that use the same communication mechanism (rest, pub-sub, etc)
  // We may want to abstract that communication layer into a language in itself, so we can work with it (and test it) in isolation
  // and then, when we use it in our program, we'll be more confident about it, and we can replace implementation details
  // (say, move from apache http to http4s) modifying only 1 class (the natural transformation) and knowing things will work the same

  // Let's create an example. Let's assume we interact with a messaging system, now that Kafka and Event-Sourcing are
  // in vogue (simplistic, of course! We only care about integration right now)
  sealed trait Messaging[A]
  case class Publish(channelId: ChannelId, source: SourceId, messageId: MessageId, payload: Payload) extends Messaging[Response]
  case class Subscribe(channelId: ChannelId, filterBy: Condition) extends Messaging[Payload]

  // Define the Free Monad as usual
  type MessagingF[A] = Free[Messaging, A]

  // And create support methods to lift to Free, using the `old` way we saw at the beginning
  def publish(channelId: ChannelId, source: SourceId, messageId: MessageId, payload: Payload): MessagingF[Response] =
    liftF[Messaging, Response](Publish(channelId, source, messageId, payload))

  def subscribe(channelId: ChannelId, filterBy: Condition): MessagingF[Payload] =
    liftF[Messaging, Payload](Subscribe(channelId, filterBy))

  // And, as usual, let's add a simple interpreter to Id that uses println
  def messagingPrinter: Messaging ~> Id =
    new (Messaging ~> Id) {
      def apply[A](fa: Messaging[A]): Id[A] = fa match {
        case Publish(channelId, source, messageId, payload) =>
          println(s"Publish [$channelId] From: [$source] Id: [$messageId] Payload: [$payload]")
          "ok"
        case Subscribe(channelId, filterBy) =>
          val payload = "Event fired"
          println(s"Received message from [$channelId] (filter: [$filterBy]): [$payload]")
          payload
      }
    }

  // A small program to prove both the language and interpreter work. You can see it running in Main object, below.
  val testMessagingInterpreter: MessagingF[Payload] = for {
    _ <- publish("BBC", "Sherlock", UUID.randomUUID().toString, "Run Moriarty")
    _ <- publish("BBC", "Adler", UUID.randomUUID().toString, "Sherlocked")
    payload <- subscribe("BBC", "Sherlock")
  } yield payload

  // So we have the language, now we want to force `Orders` to use this new language to define its behaviour. How can we?
  // Remember that a natural transformation is from a Monad F to a Monad G. And Free Monads are Monads.
  // So we can transform from one Free to another, via an interpreter!
  // NOTE: Remember that the second member in an interpreter MUST be a monad (or foldMap will fail as it requires evidence of Monad)
  def orderToMessageInterpreter: Orders ~> MessagingF =
    new (Orders ~> MessagingF) {
      def apply[A](fa: Orders[A]): MessagingF[A] = {
        fa match {
          case ListStocks() =>
            // yes, we cna use a for-comprehension inside the natural transformation if our interpreter requires
            // several steps. It works :)
            for {
              _ <- publish("001", "Orders", UUID.randomUUID().toString, "Get Stocks List")
              payload <- subscribe("001", "*")
            } yield List(payload)
          case Buy(stock, amount) =>
            publish("001", "Orders", UUID.randomUUID().toString, s"Buy $stock $amount")
          case Sell(stock, amount) =>
            publish("001", "Orders", UUID.randomUUID().toString, s"Sell $stock $amount")
        }
      }
    }

  // Now let's define a chain of interpreters for our Orders. We want to define a program that only uses Orders
  // (see `smartTrade` above, our first program) and we want our interpreters to display the process Orders -> Messaging -> Id
  //
  // It is quite straightforward, although we need a small bridge between MessagingF and Messaging for our interpreters.
  // Remember the second member of a natural transformation MUST be a Monad (MessagingF) but our existing `messagingFreePrinter`
  // is from `Messaging` (trait, not Free Monad) to Id.
  // It's easy to define a new natural transformation `MessagingF ~> Id` that reuses `messagingFreePrinter` underneath, as the one
  // we define below. By reusing `messagingPrinter` we don't introduce additional risk (new logic) and we fix the issue.
  def messagingFreePrinter: MessagingF ~> Id =
    new (MessagingF ~> Id) {
      def apply[A](fa: MessagingF[A]): Id[A] = fa.foldMap(messagingPrinter)
    }

  // This works, see example in Main object, below.
  def ordersToTerminalViaMessage: Orders ~> Id = orderToMessageInterpreter andThen messagingFreePrinter


  // The above works, great.
  // Now we want to build an interpreter from AuditableTradeApp to Id, which also uses the middle transformation from Orders to Messaging.
  // With this we will have everything we built above in use: 4 languages (Orders, Log, Audit, Messaging) and `Orders` implemented as
  // a series of `Messaging` actions (higher level defined as lower-level actions)
  // We are also reusing the elements we already implemented, which guarantees certain stability

  // For this step we need to define two new interpreters. The reason is that to define `Orders` as a series of `Messaging`
  // we need to extend `composedInterpreter` (see above). From its existing implementation, the branch that goes to `logPrinter`
  // is ok as is, but `orderPrinter` goes directly from `Order` to `Id`, and we want to go via `Messaging`.
  // So we define a new interpreter that fixes that by replacing `orderPrinter` with `ordersToTerminalViaMessage`, which we defined above
  def composedViaMessageInterpreter: TradeApp ~> Id = ordersToTerminalViaMessage or logPrinter

  // For the next CoProduct, AuditableTradeApp, We can use the existing `auditPrinter` to match Audit steps as we have not modified
  // this part. We use the `composedViaMessageInterpreter` we defined above to to manage all the other cases (Orders and Log)
  // Note: there may be better ways to manage all these interpreters that use CoProducts but, if so, I don't know how.
  // Given they are reusing existing interpreters via composition, this way doesn't sound like such a bad thing
  def auditableToTerminalViaMessage: AuditableTradeApp ~> Id = auditPrinter or composedViaMessageInterpreter

  // And you can see this working in Main object, below.

  // Ta-da!
  // With this we have the pieces we may need to use Free Monads to implement our business logic
  // Some steps may seem 'boilerplate', and they may be, but it's stuff that we define just once and we can reuse a lot.
  // In fact in this example, all our interpreters are built on top of the 3 basic ones, by composition.
  // This reduces the areas where we can introduce bugs and facilitates testing as well as replacing logic with minimal impact
}


// Below you can see use cases for all the different programs and interpreters built above
object Main extends App {

  import Orders._

  println(s"> Smart trade - program - $smartTrade")
  println()
  println(s"> Smart trade - printer - ${smartTrade.foldMap(orderPrinter)}")
  println(s"> Smart trade - xorInterpreter - ${smartTrade.foldMap(xorInterpreter)}")
  println()
  println(s"> Smart trade - smartTradeWithList - ${smartTradeWithList.foldMap(orderPrinter)}")
  println()
  println(s"> Smart trade - smartTradeWithLogs - ${smartTradeWithLogs.foldMap(composedInterpreter)}")
  println()
  println(s"> Smart trade - smartTradeWithAuditsAndLogs - ${smartTradeWithAuditsAndLogs.foldMap(auditableInterpreter)}")
  println()
  println(s"> Messaging layer - test messaging layer - ${testMessagingInterpreter.foldMap(messagingPrinter)}")
  println()
  println(s"> Smart trade - messaging Interpreter - ${smartTrade.foldMap(orderToMessageInterpreter)}")
  println()
  println(s"> Smart trade - messaging Interpreter + printer - ${smartTrade.foldMap(ordersToTerminalViaMessage)}")
  println()
  println(s"> Smart trade - smartTradeWithAuditsAndLogs + messaging Interpreter + printer - ${smartTradeWithAuditsAndLogs.foldMap(auditableToTerminalViaMessage)}")
  println()
}