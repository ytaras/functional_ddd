package domain

import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

object WorkTaskSpecification extends Properties("WorkTask") with Generators {
  include(new CreatedTaskSpecification)
  include(new StartedTaskSpecification)
  include(new FinishedTaskSpecification)
}

class CreatedTaskSpecification extends Properties("Created") with Generators {
  import CommonProperties._
  property("valid") = forAll(genCreatedTask) { (a: WorkTask) =>
    a.valid == (a.loggedHours == 0)
  }
  property("log hours") = notLoggable(genCreatedTask)
  property("not finishable") = notFinishable(genCreatedTask)
  property("start") = forAll(genCreatedTask) { (a: WorkTask) => (if(a.valid) {
      a.start
      a.status == Started
    } else {
      { a.start } throws classOf[IllegalStateException]
    }): Prop
  }
}

class StartedTaskSpecification extends Properties("Started") with Generators {
  import CommonProperties._
  property("valid") = validIfHoursPositive(genStartedTask)
  property("start") = notStartable(genStartedTask)
  property("finish") = forAll(genStartedTask) { (a: WorkTask) => ( if(a.valid) {
      a.finish
      a.status == Finished
    } else {
      { a.finish } throws classOf[IllegalStateException]
    }):Prop
  }
  property("log hours") = forAll(genStartedTask, arbitrary[Int]) {
    (a: WorkTask, h: Int) => (h match {
      case _ if h <= 0 => {a log h} throws classOf[IllegalArgumentException]
      case _ => {
        val saved = a.loggedHours
        a log h
        a.loggedHours == saved + h
      }
    }): Prop
  }
}

class FinishedTaskSpecification extends Properties("Finished") with Generators {
  import CommonProperties._
  property("valid") = validIfHoursPositive(genFinishedTask)
  property("not startable") = notStartable(genFinishedTask)
  property("not loggable") = notLoggable(genFinishedTask)
  property("not finishable") = notFinishable(genFinishedTask)
}

object CommonProperties extends Generators {
  def validIfHoursPositive(gen: Gen[WorkTask]) =
    forAll(gen) { (a: WorkTask) =>
      a.valid == (a.loggedHours >= 0)
    }
  def notLoggable(gen: Gen[WorkTask]) =
    forAll(gen, arbitrary[Int]) { (a: WorkTask, h: Int) =>
      {a log h} throws classOf[IllegalStateException]
    }
  def notStartable(gen: Gen[WorkTask]) =
    forAll(gen) { (a: WorkTask) =>
      { a.start} throws classOf[IllegalStateException]
    }
  def notFinishable(gen: Gen[WorkTask]) =
    forAll(gen) { (a: WorkTask) =>
      { a.finish } throws classOf[IllegalStateException]
    }
}

trait Generators {
  val genStatus = oneOf(Created, Started, Finished)


  def genTask(stGen: Gen[Status], hGen: Gen[Int]) = for {
    status <- stGen
    loggedHours <- hGen
  } yield new WorkTask(status, loggedHours)

  val genCreatedTask = genTask(value(Created), arbitrary[Int])
  val genStartedTask = genTask(value(Started), arbitrary[Int])
  val genFinishedTask = genTask(value(Finished), arbitrary[Int])
  val genWorkTask = genTask(genStatus, arbitrary[Int])
}
