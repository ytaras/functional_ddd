package domain

import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import WorkTaskGenerators._

object WorkTaskSpecification extends Properties("WorkTask") {
  include(new CreatedTaskSpecification)
  include(new StartedTaskSpecification)
  include(new FinishedTaskSpecification)
}

class WorkTaskProperties(n: String, val generator: Gen[WorkTask]) extends
  Properties(n) with WorkTaskCommonProperties

class CreatedTaskSpecification extends WorkTaskProperties("Created", genCreatedTask) {
  property("valid") = forAll(genCreatedTask) { (a: WorkTask) =>
    a.valid == (a.loggedHours == 0)
  }
  property("log hours") = notLoggable
  property("not finishable") = notFinishable
  property("start") = forAll(genCreatedTask) { (a: WorkTask) => a.valid match {
    case true =>
      a.start
      (a.status == Started): Prop
    case false => { a.start } throws classOf[IllegalStateException]
    }
  }
}

class StartedTaskSpecification extends WorkTaskProperties("Started", genStartedTask) {
  property("valid") = validIfHoursPositive
  property("not startable") = notStartable
  property("finish") = forAll(genStartedTask) { (a: WorkTask) => a.valid match {
    case true =>
      a.finish
      (a.status == Finished): Prop
    case false => { a.finish } throws classOf[IllegalStateException]
    }
  }
  property("log hours") = forAll(genStartedTask, arbitrary[Int]) {
    (a: WorkTask, h: Int) => h match {
      case _ if h <= 0 => {a log h} throws classOf[IllegalArgumentException]
      case _ =>
        val saved = a.loggedHours
        a log h
        (a.loggedHours == saved + h): Prop
    }
  }

}

class FinishedTaskSpecification extends WorkTaskProperties("Finished", genFinishedTask) {
  property("valid") = validIfHoursPositive
  property("not startable") = notStartable
  property("not loggable") = notLoggable
  property("not finishable") = notFinishable
}

trait WorkTaskCommonProperties {
  def generator: Gen[WorkTask]
  def validIfHoursPositive =
    forAll(generator) { (a: WorkTask) =>
      a.valid == (a.loggedHours >= 0)
    }
  def notLoggable =
    forAll(generator, arbitrary[Int]) { (a: WorkTask, h: Int) =>
      {a log h} throws classOf[IllegalStateException]
    }
  def notStartable =
    forAll(generator) { (a: WorkTask) =>
      { a.start } throws classOf[IllegalStateException]
    }
  def notFinishable =
    forAll(generator) { (a: WorkTask) =>
      { a.finish } throws classOf[IllegalStateException]
    }
}

object WorkTaskGenerators {
  val genStatus = oneOf(Created, Started, Finished)

  def genTask(stGen: Gen[Status], hGen: Gen[Int]) = for {
    status <- stGen
    loggedHours <- hGen
  } yield new WorkTask(status, loggedHours)

  val genCreatedTask = genTask(value(Created), arbitrary[Int])
  val genStartedTask = genTask(value(Started), arbitrary[Int])
  val genFinishedTask = genTask(value(Finished), arbitrary[Int])
  val genWorkTask = genTask(genStatus, arbitrary[Int])
  val genValidTask = oneOf(
    genTask(value(Created), value(0)),
    genTask(value(Started), posNum[Int]),
    genTask(value(Finished), posNum[Int])
  )
}
