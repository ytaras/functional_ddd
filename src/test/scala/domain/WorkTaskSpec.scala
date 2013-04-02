package domain

import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

object WorkTaskSpecification extends Properties("WorkTask") with Generators {
  include(new CreatedTaskSpecification)
  include(new StartedTaskSpecification)

}

class CreatedTaskSpecification extends Properties("Created") with Generators {
  property("valid") = forAll(genCreatedTask) { (a: WorkTask) =>
    a.valid == (a.loggedHours == 0)
  }
  property("log hours") =
    forAll(genCreatedTask, arbitrary[Int]) { (a: WorkTask, h: Int) =>
      {a log h} throws classOf[IllegalStateException]
    }
}

class StartedTaskSpecification extends Properties("Started") with Generators {
  property("valid") = forAll(genStartedTask) { (a: WorkTask) =>
    a.valid == (a.loggedHours >= 0)
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

trait Generators {
  val genStatus = oneOf(Created, Started)


  def genTask(stGen: Gen[Status], hGen: Gen[Int]) = for {
    status <- stGen
    loggedHours <- hGen
  } yield new WorkTask(status, loggedHours)

  val genCreatedTask = genTask(value(Created), arbitrary[Int])
  val genStartedTask = genTask(value(Started), arbitrary[Int])
  val genWorkTask = genTask(genStatus, arbitrary[Int])
}
