package domain

import org.scalacheck._
import Prop._

object WorkTaskRepositorySpecification extends Properties("WorkTaskRepository") {
  import WorkTaskGenerators._

  property("saves valid") = forAll(genValidTask) { a: WorkTask =>
    WorkTaskRepository.save(a)
  }

  property("doesnt save invalid") = forAll(genWorkTask suchThat (!_.valid)) {
    a: WorkTask => !WorkTaskRepository.save(a)
  }

  property("saves and loads") = SavingWorkTask
}

object SavingWorkTask extends Commands {
  import WorkTaskGenerators._
  import Gen._
  case class State(tasks: Map[String, WorkTask])
  case class SaveNew(wt: WorkTask) extends Command {
    def nextState(st: State) = st.copy(tasks = st.tasks + (wt.id -> wt))
    def run(st: State) = WorkTaskRepository.save(wt)

    postConditions += {
      case (_, _, true) => true
      case _ => "repository save return false" |: false
    }
  }

  def initialState = State(Map.empty)
  def genSave: Gen[Command] = genValidTask flatMap { t => value(SaveNew(t)) }
  def genCommand(s: State): Gen[Command] = genSave

}
