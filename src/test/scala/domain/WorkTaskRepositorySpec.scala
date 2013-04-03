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
  case class State(tasks: Map[String, WorkTask], loadedId: String = null)
  case class SaveNew(wt: WorkTask) extends Command {
    def nextState(st: State) = st.copy(tasks = st.tasks + (wt.id -> wt))
    def run(st: State) = WorkTaskRepository.save(wt)

    postConditions += {
      case (_, _, true) => true
      case _ => "repository save return false" |: false
    }
  }
  case class LoadExisting(rndNum: Int) extends Command {   
    
    private def key(map: Map[String, _]): String = {
      val index = rndNum % map.size
      map.keys.toList(index)
    }
    def nextState(st: State) = {
      st.copy(loadedId = key(st.tasks))
    }
    def run(st: State) = WorkTaskRepository.load(key(st.tasks))
    preConditions += {
      case State(map, _) => !map.isEmpty
    }

    postConditions += {
      case (_, State(map, id), wt: WorkTask) => map(id) == wt
    }
  }

  def initialState = State(Map.empty)
  def genSave: Gen[Command] = genValidTask flatMap { t => value(SaveNew(t)) }
  def genLoad: Gen[Command] = posNum[Int] flatMap { t => value(LoadExisting(t))}
  def genCommand(s: State): Gen[Command] = oneOf(genSave, genLoad)

}
