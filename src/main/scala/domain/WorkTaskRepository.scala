package domain

object WorkTaskRepository {
  private var map: Map[String, WorkTask] = Map.empty
  def save(wt: WorkTask): Boolean = if (!wt.valid)
      false
      else { map += (wt.id -> wt); true }
  def load(id: String): Option[WorkTask] = map.get(id)
}
