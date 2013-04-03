package domain

trait Repository[Id, Value <: AggregateRoot[Id]] {
  private var map: Map[Id, Value] = Map.empty

  def save(wt: Value): Boolean = if (!wt.valid)
      false
      else { map += (wt.id -> wt); true }
  def load(id: Id): Option[Value] = map.get(id)
}

trait AggregateRoot[Id] {
  def id: Id
  def valid: Boolean
}

