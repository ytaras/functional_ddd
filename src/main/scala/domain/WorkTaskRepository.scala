package domain

object WorkTaskRepository {
  def save(a: WorkTask): Boolean = a.valid
}
