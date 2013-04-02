package domain

case class WorkTask(
  private var _status: Status,
  private var _loggedHours: Int) {
  def status = _status
  def loggedHours = _loggedHours

  def valid = status match {
    case Created => loggedHours == 0
    case Started | Finished => loggedHours >= 0
  }

  def log(hours: Int): Unit = status match {
    case Started if hours <= 0 => throw new IllegalArgumentException
    case Started               => _loggedHours += hours
    case _                     => throw new IllegalStateException
  }

  def start = if(valid && status == Created)
      _status = Started
    else
      throw new IllegalStateException

  def finish: Unit = status match {
    case Started if valid => _status = Finished
    case _ => throw new IllegalStateException
  }
}

sealed trait Status
case object Created extends Status
case object Started extends Status
case object Finished extends Status
