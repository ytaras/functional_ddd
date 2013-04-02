package domain

case class WorkTask(
  private var _status: Status,
  private var _loggedHours: Int) {
  def status = _status
  def loggedHours = _loggedHours

  def valid = status match {
    case Created => loggedHours == 0
    case Started => loggedHours >= 0
  }
}

sealed trait Status
case object Created extends Status
case object Started extends Status
