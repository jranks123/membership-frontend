package model.Healthcheck

case class BooleanCheck(name: String, exec: () => Boolean) extends HealthCheck {
  override def messages = List(s"Test $name failed, health check will fail")
  override def ok = exec()
}
