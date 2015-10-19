package model.Healthcheck

trait HealthCheck {
  def ok: Boolean
  def messages: Seq[String] = Nil
}
