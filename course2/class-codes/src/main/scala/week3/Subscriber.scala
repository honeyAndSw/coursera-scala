package week3

trait Subscriber {
  def handler(pub: Publisher)
}
