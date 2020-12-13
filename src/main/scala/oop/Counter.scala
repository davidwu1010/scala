package oop

class Counter(val count: Int = 0) {
  def inc: Counter = inc(1)
  def dec: Counter = dec(1)
  def inc(n: Int) = new Counter(count + n)
  def dec(n: Int) = new Counter(count - n)
}
