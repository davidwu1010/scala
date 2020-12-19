package method

object Method extends App {

}

class Person(val name: String, favoriteMovie: String, val age: Int = 0) {
  def likes(movie: String): Boolean = movie == favoriteMovie
  def +(person: Person): String = s"${this.name} is hanging out with ${person.name}"
  def +(nickname: String): Person = new Person(s"$name ($nickname)", favoriteMovie)
  def unary_+ : Person = new Person(name, favoriteMovie, age + 1)
  def unary_! : String = s"$name, what the heck?!"
  def isAlive: Boolean = true
  def apply: String = s"Hi, my name is $name and I like $favoriteMovie"
  def apply(n: Int): String = s"$name watched $favoriteMovie $n times"
  def learns(thing: String): String = s"$name learns $thing"
  def learnsScala: String = this learns "Scala"
}