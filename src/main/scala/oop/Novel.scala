package oop

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge = yearOfRelease - author.year
  def isWrittenBy(author: Writer) = author == this.author
  def copy(year: Int) = new Novel(name, year, author)
}
