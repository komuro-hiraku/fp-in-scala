package chapter4

object ValidatedPerson {

  def mkName(name: String): Validated[String, Name] =
    if ( name == "" || name == null)
      Invalid(List("Name is empty"))
    else
      Valid(new Name(name))

  def mkAge(age: Int): Validated[String, Age] =
    if ( age < 0) Invalid(List("Age is out of range"))
    else Valid(new Age(age))

  /**
    * Person生成関数。ただしnameもageも不正だった場合、nameのエラーしか報告されない（ageの評価が行われないため）
    * @param name
    * @param age
    * @return
    */
  def mkPerson(name: String, age: Int) : Validated[String, Person] =
    Validated.map2(mkName(name), mkAge(age))(Person(_, _))
}


