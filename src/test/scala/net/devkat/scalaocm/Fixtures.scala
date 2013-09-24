package net.devkat.scalaocm

import Ocm._
import Path.root
import java.util.Calendar
import net.devkat.scalaocm.annotations.JcrProperty
import net.devkat.scalaocm.node.Folder

/**
 * Test Record: Company. It has many different field types for test purposes.
 */
class Company extends JcrNode[Company] {

  //override val idField = new LongField(this)

  def jcrName = nodeName

  @JcrProperty var nodeName:String = _
  @JcrProperty var name:String = _
  @JcrProperty var description:String = _
  @JcrProperty var country:Long = _
  @JcrProperty var postCode:String = _
  @JcrProperty var created:Calendar = _
  @JcrProperty var employeeSatisfaction:BigDecimal = _

  //lazy val employees = MySchema.companyToEmployees.left(this)
}

object Company extends Company

/**
 * Test record: An employee belongs to a company.
 */
class Employee extends JcrNode[Employee] {

  @JcrProperty var name:String = _
  @JcrProperty var companyId:Long = _
  @JcrProperty var email:String = _
  @JcrProperty var salary:BigDecimal = _
  @JcrProperty var locale:String = _
  @JcrProperty var timeZone:String = _
  @JcrProperty var password:String = _
  @JcrProperty var photo:Array[Byte] = _
  @JcrProperty var admin:Boolean = _
  @JcrProperty var departmentNumber:Long = _
  @JcrProperty var roles:List[String] = _
  @JcrProperty var rooms:List[String] = _

  lazy val company = parentOf[Company](this)

}

object Employee extends Employee

/**
 * Test record: One or more employees can have a room (one-to-many-relation).
 */
class Room extends JcrNode[Room] {

  @JcrProperty var name:String = _

  //lazy val employees = getReferences[Employee] map { _._1 }
}

object Room extends Room

object CompanySchema extends Schema(
  classOf[Company]) {
  
  import Path._
  import Ocm._

  /**
   * Creates some test instances of companies and employees
   * and saves them in the database.
   */
  def createTestData {
    import TestData._

    /*
    allCompanies.foreach(companies.insert(_))
    allEmployees.foreach(employees.insert(_))
    allRooms.foreach(rooms.insert(_))
    */
    
    e1.rooms = List(r1.identifier, r2.identifier)
    e1.save()
    jcrSession.save()
  }
  
  def cleanup {
    getNodes[Company](root / "first") foreach { _.remove() }
    getNodes[Company](root / "second") foreach { _.remove() }
    getNodes[Company](root / "third") foreach { _.remove() }
    getNodes[Room](root / "rooms") foreach { _.remove() }
  }

  object TestData {

    val c1 = create[Company](root / "first")
    c1.name = "First Company USA"
    c1.created = Calendar.getInstance()

    val c2 = create[Company](root / "second")
    c2.name = "Second Company USA"
    c2.created = Calendar.getInstance()

    val c3 = create[Company](root / "third")
    c3.name = "Company or Employee"
    c3.created = Calendar.getInstance()

    val allCompanies = List(c1, c2, c3)

    lazy val e1 = create[Employee](c1.jcrPath / "peter_example")
    e1.name = "Peter Example"
    e1.email = "peter@example.com"
    e1.salary = BigDecimal(345)
    e1.locale = java.util.Locale.GERMAN.toString
    e1.timeZone = "Europe/Berlin"
    e1.password = "exampletest"
    e1.admin = false
    e1.departmentNumber = 2
    //e1.role(EmployeeRole.Programmer).
    e1.photo = Array[Byte](0, 1, 2, 3, 4)

    lazy val allEmployees = List(e1)

    val rooms = create[Folder](root / "rooms")
    val r1 = create[Room](rooms.jcrPath / "room1")
    r1.name = "Room 1"
    val r2 = create[Room](rooms.jcrPath / "room2")
    r2.name = "Room 2"
    val r3 = create[Room](rooms.jcrPath / "room3")
    r3.name = "Room 3"

    lazy val allRooms = List(r1, r2, r3)
    
  }

}

