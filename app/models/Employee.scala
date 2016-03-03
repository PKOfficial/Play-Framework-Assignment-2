package models

import com.google.inject.ImplementedBy
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._

/**
  * This contain The Case Class of Employee
  * A Trait for Method Declaration
  * Google Juice
  * class that implement the trait
  */

case class Employee(id:Int,name:String,address:String,dateOfBirth:String,dateOfJoining:String,designation:String)

@ImplementedBy(classOf[EmployeeService])
trait EmployeeServiceAPi {

  def getAllEmployee:Future[List[Employee]]
  def getEmployee(id:Int):Option[Employee]
  def getEmployeeByName(name:String):List[Employee]
  def deleteEmployee(employee: Employee):Boolean
  def addEmpployee(employee: Employee):Boolean
  def updateEmployee(employee:Employee):Boolean
}

class EmployeeService extends EmployeeServiceAPi{

  val listOfEmployee:ListBuffer[Employee] = ListBuffer(
    Employee(1,"Akash Sethi","A-24, Sec -61,Noida","27/04/1992","20/04/2015","Trainee"),
    Employee(2,"Prabhat Kashyap","A-24, Sec - 61,Noida","11/06/1992","20/04/2015","Trainee"),
    Employee(3,"Martin Odersky","A-24, Sec - 61,Noida","27/04/1960","21/01/2008","Instructor"),
    Employee(4,"John Marucs","XYZ-22, Twin Towers, NYC","20/4/1992","20/04/2015","VP"))

  override def getAllEmployee: Future[List[Employee]] = Future{

    listOfEmployee.toList
  }

  override def getEmployee(id: Int): Option[Employee] = {

    def local(list:List[Employee]):Option[Employee] = {
      list match{
        case head::tail if head.id == id => Some(head)
        case head::tail  => local(tail)
        case Nil => None
      }
    }
    local(listOfEmployee.toList)
  }

  override def getEmployeeByName(name: String): List[Employee] = {

    def local(list:List[Employee],listByName:List[Employee]):List[Employee] = {
      list match{
        case head::tail if head.name == name => local(tail,head::listByName)
        case head::tail  => local(tail,listByName)
        case Nil => listByName
      }
    }
    local(listOfEmployee.toList,List())
  }

  override def deleteEmployee(employee: Employee): Boolean = {

    listOfEmployee.-=(employee)
    true
  }

  override def addEmpployee(employee: Employee): Boolean = {
    if(getEmployee(employee.id).isDefined){
      false
    }
    else {
      listOfEmployee.+=(employee)
      true
    }
  }

  override def updateEmployee(employee: Employee): Boolean = {
    listOfEmployee foreach { emp =>
      listOfEmployee.-=(emp)
      if(emp.id == employee.id){
        listOfEmployee.+=(employee)
      }
      else {
        listOfEmployee.+=(emp)
      }
    }
    true
  }

}
