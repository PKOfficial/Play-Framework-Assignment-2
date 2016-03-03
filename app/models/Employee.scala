package models

import com.google.inject.ImplementedBy
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by akash on 3/3/16.
  */

case class Employee(id:Int,name:String,address:String,dateOfBirth:String,dateOfJoining:String,designation:String)

@ImplementedBy(classOf[EmployeeService])
trait EmployeeServiceAPi {

  def getAllEmployee:Future[List[Employee]]
  def getEmployee(id:Int):Option[Employee]
  def getEmployeeByName(name:String):List[Employee]
  def deleteEmployee(employee: Employee):Boolean
  def addEmpployee(employee: Employee):Boolean
}

class EmployeeService extends EmployeeServiceAPi{

  val listOfEmployee:ListBuffer[Employee] = ListBuffer(Employee(1,"Akash","A-24, Sec -61,Noida","20/4/1992","20/4/1992","Trainee"),Employee(2,"Prabhat","A-24, Sec - 61,Noida","20/4/1992","20/4/1992","Trainee"))

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
    listOfEmployee.+=(employee)
    true
  }
}