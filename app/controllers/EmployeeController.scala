package controllers

import models.{Employee, EmployeeService}
import play.api.data.Form
import play.api.mvc.{Action, Controller}
import play.api.data.Forms._
import com.google.inject.Inject
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import play.api.i18n.Messages.Implicits._
import play.api.Play.current

/**
  * This Controller contails all the action needed
  * This controller contains functinallity for
  * - show DashBoard
  * - Process Search Form
  * - Show Add Employee
  * - Process Add Employee
  * - Show Edit Page
  * - Process Edit Page
  * - Delete The Employee
  * - Show List of Employee
  */

class EmployeeController @Inject()(employee: EmployeeService) extends Controller {

  /**
    * This is Search Form
    */
  val searchEmployeeForm = Form {
    single(
      "name" -> nonEmptyText
    )
  }

  /**
    * This function validate the Date
    */
  def validateDate(date: String): Boolean = {
    if (!date.matches(".*[a-zA-Z]+.*")) {
      val df = new java.text.SimpleDateFormat("dd/MM/yyyy")
      date.equals(df.format(df.parse(date)))
    }
    else {
      false
    }
  }


  /**
    * This is Employee Form
    */
  val employeeForm = Form {
    mapping(
      "id" -> number(min=1),
      "name" -> nonEmptyText,
      "address" -> nonEmptyText,
      "dateOfBirth" -> text.verifying("Valid Date Required", data => validateDate(data)),
      "dateOfJoining" -> text.verifying("Valid Date Required", data => validateDate(data)),
      "designation" -> nonEmptyText
    )(Employee.apply)(Employee.unapply)
  }

  /**
    * This Action is to Show The Dashboard With Employee Table Updated
    */
  def showDashboard = Action.async { implicit request =>
    val empList = employee.getAllEmployee
    empList.map { employeList =>
      Ok(views.html.index(searchEmployeeForm, employeList))
    }
  }

  /**
    * This Action is to Process The Dashboard search
    * This action can send back bad request
    * This requect can response with aspected output
    */
  def processSearchForm = Action.async { implicit request =>
    val empList = employee.getAllEmployee

    searchEmployeeForm.bindFromRequest.fold(

      badForm =>
        empList.map { employeList =>
          BadRequest(views.html.index(badForm, employeList))
        },
      employeeData => {

        empList.map { emplist =>
          if (employeeData == "") {
            Ok(views.html.index(searchEmployeeForm, emplist))
          }
          else {
            val listOfName: List[Employee] = employee.getEmployeeByName(employeeData).toList
            Ok(views.html.index(searchEmployeeForm, listOfName))
          }
        }
      }
    )

  }

  /**
    * This Action is to Show The Add Form
    */
  def showAddForm = Action { implicit request =>
    Ok(views.html.add(employeeForm))
  }

  /**
    * This Action is to Process The Add Form
    * If error found then this action populate the error
    * otherwise it redirect to dashboard
    */
  def processAddForm = Action.async { implicit request =>

    employeeForm.bindFromRequest.fold(

      badForm => Future {
        BadRequest(views.html.add(badForm))
      },

      employeeData => Future {
        if(employee.addEmpployee(employeeData)) {
          Redirect(routes.EmployeeController.showDashboard()).flashing("success" -> "Employee Has been Added")
        }
        else{
          Redirect(routes.EmployeeController.showDashboard()).
            flashing("error" -> "Failed to add Employee. Duplicate ID")
        }
      }
    )

  }

  /**
    * This Action is to Show The Edit Employee Form
    */
  def showEditForm(emp: Int) = Action { implicit request =>
    Ok(views.html.edit(employeeForm, employee.getEmployee(emp).get))
  }

  /**
    * This Action is to Process the Edit Form
    * If error found then this action populate the error
    * otherwise it redirect to dashboard
    */
  def processEditForm = Action.async { implicit request =>

    employeeForm.bindFromRequest.fold(

      badForm => Future {
        BadRequest(views.html.edit(badForm, employee.getEmployee(1).get))
      },

      employeeData => Future {
        employee.updateEmployee(employeeData)
        Redirect(routes.EmployeeController.showDashboard())
      }
    )
  }

  /**
    * This Action is to Delet The Employee
    * and Redirect to Dashboard page
    */
  def delete(id: Int) = Action.async {
    val empList = employee.getAllEmployee
    val employeeToDelete: Employee = employee.getEmployee(id).get
    employee.deleteEmployee(employeeToDelete)
    empList.map { emplist =>
      Redirect(routes.EmployeeController.showDashboard())
    }
  }

}
