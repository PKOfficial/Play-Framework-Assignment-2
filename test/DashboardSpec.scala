import controllers.EmployeeController
import models.{Employee, EmployeeService}
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.concurrent.Execution.Implicits._
import play.api.test.Helpers._
import play.api.test._

import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class DashboardSpec extends Specification with Mockito {

  val services  = mock[EmployeeService]
  val controllerObj = new EmployeeController(services)

  "Employee Panel" should{
    "Show The DashBoard Page with Mockito" in new WithApplication() {
      when(services.getAllEmployee).thenReturn(Future(List(Employee(1,"Akash","Noida","24/03/1993","21/01/2016","Good"))))
      val result = call(controllerObj.showDashboard,FakeRequest(GET,"/"))
      contentType(result) must beSome.which(_ == "text/html")
    }
  }
  "Show the DashBoard Page without Mockito" in new WithApplication {
    val home = route(FakeRequest(GET, "/")).get
    status(home) must equalTo(OK)
    contentType(home) must beSome.which(_ == "text/html")
    contentAsString(home) must contain("Name")
  }

  "Search the Particular Employee" in new WithApplication {
    val search = route(FakeRequest(POST, "/search").withFormUrlEncodedBody("name" -> "Akash"))
    status(search.get) must equalTo(OK)
    contentType(search.get) must beSome.which(_ == "text/html")
    contentAsString(search.get) must contain("Name")
  }
  "Search the Empty " in new WithApplication {
    val search = route(FakeRequest(POST, "/search").withFormUrlEncodedBody("name" -> ""))
    status(search.get) must equalTo(OK)
    contentType(search.get) must beSome.which(_ == "text/html")
    contentAsString(search.get) must contain("Name")
  }
  "Search the Empty " in new WithApplication {
    val search = route(FakeRequest(POST, "/search"))
    status(search.get) must equalTo(BAD_REQUEST)
  }

  "Show the Add Page " in new WithApplication {
    val add = route(FakeRequest(GET, "/add")).get
    status(add) must equalTo(OK)
    contentType(add) must beSome.which(_ == "text/html")
    contentAsString(add) must contain("Name")
  }
  "Add the Particular Employee" in new WithApplication {
    val search = route(FakeRequest(POST, "/addemployee").withFormUrlEncodedBody("id" -> "3","name" -> "Someone","address" -> "Noidas","dateOfBirth" ->"12/12/1990","dateOfJoining" -> "12/12/2015","designation" -> "UP"))
    status(search.get) must equalTo(SEE_OTHER)
  }
  "Add the Wrong Employee" in new WithApplication {
    val search = route(FakeRequest(POST, "/addemployee").withFormUrlEncodedBody("id" -> "3","name" -> "","address" -> "Noidas","dateOfBirth" ->"12/12/1990","dateOfJoining" -> "12/12/2015","designation" -> "UP"))
    status(search.get) must equalTo(BAD_REQUEST)
  }
  "Add the Employee with  Wrong Date" in new WithApplication {
    val search = route(FakeRequest(POST, "/addemployee").withFormUrlEncodedBody("id" -> "3","name" -> "akash","address" -> "Noidas","dateOfBirth" ->"12/123/1990","dateOfJoining" -> "12/123/2015","designation" -> "UP"))
    status(search.get) must equalTo(BAD_REQUEST)
  }
  "Show the Edit Page " in new WithApplication {
    val add = route(FakeRequest(GET, "/edit/1")).get
    status(add) must equalTo(OK)
    contentType(add) must beSome.which(_ == "text/html")
    contentAsString(add) must contain("Edit")
  }
  "Edit the  Employee" in new WithApplication {
    val edit = route(FakeRequest(POST, "/editemployee").withFormUrlEncodedBody("id" -> "3","name" -> "Akash","address" -> "Noidas","dateOfBirth" ->"12/12/1990","dateOfJoining" -> "12/12/2015","designation" -> "UP"))
    status(edit.get) must equalTo(SEE_OTHER)
  }
  "Edit the Wrong Date with  Employee" in new WithApplication {
    val search = route(FakeRequest(POST, "/editemployee").withFormUrlEncodedBody("id" -> "3","name" -> "","address" -> "Noidas","dateOfBirth" ->"12/124/1990","dateOfJoining" -> "12/12/2015","designation" -> "UP"))
    status(search.get) must equalTo(BAD_REQUEST)
  }
  "Edit the  Employee with Wrong Value" in new WithApplication {
    val edit = route(FakeRequest(POST, "/editemployee").withFormUrlEncodedBody("id" -> "3","name" -> "","address" -> "Noidas","dateOfBirth" ->"12/12/1990","dateOfJoining" -> "12/12/2015","designation" -> "UP"))
    status(edit.get) must equalTo(BAD_REQUEST)
  }
  "Delete the  Employee" in new WithApplication {
    val delete = route(FakeRequest(GET, "/delete/1"))
    status(delete.get) must equalTo(SEE_OTHER)
  }

}
