package com.github.verlattice.client.screens

import java.util.{TimeZone, GregorianCalendar, Date}

import com.github.verlattice.client.UIBuilder._
import com.github.verlattice.client._
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

class EditPlanScreen(div: HTMLDivElement, planName: String) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    val plan = MockServer.getPlan(planName)

    val titleParagraph: HTMLParagraphElement = paragraph("<h1>Edit Plan - " + planName + "</h1>")
    div.appendChild(titleParagraph)

    val actionNameInput: HTMLInputElement = textInputBox("actionNameInput", "")

    val dateInputBox = UIBuilder.dateInputBox()

    val addElementButton: HTMLButtonElement = UIBuilder.button("Add Action", () => {
      val date: js.Date = new js.Date(dateInputBox.value)
      val newPlan: Plan = plan.copy(scheduleElements = ScheduleElement(date.getTime().toLong, actionNameInput.value) :: plan.scheduleElements)
      MockServer.updatePlan(planName, newPlan)
      resetToScreen(new EditPlanScreen(div, planName), div, doneCallback)
    })
    div.appendChild(paragraph(label("Execute action "), actionNameInput, label(" on "), dateInputBox, addElementButton))

    if (plan.scheduleElements.isEmpty) {
      div.appendChild(paragraph("<em>This plan is currently empty.</em>"))
    } else {
      val listItems: List[HTMLDivElement] = plan.scheduleElements.sortWith((elem1, elem2) => elem1.time < elem2.time).map(scheduleElement =>
        UIBuilder.div(
          makeLabel(scheduleElement.actionToPerform + " @ " + new js.Date(scheduleElement.time).toDateString()),
          makeLabel("...resourceSet=" + MockServer.getState(planName, scheduleElement.time) + "..."),
          button("Remove", () => {
            MockServer.removeElementFromPlan(plan.name, scheduleElement.time)
            resetToScreen(new EditPlanScreen(div, planName), div, () => {
              this.visit(doneCallback)
            })
          }))
      )
      div.appendChild(list(listItems : _*)) // weird syntax
    }

    val backButton: HTMLButtonElement = UIBuilder.button("Back", doneCallback)
    div.appendChild(backButton)

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }

  def makeLabel(actionName: String): HTMLLabelElement = {
    val lbl: HTMLLabelElement = label(actionName)
    lbl.setAttribute("style", "margin-right: 50px")
    lbl
  }
}
