package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder._
import com.github.verlattice.client._
import org.scalajs.dom.raw._

import scala.scalajs.js.Dynamic.{global => g}

class EditPlanScreen(div: HTMLDivElement, planName: String) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    val plan = MockServer.getPlan(planName)

    val titleParagraph: HTMLParagraphElement = paragraph("<h1>Edit Plan - " + planName + "</h1>")
    div.appendChild(titleParagraph)

    if (plan.scheduleElements.isEmpty) {
      div.appendChild(paragraph("<em>This plan is currently empty.</em>"))
    } else {
      val listItems: List[HTMLDivElement] = plan.scheduleElements.map(scheduleElement =>
        UIBuilder.div(
          makeLabel("At time " + scheduleElement.time + " do: " + scheduleElement.actionToPerform),
          button("Edit...", () => {
            resetToScreen(new EditActionScreen(div, scheduleElement.actionToPerform), div, () => {
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
