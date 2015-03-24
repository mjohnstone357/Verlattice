package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder._
import com.github.verlattice.client.{Action, MockServer, UIBuilder}
import org.scalajs.dom.raw.{HTMLParagraphElement, HTMLButtonElement, HTMLDivElement, HTMLInputElement}

class EditActionScreen(div: HTMLDivElement, initialActionName: String) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    val titleParagraph: HTMLParagraphElement = paragraph("<h1>Edit Action - " + initialActionName + "</h1>")
    div.appendChild(titleParagraph)

    div.appendChild(paragraph("Name:"))

    var currentActionName = initialActionName

    val actionNameInput: HTMLInputElement = textInputBox("actionName", initialActionName)
    div.appendChild(paragraph(actionNameInput, button("RENAME", () => {
      MockServer.renameAction(currentActionName, actionNameInput.value)
      currentActionName = actionNameInput.value
      titleParagraph.innerHTML = "<h1>Edit Action - " + currentActionName + "</h1>"
    })))

    div.appendChild(paragraph(button("UPDATE", () => {
      MockServer.addAction(Action(actionNameInput.value, List.empty, List.empty))
      changeToScreen(new ManageActionsScreen(div), div)
    })))

    val backButton: HTMLButtonElement = UIBuilder.button("Back", doneCallback)
    div.appendChild(backButton)

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }
}
