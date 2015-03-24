package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder._
import com.github.verlattice.client.{Action, MockServer, UIBuilder}
import org.scalajs.dom.raw.{HTMLParagraphElement, HTMLButtonElement, HTMLDivElement, HTMLInputElement}

class EditActionScreen(div: HTMLDivElement, actionName: String) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    val titleParagraph: HTMLParagraphElement = paragraph("<h1>Edit Action - " + actionName + "</h1>")
    div.appendChild(titleParagraph)

    div.appendChild(paragraph("Name:"))

    val action: Action = MockServer.getAction(actionName)

    val actionNameInput: HTMLInputElement = textInputBox("actionName", actionName)
    div.appendChild(paragraph(actionNameInput, button("RENAME", () => {
      MockServer.renameAction(actionName, actionNameInput.value)
      resetToScreen(new EditActionScreen(div, actionNameInput.value), div, doneCallback)
    })))

    div.appendChild(paragraph("<h2>Action Inputs and Outputs</h2>"))

    div.appendChild(paragraph("<h3>Input Resources</h3>"))

    div.appendChild(paragraph("<h3>Output Resources</h3>"))

    val backButton: HTMLButtonElement = UIBuilder.button("Back", doneCallback)
    div.appendChild(backButton)

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }
}
