package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder._
import com.github.verlattice.client._
import org.scalajs.dom.raw._

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

    val newInputBox: HTMLInputElement = textInputBox("newInputBox", "")
    val addInputButton: HTMLButtonElement = button("ADD", () => {
      val newInputName: String = newInputBox.value
      MockServer.addInputToAction(actionName, ActionInput(newInputName, 1))
      resetToScreen(new EditActionScreen(div, actionName), div, doneCallback)
      })
    div.appendChild(paragraph(newInputBox, addInputButton))

    if (action.inputs.isEmpty) {
      div.appendChild(paragraph("<em>This action doesn't currently take any inputs.</em>"))
    } else {
      div.appendChild(list(action.inputs.map(actionInput => UIBuilder.div(label(actionInput.render)))))
    }

    div.appendChild(paragraph("<h3>Output Resources</h3>"))

    val newOutputBox: HTMLInputElement = textInputBox("newInputBox", "")
    val addOutputButton: HTMLButtonElement = button("ADD", () => {
      val newOutputName: String = newOutputBox.value
      MockServer.addOutputToAction(actionName, ActionOutput(newOutputName, 1))
      resetToScreen(new EditActionScreen(div, actionName), div, doneCallback)
    })
    div.appendChild(paragraph(newOutputBox, addOutputButton))

    if (action.outputs.isEmpty) {
      div.appendChild(paragraph("<em>This action doesn't currently produce any outputs.</em>"))
    } else {
      div.appendChild(list(action.outputs.map(actionOutput => UIBuilder.div(label(actionOutput.render)))))
    }


    val backButton: HTMLButtonElement = UIBuilder.button("Back", doneCallback)
    div.appendChild(backButton)

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }
}
