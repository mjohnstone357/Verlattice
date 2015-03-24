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
    val newInputQuantityBox: HTMLInputElement = textInputBox("newInputQuantity", "")
    val addInputButton: HTMLButtonElement = button("ADD", () => {
      val newInputName: String = newInputBox.value
      val newInputQuantity: Int = newInputQuantityBox.value.toInt
      MockServer.addInputToAction(actionName, ActionInput(newInputName, newInputQuantity))
      resetToScreen(new EditActionScreen(div, actionName), div, doneCallback)
      })
    div.appendChild(paragraph(newInputBox, label(" Quantity: "), newInputQuantityBox, addInputButton))

    if (action.inputs.isEmpty) {
      div.appendChild(paragraph("<em>This action doesn't currently take any inputs.</em>"))
    } else {
      div.appendChild(list(action.inputs.map(actionInput => UIBuilder.div(paragraph(actionInput.render)))))
    }

    div.appendChild(paragraph("<h3>Output Resources</h3>"))

    val newOutputBox: HTMLInputElement = textInputBox("newOutputBox", "")
    val newOutputQuantityBox: HTMLInputElement = textInputBox("newOutputQuantity", "")
    val addOutputButton: HTMLButtonElement = button("ADD", () => {
      val newOutputName: String = newOutputBox.value
      val newOutputQuantity: Int = newOutputQuantityBox.value.toInt
      MockServer.addOutputToAction(actionName, ActionOutput(newOutputName, newOutputQuantity))
      resetToScreen(new EditActionScreen(div, actionName), div, doneCallback)
    })
    div.appendChild(paragraph(newOutputBox, label(" Quantity: "), newOutputQuantityBox, addOutputButton))

    if (action.outputs.isEmpty) {
      div.appendChild(paragraph("<em>This action doesn't currently produce any outputs.</em>"))
    } else {
      div.appendChild(list(action.outputs.map(actionOutput => UIBuilder.div(paragraph(actionOutput.render)))))
    }


    val backButton: HTMLButtonElement = UIBuilder.button("Back", doneCallback)
    div.appendChild(backButton)

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }
}
