package com.github.verlattice.client.screens

import com.github.verlattice.client.{MockServer, UIBuilder}
import com.github.verlattice.client.UIBuilder._
import org.scalajs.dom.raw.{HTMLInputElement, HTMLButtonElement, HTMLDivElement}

class ManageResourcesScreen(div: HTMLDivElement) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    div.appendChild(paragraph("<h1>Manage Resource Types</h1>"))
    div.appendChild(paragraph("From here you can manage your resource types."))

    div.appendChild(paragraph("<h2>Available Resource Types</h2>"))

    val newResourceInput: HTMLInputElement = textInputBox("newRTName", "")
    val createButton: HTMLButtonElement = button("CREATE", () => {
      MockServer.addResourceTypeName(newResourceInput.value)
      changeToScreen(new ManageResourcesScreen(div), div)
    })
    div.appendChild(
      paragraph(
        newResourceInput,
        createButton))

    newResourceInput.focus()

    val resourceNames: List[String] = MockServer.getResourceTypeNames

    if (resourceNames.isEmpty) {
      div.appendChild(paragraph("<em>You haven't added any resource types yet.</em>"))
    } else {
      div.appendChild(paragraph("The following resource types are available:"))
      for(resourceName <- resourceNames) {
        div.appendChild(paragraph("<li>" + resourceName + "</li>"))
      }
    }

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }
}
