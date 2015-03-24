package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder._
import com.github.verlattice.client._
import org.scalajs.dom.raw._

import scala.scalajs.js.Dynamic.{global => g}

class EditPlanScreen(div: HTMLDivElement, planName: String) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    val titleParagraph: HTMLParagraphElement = paragraph("<h1>Edit Plan - " + planName + "</h1>")
    div.appendChild(titleParagraph)

    val backButton: HTMLButtonElement = UIBuilder.button("Back", doneCallback)
    div.appendChild(backButton)

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      changeToScreen(new WelcomeScreen(div), div)
    })
    div.appendChild(homeButton)

  }
}
