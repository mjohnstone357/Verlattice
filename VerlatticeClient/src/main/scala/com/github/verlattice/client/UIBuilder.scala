package com.github.verlattice.client

import org.scalajs.dom.document
import org.scalajs.dom.raw._
import org.scalajs.jquery._

object UIBuilder {

  var idCounter = 0

  private def nextID(): String = {
    val id = "elem" + idCounter
    idCounter += 1
    id
  }

  def div(): HTMLDivElement = {
    document.createElement("div").asInstanceOf[HTMLDivElement]
  }

  def div(childElements: HTMLElement*): HTMLDivElement = {
    val newDiv: HTMLDivElement = div()
    for (childElement <- childElements) {
      newDiv.appendChild(childElement)
    }
    newDiv
  }

  def label(text: String): HTMLLabelElement = {
    val lbl: HTMLLabelElement = document.createElement("label").asInstanceOf[HTMLLabelElement]
    lbl.textContent = text
    lbl
  }

  def paragraph(innerHTML: String): HTMLParagraphElement = {
    val parNode = document.createElement("p").asInstanceOf[HTMLParagraphElement]
    parNode.innerHTML = innerHTML
    parNode
  }

  def list(listElements: HTMLDivElement*): HTMLUListElement = {
    val list: HTMLUListElement = document.createElement("ul").asInstanceOf[HTMLUListElement]
    for (listElement <- listElements) {
      list.appendChild(listElement)
    }
    list
  }

  def list(listElements: List[HTMLDivElement]): HTMLUListElement = {
    val list: HTMLUListElement = document.createElement("ul").asInstanceOf[HTMLUListElement]
    for (listElement <- listElements) {
      list.appendChild(listElement)
    }
    list
  }

  def paragraph(childElements: HTMLElement*): HTMLParagraphElement = {
    val parNode = document.createElement("p").asInstanceOf[HTMLParagraphElement]
    for (childElement <- childElements) {
      parNode.appendChild(childElement)
    }
    parNode
  }

  def button(label: String, onClick: () => Unit): HTMLButtonElement = {
    val btn: HTMLButtonElement = document.createElement("button").asInstanceOf[HTMLButtonElement]
    btn.setAttribute("type", "button")
    val buttonID: String = nextID()
    btn.id = buttonID
    btn.innerHTML = label
    jQuery(document).on("click", "#" + buttonID, onClick)

    btn
  }

  def textInputBox(name: String, label: String): HTMLInputElement = {
    val input: HTMLInputElement = document.createElement("input").asInstanceOf[HTMLInputElement]
    input.setAttribute("type", "text")
    input.setAttribute("name", name)
    input.value = label
    input
  }

  def removeAllChildren(element: HTMLElement) = {
    while (element.firstChild != null) {
      element.removeChild(element.firstChild)
    }
  }
}
