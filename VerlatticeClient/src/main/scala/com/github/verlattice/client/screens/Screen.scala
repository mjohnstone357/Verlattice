package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder
import org.scalajs.dom.raw.HTMLDivElement

trait Screen {

  def visit(doneCallback: () => Unit): Unit

  def changeToScreen(screen: Screen, div: HTMLDivElement): Unit = {

    val tempDiv = UIBuilder.div()

    while(div.firstChild != null) {
      tempDiv.appendChild(div.firstChild)
    }

    screen.visit(() => {
      UIBuilder.removeAllChildren(div)
      while (tempDiv.firstChild != null) {
        div.appendChild(tempDiv.firstChild)
      }
    })

  }

  // TODO Do I need this? Perhaps it's a premature optimisation
  def resetToScreen(screen: Screen, div: HTMLDivElement, doneCallback: () => Unit): Unit = {
    UIBuilder.removeAllChildren(div)
    screen.visit(() => {
      UIBuilder.removeAllChildren(div)
      doneCallback()
    })
  }
}
