package com.github.verlattice.client.screens

import com.github.verlattice.client.UIBuilder
import com.github.verlattice.client.UIBuilder._
import org.scalajs.dom.raw.{HTMLButtonElement, HTMLDivElement}

import scala.scalajs.js.Dynamic.{global => g}


class HelpScreen(div: HTMLDivElement) extends Screen {

  def visit(doneCallback: () => Unit): Unit = {

    div.appendChild(paragraph("<h1>Help</h1>"))

    div.appendChild(paragraph("In order to use Verlattice, you'll need to know about its three core concepts:<p>" +
      "<li>Resource Types</li>" +
      "<li>Actions</li>" +
      "<li>Plans</li>" +
      "</p>"))

    div.appendChild(paragraph("<h2>Resource Types</h2>"))
    div.appendChild(paragraph("For each kind of thing you want to manage using Verlattice, you'll need to create a Resource Type."))
    div.appendChild(paragraph("A resource type can be whatever you want. For example: US dollars, tins of paint or wood panels."))

    div.appendChild(paragraph("<h2>Actions</h2>"))
    div.appendChild(paragraph("In Verlattice, an action is something you do outside of the application which has an " +
      "effect on the resources managed within the application."))
    div.appendChild(paragraph("For example, you might take an action to 'buy a cake'. " +
      "This would use up some of your dollars but would provide you with a cake."))

    div.appendChild(paragraph("<h2>Plans</h2>"))
    div.appendChild(paragraph("A plan defines the set of actions you intend to take. You can make as many plans as you like."))
    div.appendChild(paragraph("Based on the actions selected for the plan, Verlattice will automatically compute " +
      "the set of resources that will be available at every point in time in the plan."))
    div.appendChild(paragraph("It will also warn you when there aren't enough of a particular resource available, causing a plan to be unviable."))

    val homeButton: HTMLButtonElement = UIBuilder.button("Go Home", () => {
      doneCallback()
    })
    div.appendChild(homeButton)

  }
}
