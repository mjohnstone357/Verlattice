package com.github.verlattice.client

import com.github.verlattice.client.screens.WelcomeScreen
import org.scalajs.dom.raw.HTMLDivElement
import scala.scalajs.js.Dynamic.{global => g}


import org.scalajs.dom
import dom.document

import scala.scalajs.js.JSApp

object VerlatticeClient extends JSApp {

  override def main(): Unit = {

    UIBuilder.removeAllChildren(document.body)

    new WelcomeScreen(document.body.asInstanceOf[HTMLDivElement]).visit(() => {
      // This shouldn't really happen, as the welcome screen is the top-level screen (like the main method)
      g.alert("It is now safe to turn off your computer")
    })

  }

}
