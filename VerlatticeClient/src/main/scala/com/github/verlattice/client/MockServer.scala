package com.github.verlattice.client

import scala.collection.mutable

object MockServer {

  def getVersion: String = "0.0.1"

  private val resourceTypeNames = mutable.HashSet[String]()

  def getResourceTypeNames: List[String] = resourceTypeNames.toList.sorted

  def addResourceTypeName(resourceTypeName: String): Unit = {
    resourceTypeNames += resourceTypeName
  }

  private val actions = mutable.HashSet[Action]()

  def getActionNames: List[String] = actions.map(action => action.name).toList.sorted

  def addAction(action: Action): Unit = {
    actions += action
  }

  def getAction(actionName: String): Action = {
    actions.filter(existingAction => existingAction.name == actionName).head
  }

  def updateAction(action: Action): Unit = {
    val oldAction = actions.filter(existingAction => existingAction.name == action.name).head
    actions.remove(oldAction)
    actions += action
  }

  def renameAction(oldName: String, newName: String): Unit = {
    val oldAction = actions.filter(existingAction => existingAction.name == oldName).head
    actions.remove(oldAction)
    actions += oldAction.copy(name = newName)
  }

}

sealed case class Action(name: String, inputs: List[ActionInput], outputs: List[ActionOutput])

sealed case class ActionInput(resourceType: String, quantity: Int) {
  def render: String = resourceType + " x " + quantity
}


sealed case class ActionOutput(resourceType: String, quantity: Int) {
  def render: String = resourceType + " x " + quantity
}
