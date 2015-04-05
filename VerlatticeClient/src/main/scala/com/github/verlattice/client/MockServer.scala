package com.github.verlattice.client

import scala.collection.mutable

object MockServer {

  def getVersion: String = "0.0.3"

  private val resourceTypeNames = mutable.HashSet[String]()
  private val actions = mutable.HashSet[Action]()
  private val plans = mutable.HashSet[Plan]()

  def getResourceTypeNames: List[String] = resourceTypeNames.toList.sorted

  def addResourceTypeName(resourceTypeName: String): Unit = {
    resourceTypeNames += resourceTypeName
  }

  def getActionNames: List[String] = actions.map(action => action.name).toList.sorted

  def addAction(action: Action): Unit = {
    actions += action
  }

  def getAction(actionName: String): Action = {
    actions.filter(existingAction => existingAction.name == actionName).head
  }

  def actionExists(actionName: String): Boolean = {
    resourceTypeNames.contains(actionName)
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

  def addInputToAction(actionName: String, input: ActionInput): Unit = {
    val oldAction = actions.filter(existingAction => existingAction.name == actionName).head
    actions.remove(oldAction)
    actions += oldAction.copy(inputs = input :: oldAction.inputs)
  }

  def addOutputToAction(actionName: String, output: ActionOutput): Unit = {
    val oldAction = actions.filter(existingAction => existingAction.name == actionName).head
    actions.remove(oldAction)
    actions += oldAction.copy(outputs = output :: oldAction.outputs)
  }

  def getPlanNames: List[String] = plans.toList.map(plan => plan.name)

  def getPlan(planName: String): Plan = {
    plans.filter(_.name == planName).head
  }

  def addPlan(plan: Plan): Unit = {
    plans += plan
  }

  def updatePlan(oldPlanName: String, plan: Plan): Unit = {
    val oldPlan = plans.filter(p => p.name == oldPlanName).head
    plans -= oldPlan
    plans += plan
  }

  def removeElementFromPlan(planName: String, elementTime: Long): Unit = {
    val plan: Plan = getPlan(planName)
    val remainingElements: List[ScheduleElement] = plan.scheduleElements.filter(element => element.time != elementTime)
    val newPlan = plan.copy(scheduleElements = remainingElements)
    updatePlan(planName, newPlan)
  }

  def computeStates(plan: Plan): Map[Long, Either[MissingResource, List[ActionOutput]]] = {
    new StateComputer(plan, actions.toSet).computeStates()
  }

  def planHasIssues(planName: String): Boolean = {
    computeStates(getPlan(planName)).map(_._2).filter(_.isLeft).nonEmpty // At least one error
  }
}

sealed case class Plan(name: String, scheduleElements: List[ScheduleElement])

sealed case class ScheduleElement(time: Long, actionToPerform: String)

sealed case class Action(name: String, inputs: List[ActionInput], outputs: List[ActionOutput])

sealed case class ActionInput(resourceType: String, quantity: Int) {
  def render: String = resourceType + " <em>x" + quantity + "</em>"
}

sealed case class ActionOutput(resourceType: String, quantity: Int) {
  def render: String = resourceType + " <em>x" + quantity + "</em>"
}

sealed case class PlanState(time: Long, outputs: List[ActionOutput])
