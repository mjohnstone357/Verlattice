package com.github.verlattice.client

import scala.collection.mutable

object MockServer {

  def getVersion: String = "0.0.1"

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

  def getState(planName: String, time: Long): Option[PlanState] = {
    val plan: Plan = getPlan(planName)
    val precedingElements: List[ScheduleElement] = plan.scheduleElements.filter(element => element.time <= time)
    var currentState = List[ActionOutput]()
    for (element <- precedingElements) {
      val maybeState: Option[List[ActionOutput]] = applyStuff(currentState, element)
      maybeState match {
        case Some(state) => currentState = state
        case None => return None
      }
    }
    Some(PlanState(time, currentState))
  }

  private def applyStuff(state: List[ActionOutput], scheduleElement: ScheduleElement): Option[List[ActionOutput]] = {
    val action: Action = getAction(scheduleElement.actionToPerform)
    val afterInputsSubtracted: Option[List[ActionOutput]] = stateSubtract(state, action.inputs)
    afterInputsSubtracted match {
      case None => None
      case Some(resultantState) =>
        Some(stateAdd(resultantState, action.outputs))
    }
  }

  private def stateAdd(baseState: List[ActionOutput], stateToAdd: List[ActionOutput]): List[ActionOutput] = {
    var currentState: List[ActionOutput] = baseState
    for (output <- stateToAdd) {
      val resourcesOfType = currentState.filter(actionOutput => actionOutput.resourceType == output.resourceType)
      if (resourcesOfType.isEmpty) {
        // No need to remove/coalesce anything
        currentState = output :: currentState
      } else {
        val existingResource: ActionOutput = resourcesOfType.head
        val newResource = ActionOutput(resourceType = output.resourceType, quantity = output.quantity + existingResource.quantity)
        currentState = newResource :: currentState.filter(actionOutput => actionOutput.resourceType != output.resourceType)
      }
    }
    currentState
  }

  private def stateSubtract(baseState: List[ActionOutput], stateToSubtract: List[ActionInput]): Option[List[ActionOutput]] = {
    var currentState: List[ActionOutput] = baseState
    for (input <- stateToSubtract) {
      val resourcesOfType = currentState.filter(actionOutput => actionOutput.resourceType == input.resourceType)
      if (resourcesOfType.isEmpty) {
        // Can't subtract this!
        return None
      } else {
        // There is an applicable resource
        val existingResource: ActionOutput = resourcesOfType.head
        if (existingResource.quantity > input.quantity) {
          // We still have a resource at the end
          val newResource = ActionOutput(resourceType = input.resourceType, quantity = existingResource.quantity - input.quantity)
          currentState = newResource :: currentState.filter(actionOutput => actionOutput.resourceType != input.resourceType)
        }
      }
    }
    Some(currentState)
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
