package com.github.verlattice.client

import scala.collection.mutable

class StateComputer(plan: Plan, actions: Set[Action]) {

  def computeStates(): Map[Long, Either[MissingResource, List[ActionOutput]]] = {

    val pairs = mutable.MutableList[(Long, Either[MissingResource, List[ActionOutput]])]()
    var lastPair: (Long, Either[MissingResource, List[ActionOutput]]) = (0, Right(List[ActionOutput]()))

    for (element <- plan.scheduleElements) {
      val action = actions.filter(a => a.name == element.actionToPerform).head
      lastPair._2 match {
        case Left(missingResource: MissingResource) => pairs += lastPair.copy(_1 = element.time) // We just keep repeating the error
        case Right(previousState: List[ActionOutput]) =>
          val subtracted: Either[String, List[ActionOutput]] = stateSubtract(previousState, action.inputs)
          subtracted match {
            case Left(missingResourceName: String) =>
              val tuple: (Long, Either[MissingResource, List[ActionOutput]]) = (element.time, Left(MissingResource(element.actionToPerform, missingResourceName, element.time)))
              pairs += tuple
              lastPair = tuple
            case Right(afterSubtraction: List[ActionOutput]) =>
              val resultantState = stateAdd(afterSubtraction, action.outputs)
              val tuple: (Long, Either[MissingResource, List[ActionOutput]]) = (element.time, Right(resultantState))
              pairs += tuple
              lastPair = tuple
          }
      }
    }
    pairs.toMap
  }

    private def stateAdd(baseState: List[ActionOutput], stateToAdd: List[ActionOutput]): List[ActionOutput] =
    {
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
      currentState.sortBy(actionOutput => actionOutput.resourceType) // sorted for determinism
    }

    private def stateSubtract(baseState: List[ActionOutput], stateToSubtract: List[ActionInput]): Either[String, List[ActionOutput]] = {

      var currentState: List[ActionOutput] = baseState
      for (input <- stateToSubtract) {
        val resourcesOfType = currentState.filter(actionOutput => actionOutput.resourceType == input.resourceType)
        if (resourcesOfType.isEmpty) {
          // Can't subtract this!
          return Left(input.resourceType)
        } else {
          // There is an applicable resource
          val existingResource: ActionOutput = resourcesOfType.head
          if (existingResource.quantity > input.quantity) {
            // We still have a resource at the end
            val newResource = ActionOutput(resourceType = input.resourceType, quantity = existingResource.quantity - input.quantity)
            currentState = newResource :: currentState.filter(actionOutput => actionOutput.resourceType != input.resourceType)
          } else if (existingResource.quantity == input.quantity) {
            // This exactly cancels out
            currentState = currentState.filter(actionOutput => actionOutput.resourceType != input.resourceType)
          } else {
            // Not enough
            return Left(input.resourceType)
          }
        }
      }
      Right(currentState.sortBy(actionOutput => actionOutput.resourceType))
    }
}

sealed case class MissingResource(requestedActionName: String, resourceName: String, time: Long)
