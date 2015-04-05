package com.github.verlattice.client

import org.scalatest.{FlatSpec, Matchers}

class StateComputerTest extends FlatSpec with Matchers {

  "The state computer" should "handle the empty case" in {
    val plan = Plan("Test Plan", List())
    val computer = new StateComputer(plan, Set())
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map())
  }

  it should "compute the resultant state after executing the sole action (which is a no-op)" in {
    val plan = Plan("Test Plan", List(ScheduleElement(1000L, "no-op")))
    val computer = new StateComputer(plan, Set(Action("no-op", List(), List())))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map((1000L, Right(List[ActionOutput]()))))
  }

  it should "compute the resultant state after executing the sole action (which produces one output)" in {
    val plan = Plan("Test Plan", List(ScheduleElement(1000L, "makeAlpha")))
    val computer = new StateComputer(plan, Set(Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10)))))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (1000L, Right(List(ActionOutput("Alpha", 10))))
    ))
  }

  it should "compute the resultant state after executing the same action twice" in {
    val plan = Plan("Test Plan", List(ScheduleElement(1000L, "makeAlpha"), ScheduleElement(2000L, "makeAlpha")))
    val computer = new StateComputer(plan, Set(Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10)))))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (1000L, Right(List(ActionOutput("Alpha", 10)))),
      (2000L, Right(List(ActionOutput("Alpha", 20)))
    )))
  }

  it should "compute the resultant state after executing the same action twice then executing an action to remove items" in {
    val plan = Plan("Test Plan", List(
      ScheduleElement(1000L, "makeAlpha"),
      ScheduleElement(2000L, "makeAlpha"),
      ScheduleElement(3000L, "destroyAlpha")
    ))
    val computer = new StateComputer(plan, Set(
      Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10))),
      Action("destroyAlpha", inputs = List(ActionInput("Alpha", 5)), outputs = List())
    ))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (2000L, Right(List(ActionOutput("Alpha", 20)))),
      (1000L, Right(List(ActionOutput("Alpha", 10)))),
      (3000L, Right(List(ActionOutput("Alpha", 15))))
    ))
  }

  it should "compute the resultant state correctly with multiple output types" in {
    val plan = Plan("Test Plan", List(
      ScheduleElement(1000L, "makeAlpha"),
      ScheduleElement(2000L, "makeBravo"),
      ScheduleElement(3000L, "destroyAlpha"),
      ScheduleElement(4000L, "destroyBravo")
    ))
    val computer = new StateComputer(plan, Set(
      Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10))),
      Action("makeBravo", inputs = List(), outputs = List(ActionOutput("Bravo", 10))),
      Action("destroyAlpha", inputs = List(ActionInput("Alpha", 5)), outputs = List()),
      Action("destroyBravo", inputs = List(ActionInput("Bravo", 5)), outputs = List())
    ))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (1000L, Right(List(ActionOutput("Alpha", 10)))),
      (2000L, Right(List(ActionOutput("Alpha", 10), ActionOutput("Bravo", 10)))),
      (3000L, Right(List(ActionOutput("Alpha", 5), ActionOutput("Bravo", 10)))),
      (4000L, Right(List(ActionOutput("Alpha", 5), ActionOutput("Bravo", 5))))
    ))
  }

  it should "indicate that a resource is missing" in {
    val plan = Plan("Test Plan", List(
      ScheduleElement(1000L, "makeAlpha"),
      ScheduleElement(2000L, "makeBravo"),
      ScheduleElement(3000L, "destroyAlpha"),
      ScheduleElement(4000L, "destroyBravo")
    ))
    val computer = new StateComputer(plan, Set(
      Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10))),
      Action("makeBravo", inputs = List(), outputs = List(ActionOutput("Bravo", 10))),
      Action("destroyAlpha", inputs = List(ActionInput("Alpha", 193284234)), outputs = List()), // Note increased amount
      Action("destroyBravo", inputs = List(ActionInput("Bravo", 5)), outputs = List())
    ))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (1000L, Right(List(ActionOutput("Alpha", 10)))),
      (2000L, Right(List(ActionOutput("Alpha", 10), ActionOutput("Bravo", 10)))),
      (3000L, Left(MissingResource("destroyAlpha", "Alpha", 3000L))),
      (4000L, Left(MissingResource("destroyAlpha", "Alpha", 3000L)))
    ))
  }

  it should "allow resources to be exactly canceled out" in {
    val plan = Plan("Test Plan", List(
      ScheduleElement(1000L, "makeAlpha"),
      ScheduleElement(2000L, "destroyAlpha")
    ))
    val computer = new StateComputer(plan, Set(
      Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10))),
      Action("destroyAlpha", inputs = List(ActionInput("Alpha", 10)), outputs = List())
    ))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (1000L, Right(List(ActionOutput("Alpha", 10)))),
      (2000L, Right(List()))
    ))
  }

  it should "enforce ordering of schedule elements" in {
    val plan = Plan("Test Plan", List(
      ScheduleElement(2000L, "makeAlpha"),
        ScheduleElement(1000L, "destroyAlpha")
    ))
    val computer = new StateComputer(plan, Set(
      Action("makeAlpha", inputs = List(), outputs = List(ActionOutput("Alpha", 10))),
      Action("destroyAlpha", inputs = List(ActionInput("Alpha", 10)), outputs = List())
    ))
    val states: Map[Long, Either[MissingResource, List[ActionOutput]]] = computer.computeStates()
    states should be (Map(
      (1000L, Left(MissingResource("destroyAlpha", "Alpha", 1000L))),
      (2000L, Left(MissingResource("destroyAlpha", "Alpha", 1000L)))
    ))
  }
}
