package com.ing.loca

trait Entity {
  type Operation
  type State

  // defines which return value occurs when operations is executed in state
  def returnValue(state: State, operation: Operation): Any

  // next internal state for items
  //  If operation's precondition fails, this returns the same state. return value may signal failure
  def effect(state: State, operation: Operation): State

}
