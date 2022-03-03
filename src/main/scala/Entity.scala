package com.ing.loca

trait OperationType

trait Entity {
  trait OperationType {
    type ReturnValue

    // defines which return value occurs when operations is executed in state
    def returnValue(state: State): ReturnValue

    // next internal state for items
    //  If operation's precondition fails, this returns the same state. return value may signal failure
    def effect(state: State): State
  }
  type State

  type Operation <: OperationType
}
