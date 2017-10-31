package forkpin

trait Player {
  val toFEN: String
}
case object Black extends Player {
  val toFEN = "b"
}
case object White extends Player {
  val toFEN = "w"
}
