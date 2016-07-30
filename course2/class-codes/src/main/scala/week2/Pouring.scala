package week2

/**
  * Created by naheon on 2016. 7. 30..
  */
class Pouring(capacity: Vector[Int]) {
  /** States */
  type State = Vector[Int]

  val initialState = capacity map (x => 0)

  val glasses = 0 until capacity.length

  /** Moves */
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      // Can pour as much as amount of water
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  // Possible moves within capacity
  val moves =
      (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  /** Paths */
  class Path(history: List[Move], val endState: State) {
    // def endState: State = (moves foldRight initialState)(_ change _) // trackState(history)

    private def trackState(moves: List[Move]): State = moves match {
      case Nil => initialState
      case last::rest => last change trackState(rest)
    }

    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString = (history.reverse mkString " ") + ("-->" + endState)
  }

  /**
    * From initial paths, create streams of paths.
    * @param paths
    * @return
    */
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      // Generate all successive paths.
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next

      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val initialPath = new Path(Nil, initialState)
  val pathSets: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] = for {
    pathSet <- pathSets
    path <- pathSet
    if path.endState contains target
  } yield path


}
