package graphicgame

object ShortestPath extends App {
  val maze = Array(
    Array(0,-1, 0, 0, 0, 0, 0, 0, 0,-1),
    Array(0,-1, 0,-1, 0,-1,-1,-1, 0, 0),
    Array(0,-1, 0,-1, 0, 0, 0,-1, 0,-1),
    Array(0,-1, 0,-1,-1,-1, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0,-1, 0,-1, 0,-1),
    Array(0,-1,-1,-1, 0,-1, 0,-1, 0, 0),
    Array(0,-1, 0,-1, 0,-1, 0,-1,-1, 0),
    Array(0,-1, 0, 0, 0, 0,-1, 0, 0, 0),
    Array(0,-1, 0,-1, 0,-1,-1, 0,-1,-1),
    Array(0, 0, 0,-1, 0,-1, 0, 0, 0, 0))
    
  val offsets = Array((-1, 0), (1, 0), (0, 1), (0, -1))
  def shortestPath(sx: Int, sy: Int, ex: Int, ey: Int): Int = {
    val queue = new ArrayQueue[(Int, Int, Int)]()
    var visited = Set(sx -> sy)
    queue.enqueue((sx, sy, 0))
    while(!queue.isEmpty) {
      val (x, y, steps) = queue.dequeue()
      if(x == ex && y == ey) return steps
      for((dx, dy) <- offsets) {
        val nx = x+dx
        val ny = y+dy
        if(ny >= 0 && ny < maze.length && nx >= 0 && nx < maze(ny).length && 
            maze(ny)(nx) != -1 && !visited(nx -> ny)) {
          queue.enqueue((nx, ny, steps+1))
          visited += (nx -> ny)
        }
      }
    }
    1000000000
  }
  
  println(shortestPath(0, 0, 9, 9))
}
