
object sudoku {

  // 数独のマスの座標
  type Cell = (Int, Int)
  // 数独の盤面
  type Board = List[(Cell, Int)]

  def main(args: Array[String]): Unit = {
    val example:Board = List(((3,0), 8)
      ,((5,0), 1)
      ,((6,1), 4)
      ,((7,1), 3)
      ,((0,2), 5)
      ,((4,3), 7)
      ,((6,3), 8)
      ,((6,4), 1)
      ,((1,5), 2)
      ,((4,5), 3)
      ,((0,6), 6)
      ,((7,6), 7)
      ,((8,6), 5)
      ,((2,7), 3)
      ,((3,7), 4)
      ,((3,8), 2)
      ,((6,8), 6)
    )

    val ret = sdk(example:Board)
    ret.foreach(format(_))
  }

  // 81マス全体
  val cells:List[Cell] = (for (x <- (0 to 8); y <- (0 to 8)) yield (x, y)).toList

  def sdk(board:Board):List[Board] = {
    board match {
      // 盤面が81の場合は終了
      case m if m.length == 81 => List(m)

      case _ => {
        // まだ数値が決まっていないマス
        val remains = cells.filter(x => (board.forall(x != _._1)))
        // 最も周りのマスが埋まっているマス->手の可能性が少ないマス
        val cell: Cell = remains.maxBy(used(board, _).length)
        // マスに入れることのできる数値の候補
        val ns: List[Int] = (1 to 9).filter(x => used(board, cell).forall(x != _)).toList
        ns.flatMap(x => sdk((cell, x) :: board)).distinct
      }
    }
  }

  // ある盤面状況で、あるマスの周囲に使われている数値を列挙する
  def used(board:Board, cell:Cell):List[Int] = {
    // x の縦、横、エリア内をチェック
    val usedcells = board.filter(x => ((area(x._1) == area(cell) || x._1._1 == cell._1 || x._1._2 == cell._2)))
    usedcells.map(_._2).distinct
  }

  // マスの所属する区間
  def area(cell:Cell):Int = {
    cell._2 / 3 * 3 + cell._1 / 3
  }

  // 出力を整形
  def format(board:Board) = {
    val sorted = board.sortBy(_._1._2).sortBy(_._1._1)
    sorted.foreach( x => if (x._1._2 / 8 == 1) println(x._2) else print(x._2))
    println()
  }
}