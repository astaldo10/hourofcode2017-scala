package hourofcode2017.scala.gameoflife.v1


/*
 * Hour of Code 2017: Introduction to Functional Programming with Scala 
 * author:  Andrés López
 * version: 1
 *
 * First Game of Life implementation! Welcome to Functional Programming!
 *
 * This version covers:
 *   - values
 *   - functions flow
 *   - pattern matching
 *   - tail recursion
 *   - wildcards
 *
 */
object game {
    
    /*
     * Okay, so let's consider two things:
     *   - our game table will be represented as a list of lists of ints
     *   - our cells will take one of these values (representing their state):
     *     + 0 -> dead cell
     *     + 1 -> live cell
     */
    
    /*
     * These are expansion functions: given a list of lists of ints return another list of lists of ints with their corresponding
     * "coordenate" modified if a dead cell is found in the margin of that coordenate (adding a margin to that side).
     */
    def expandNorth(table: List[List[Int]]): List[List[Int]] = if (table.head.forall(_ == 0)) table else marginNorth(table)
    def expandSouth(table: List[List[Int]]): List[List[Int]] = if (table.last.forall(_ == 0)) table else marginSouth(table)
    def expandWest(table: List[List[Int]]): List[List[Int]] = if (table.forall(_.head == 0)) table else marginWest(table)
    def expandEast(table: List[List[Int]]): List[List[Int]] = if (table.forall(_.last == 0)) table else marginEast(table)
    
    /*
     * These are margin functions: add a margin to a list of lists of ints (represented by dead cells) to their corresponding
     * side.
     */
    def marginNorth(table: List[List[Int]]): List[List[Int]] = List.fill(table.head.size)(0) +: table
    def marginSouth(table: List[List[Int]]): List[List[Int]] = table :+ List.fill(table.last.size)(0)
    def marginWest(table: List[List[Int]]): List[List[Int]] = table.map(0 +: _)
    def marginEast(table: List[List[Int]]): List[List[Int]] = table.map(_ :+ 0)
    
    /*
     * Time to calculate next game table generation!
     */
    def nextGeneration(table: List[List[Int]]): List[List[Int]] = {
        
        /*
         * We need to iterate over the rows, taking three at the same time and stopping when an end of list (Nil) is detected after the
         * third row.
         */
        @annotation.tailrec
        def iterateRows(rows: List[List[Int]])(result: List[List[Int]]): List[List[Int]] = rows match {
            
            case north :: center :: south :: Nil =>
                result :+ iterateCols(north, center, south)(List[Int]())
            
            case north :: center :: south :: _ => 
                iterateRows(rows.tail)(result :+ iterateCols(north, center, south)(List[Int]()))
            
            case _ => throw new Error("Less than three rows to iterate over.")
            
        }

        /*
         * Now, We need to iterate over the columns, taking a cell and its environment (the surrounding 8 cells) at the same time and
         * stopping when an end of list (Nil) is detected after eastern cells.
         */
        @annotation.tailrec
        def iterateCols(northRow: List[Int], centerRow: List[Int], southRow: List[Int])(result: List[Int]): List[Int] = (northRow, centerRow, southRow) match {
            
            case (nw :: n :: ne :: Nil, w :: cell :: e :: Nil, sw :: s :: se :: Nil) =>
                result :+ nextState(nw, n, ne, w, e, sw, s, se)(cell)
            
            case (nw :: n :: ne :: _, w :: cell :: e :: _, sw :: s :: se :: _) =>
                iterateCols(northRow.tail, centerRow.tail, southRow.tail)(result :+ nextState(nw, n, ne, w, e, sw, s, se)(cell))
            
            case _ => throw new Error("Less than three columns to iterate over.")
            
        }
        
        // Do not forget to add margins to out table before its execution!
        iterateRows(marginNorth(marginSouth(marginWest(marginEast(table)))))(List())
        
    }
    
    /*
     * Given a cell environment composed by a cell and its surrounding cells we can calculate the next cell state!
     */
    def nextState(nw: Int, n: Int, ne: Int, w: Int, e: Int, sw: Int, s: Int, se: Int)(cell: Int): Int = {
    
        val population: Int = nw + n + ne + w + e + sw + s + se
        if ((cell == 1 && (population == 2 || population == 3)) || (cell == 0 && population == 3)) 1 else 0
        
    }
    
    /*
     * And now, let's run the game for some generations!
     */
    @annotation.tailrec
    def runGame(table: List[List[Int]])(generations: Int): List[List[Int]] =
        if (generations <= 0) table
        else runGame(nextGeneration(expandNorth(expandSouth(expandWest(expandEast(table))))))(generations - 1)
    
}


object GameOfLife extends App {
    
    import game._
    
    // This is our game table...
    val table: List[List[Int]] = List(List(0, 1, 0), List(1, 1, 1), List(0, 1, 0))
    
    // ... and this is where magic starts!
    if (args.isEmpty) println("No generations specified. Aborting execution.")
    else runGame(table)(args.head.toInt).foreach(println)
    
    // Not that hard, wasn' it?
    
}
