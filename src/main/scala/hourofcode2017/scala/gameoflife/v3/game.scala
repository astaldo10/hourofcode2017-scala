package hourofcode2017.scala.gameoflife.v3


/*
 * Hour of Code 2017: Introduction to Functional Programming with Scala 
 * author:  Andrés López
 * version: 3
 *
 * Third Game of Life implementation. This looks good!
 *
 * This version covers:
 *   - high order functions
 *   - type alias
 *   - type functions
 *
 */
object game {
    
    /*
     * Keep improving! Now:
     *   - create an alias for our game table type
     *   - allow type parametrization on functions
     *   - create a Int game implementation
     */
    
    // Isn't this type easier to read and understand?
    type GameTable[A] = List[List[A]]
    
    implicit class GameOfInts(table: GameTable[Int]) {
        
        /*
         * This implementation looks cleaner, don't you think?
         */
         
        def expandNorth: GameTable[Int] = expNorth(0)(table)
        def expandSouth: GameTable[Int] = expSouth(0)(table)
        def expandWest: GameTable[Int] = expWest(0)(table)
        def expandEast: GameTable[Int] = expEast(0)(table)
        
        def marginNorth: GameTable[Int] = margNorth(0)(table)
        def marginSouth: GameTable[Int] = margSouth(0)(table)
        def marginWest: GameTable[Int] = margWest(0)(table)
        def marginEast: GameTable[Int] = margEast(0)(table)
        
        def nextGeneration: GameTable[Int] = nGeneration(table.marginNorth.marginSouth.marginWest.marginEast)(nextState)
        
        def nextState(nw: Int, n: Int, ne: Int, w: Int, e: Int, sw: Int, s: Int, se: Int)(cell: Int): Int = {
        
            val population: Int = nw + n + ne + w + e + sw + s + se
            if ((cell == 1 && (population == 2 || population == 3)) || (cell == 0 && population == 3)) 1 else 0
            
        }
            
        def run(generations: Int): GameTable[Int] = runGame(table)(generations)
        
    }
    
    /*
     * Our expansion functions are type parametrized: we can use them for more types than just Int and they
     * will work the same way for all of them.
     */
    def expNorth[A](deadState: A)(table: GameTable[A]): GameTable[A] = if (table.head.forall(_ == deadState)) table else margNorth(deadState)(table)
    def expSouth[A](deadState: A)(table: GameTable[A]): GameTable[A] = if (table.last.forall(_ == deadState)) table else margSouth(deadState)(table)
    def expWest[A](deadState: A)(table: GameTable[A]): GameTable[A] = if (table.forall(_.head == deadState)) table else margWest(deadState)(table)
    def expEast[A](deadState: A)(table: GameTable[A]): GameTable[A] = if (table.forall(_.last == deadState)) table else margEast(deadState)(table)

    def margNorth[A](deadState: A)(table: GameTable[A]): GameTable[A] = List.fill(table.head.size)(deadState) +: table
    def margSouth[A](deadState: A)(table: GameTable[A]): GameTable[A] = table :+ List.fill(table.last.size)(deadState)
    def margWest[A](deadState: A)(table: GameTable[A]): GameTable[A] = table.map(deadState +: _)
    def margEast[A](deadState: A)(table: GameTable[A]): GameTable[A] = table.map(_ :+ deadState)
    
    /*
     * We have defined our state calculator inside our implementation, so now we will need to pass that function
     * as a parameter to the next generation type parametrized function!
     */
    def nGeneration[A](table: GameTable[A])(nextState: (A, A, A, A, A, A, A, A) => A => A): GameTable[A] = {
        
        @annotation.tailrec
        def iterateRows(rows: GameTable[A])(result: GameTable[A]): GameTable[A] = rows match {
            
            case north :: center :: south :: Nil =>
                result :+ iterateCols(north, center, south)(List[A]())
            
            case north :: center :: south :: _ => 
                iterateRows(rows.tail)(result :+ iterateCols(north, center, south)(List[A]()))
            
            case _ => throw new Error("Less than three rows to iterate over.")
            
        }

        @annotation.tailrec
        def iterateCols(northRow: List[A], centerRow: List[A], southRow: List[A])(result: List[A]): List[A] = (northRow, centerRow, southRow) match {
            
            case (nw :: n :: ne :: Nil, w :: cell :: e :: Nil, sw :: s :: se :: Nil) =>
                result :+ nextState(nw, n, ne, w, e, sw, s, se)(cell)
            
            case (nw :: n :: ne :: _, w :: cell :: e :: _, sw :: s :: se :: _) =>
                iterateCols(northRow.tail, centerRow.tail, southRow.tail)(result :+ nextState(nw, n, ne, w, e, sw, s, se)(cell))
            
            case _ => throw new Error("Less than three columns to iterate over.")
            
        }
        
        iterateRows(table)(List())
        
    }
    
    @annotation.tailrec
    def runGame(table: GameTable[Int])(generations: Int): GameTable[Int] =
        if (generations <= 0) table
        else runGame(table.expandNorth.expandSouth.expandWest.expandEast.nextGeneration)(generations - 1)
    
}


object GameOfLife extends App {
    
    import game.{GameTable, GameOfInts}
    
    // Our game table...
    val table: GameTable[Int] = List(List(0, 1, 0), List(1, 1, 1), List(0, 1, 0))
    
    // ... and, again, running some magic.
    if (args.isEmpty) println("No generations specified. Aborting execution.")
    else table.run(args.head.toInt).foreach(println)
    
    // Types weren't that scary... or were they?
    
}
