package hourofcode2017.scala.gameoflife.v2


/*
 * Hour of Code 2017: Introduction to Functional Programming with Scala 
 * author:  Andrés López
 * version: 2
 *
 * Second Game of Life implementation. Let's do things better.
 *
 * This version covers:
 *   - execution context
 *   - implicit values
 *   - fluent pattern
 *   - pimp-my-library pattern
 *
 */
 object game {
    
    /*
     * Okay, okay, so let's do this:
     *   - instead of creating a bunch of functions and nest them, let's enrich our game table library (pimp-my-library)
     *   - make version 1 defined functions part of List[List[Int] library
     *   - this way we can chain our game table function calls
     */
     
     /*
      * Making a class implicit allows us to add that functionality to that data structure wherever we import it.
      * We will not need now to pass the game table as parameter as it is being passed in the class constructor.
      */
    implicit class GameOfInts(table: List[List[Int]]) {
        
        def expandNorth: List[List[Int]] = if (table.head.forall(_ == 0)) table else marginNorth
        def expandSouth: List[List[Int]] = if (table.last.forall(_ == 0)) table else marginSouth
        def expandWest: List[List[Int]] = if (table.forall(_.head == 0)) table else marginWest
        def expandEast: List[List[Int]] = if (table.forall(_.last == 0)) table else marginEast
        
        def marginNorth: List[List[Int]] = List.fill(table.head.size)(0) +: table
        def marginSouth: List[List[Int]] = table :+ List.fill(table.last.size)(0)
        def marginWest: List[List[Int]] = table.map(0 +: _)
        def marginEast: List[List[Int]] = table.map(_ :+ 0)
        
        def nextGeneration: List[List[Int]] = {
            
            @annotation.tailrec
            def iterateRows(rows: List[List[Int]])(result: List[List[Int]]): List[List[Int]] = rows match {
                
                case north :: center :: south :: Nil =>
                    result :+ iterateCols(north, center, south)(List[Int]())
                
                case north :: center :: south :: _ => 
                    iterateRows(rows.tail)(result :+ iterateCols(north, center, south)(List[Int]()))
                
                case _ => throw new Error("Less than three rows to iterate over.")
                
            }
    
            @annotation.tailrec
            def iterateCols(northRow: List[Int], centerRow: List[Int], southRow: List[Int])(result: List[Int]): List[Int] = (northRow, centerRow, southRow) match {
                
                case (nw :: n :: ne :: Nil, w :: cell :: e :: Nil, sw :: s :: se :: Nil) =>
                    result :+ nextState(nw, n, ne, w, e, sw, s, se)(cell)
                
                case (nw :: n :: ne :: _, w :: cell :: e :: _, sw :: s :: se :: _) =>
                    iterateCols(northRow.tail, centerRow.tail, southRow.tail)(result :+ nextState(nw, n, ne, w, e, sw, s, se)(cell))
                
                case _ => throw new Error("Less than three columns to iterate over.")
                
            }
        
            def nextState(nw: Int, n: Int, ne: Int, w: Int, e: Int, sw: Int, s: Int, se: Int)(cell: Int): Int = {
            
                val population: Int = nw + n + ne + w + e + sw + s + se
                if ((cell == 1 && (population == 2 || population == 3)) || (cell == 0 && population == 3)) 1 else 0
                
            }
            
            iterateRows(table.marginNorth.marginSouth.marginWest.marginEast)(List())
            
        }
    
        def run(generations: Int): List[List[Int]] = runGame(table)(generations)
    
        /*
         * Be careful, this is not stack-safe! 
         *
         * def run(generations: Int): List[List[Int]] =
         *    if (generations <= 0) table
         *    else table.expandNorth.expandSouth.expandWest.expandEast.nextGeneration.run(generations - 1)
         */
        
    }
    
    @annotation.tailrec
    def runGame(table: List[List[Int]])(generations: Int): List[List[Int]] =
        if (generations <= 0) table
        else runGame(table.expandNorth.expandSouth.expandWest.expandEast.nextGeneration)(generations - 1)
    
}


object GameOfLife extends App {
    
    import game.GameOfInts
    
    // Our game table...
    val table: List[List[Int]] = List(List(0, 1, 0), List(1, 1, 1), List(0, 1, 0))
    
    // ... and, again, running some magic.
    if (args.isEmpty) println("No generations specified. Aborting execution.")
    else table.run(args.head.toInt).foreach(println)
    
    // Still not that hard, right?
    
}
