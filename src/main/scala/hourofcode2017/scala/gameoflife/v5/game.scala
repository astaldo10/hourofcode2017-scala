package hourofcode2017.scala.gameoflife.v5


/*
 * Hour of Code 2017: Introduction to Functional Programming with Scala 
 * author:  Andrés López
 * version: 5
 *
 * Fifth Game of Life implementation. Why can't I hold all this awesomeness?
 *
 * This version covers:
 *   - error handling
 *     + Option
 *     + Try
 *     + Either
 */

/*
 * This was a long trip, we are reaching our last stop:
 *   - remove throws: they are ugly!
 *   - add some functional error handling to our program
 */
sealed trait Game[A] {
    
    import Game.{GameTable, CellEnvironment}
    
    val deadState: A
    val liveState: A
    
    def population(env: CellEnvironment[A]): Int

    def expandNorth(table: GameTable[A]): GameTable[A] = if (table.head.forall(_ == deadState)) table else marginNorth(table)
    def expandSouth(table: GameTable[A]): GameTable[A] = if (table.last.forall(_ == deadState)) table else marginSouth(table)
    def expandWest(table: GameTable[A]): GameTable[A] = if (table.forall(_.head == deadState)) table else marginWest(table)
    def expandEast(table: GameTable[A]): GameTable[A] = if (table.forall(_.last == deadState)) table else marginEast(table)

    def marginNorth(table: GameTable[A]): GameTable[A] = List.fill(table.head.size)(deadState) +: table
    def marginSouth(table: GameTable[A]): GameTable[A] = table :+ List.fill(table.last.size)(deadState)
    def marginWest(table: GameTable[A]): GameTable[A] = table.map(deadState +: _)
    def marginEast(table: GameTable[A]): GameTable[A] = table.map(_ :+ deadState)
    
    /*
     * Our next generation may have some problems/errors during its execution. We must specify it in its
     * return type. In this case, errors will be represented as strings.
     */
    def nextGeneration(table: GameTable[A]): Either[String, GameTable[A]] = {
        
        /*
         * We must check the columns iteration result before the next row iteration. If an error at
         * column level happened, stop the execution as soon as possible and propagate it to upper
         * levels. Also, if an error happens during this execution, propagate it too!
         */
        @annotation.tailrec
        def iterateRows(rows: GameTable[A])(result: GameTable[A]): Either[String, GameTable[A]] = rows match {
            
            case north :: center :: south :: Nil => iterateCols(north, center, south)(List[A]()) match {
                
                case Right(r) => Right(result :+ r)
                case Left(l) => Left(l)
                
            }
                
            
            case north :: center :: south :: _ => iterateCols(north, center, south)(List[A]()) match {
                
                case Right(r) => iterateRows(rows.tail)(result :+ r)
                case Left(l) => Left(l)
                
            }
            
            case _ => Left("Less than three rows to iterate over.")
            
        }

        /*
         * If an error is detected, report it!
         */
        @annotation.tailrec
        def iterateCols(northRow: List[A], centerRow: List[A], southRow: List[A])(result: List[A]): Either[String, List[A]] = (northRow, centerRow, southRow) match {
            
            case (nw :: n :: ne :: Nil, w :: cell :: e :: Nil, sw :: s :: se :: Nil) =>
                Right(result :+ nextState(CellEnvironment(nw, n, ne, w, cell, e, sw, s, se)))
            
            case (nw :: n :: ne :: _, w :: cell :: e :: _, sw :: s :: se :: _) =>
                iterateCols(northRow.tail, centerRow.tail, southRow.tail)(result :+ nextState(CellEnvironment(nw, n, ne, w, cell, e, sw, s, se)))
            
            case _ => Left("Less than three columns to iterate over.")  // REPORTED
            
        }
        
        iterateRows(marginNorth(marginSouth(marginWest(marginEast(table)))))(List())
        
    }
    
    def nextState(env: CellEnvironment[A]): A = {
    
        val pop: Int = population(env)
        if ((env.cell == liveState && (pop == 2 || pop == 3)) || (env.cell == deadState && pop == 3)) liveState else deadState
        
    }
    
    /*
     * Stop the execution if a generation failed for some reason!
     */
    @annotation.tailrec
    def runGame(table: GameTable[A])(generations: Int): Either[String, GameTable[A]] =
        if (generations <= 0) Right(table)
        else nextGeneration(expandNorth(expandSouth(expandWest(expandEast(table))))) match {
            
            case Right(r) => runGame(r)(generations - 1)
            case Left(l) => Left(l)
            
        }
    
}

object Game {
    
    type GameTable[A] = List[List[A]]
    
    final case class CellEnvironment[A](nw: A, n: A, ne: A, w: A, cell: A, e: A, sw: A, s: A, se: A)
    
    implicit val gameOfInts: Game[Int] = new Game[Int] {
    
        val deadState: Int = 0
        val liveState: Int = 1
        
        def population(env: CellEnvironment[Int]): Int = env.nw + env.n + env.ne + env.w + env.e + env.sw + env.s + env.se
        
    }
    
    implicit val gameOfStrings: Game[String] = new Game[String] {
    
        val deadState: String = "."
        val liveState: String = "X"
        
        def population(env: CellEnvironment[String]): Int =
            (env.nw :: env.n :: env.ne :: env.w :: env.e :: env.sw :: env.s :: env.se :: Nil).map(s => if (s == deadState) 0 else 1).sum
        
    }
    
    implicit val gameOfBooleans: Game[Boolean] = new Game[Boolean] {
    
        val deadState: Boolean = false
        val liveState: Boolean = true
        
        def population(env: CellEnvironment[Boolean]): Int =
            (env.nw :: env.n :: env.ne :: env.w :: env.e :: env.sw :: env.s :: env.se :: Nil).map(s => if (s == deadState) 0 else 1).sum
        
    }
    
    // We must adapt the retun type to keep propagating the error to upper levels.
    def run[A](table: GameTable[A])(generations: Int)(implicit game: Game[A]): Either[String, GameTable[A]] = game.runGame(table)(generations)
    
}


object GameOfLife extends App {
    
    import Game.{GameTable, gameOfInts, gameOfStrings, gameOfBooleans}
    import util.Try
    
    // The game tables...
    val intTable: GameTable[Int] = List(List(0, 1, 0), List(1, 1, 1), List(0, 1, 0))
    val stringTable: GameTable[String] = List(List(".", "X", "."), List("X", "X", "X"), List(".", "X", "."))
    val booleanTable: GameTable[Boolean] = List(List(false, true, false), List(true, true, true), List(false, true, false))
    
    // ... and the (maybe) erroneous game executions!
    if (args.isEmpty) println("No generations specified. Aborting execution.") else {
        
        /*
         * Let's try to convert our generation parameter into an Int. If we can, everything will be fine.
         * If not... well, our user maybe thinks "asdqweasd" is a valid generation number.
         * 
         * We all know that feel bro.
         */
        val generations: Option[Int] = Try(args.head.toInt).toOption
        
        if (generations.isEmpty) println("Generations must be a number. Aborting execution.")
        else {
            
            // This is the highest level! We can manage the errors (if any) here!
            
            println("Running Game of Life ('Int' version):")
            Game.run(intTable)(generations.get).fold(l => println(s"Error: $l"), _.foreach(println))
            
            println("Running Game of Life ('String' version):")
            Game.run(stringTable)(generations.get).fold(l => println(s"Error: $l"), _.foreach(println))
            
            println("Running Game of Life ('Boolean' version):")
            Game.run(booleanTable)(generations.get).fold(l => println(s"Error: $l"), _.foreach(println))
            
        }
        
    }
    
    // Isn't this a cleaner way to handle errors? You can be honest!
    
    // That was all folks! Hope you enjoyed it!
    
}
