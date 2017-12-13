package hourofcode2017.scala.gameoflife.v4


/*
 * Hour of Code 2017: Introduction to Functional Programming with Scala 
 * author:  Andrés López
 * version: 4
 *
 * Fourth Game of Life implementation. Woho! Look at that!
 *
 * This version covers:
 *   - traits
 *   - objects
 *   - case classes
 *   - type classes
 *   - polymorphism
 *
 */
 
 /*
  * Last version was cool (types are), but we can do it better:
  *   - create different game implementations easier
  *   - create a type class to implicitly run games
  */
sealed trait Game[A] {
    
    /*
     * Our type parametrized functionality is reunited here now. We will only need to specify the allowed
     * states and how to calculate a cell surrounding population for next implementations!
     */
    
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
    
    def nextGeneration(table: GameTable[A]): GameTable[A] = {
        
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
                result :+ nextState(CellEnvironment(nw, n, ne, w, cell, e, sw, s, se))
            
            case (nw :: n :: ne :: _, w :: cell :: e :: _, sw :: s :: se :: _) =>
                iterateCols(northRow.tail, centerRow.tail, southRow.tail)(result :+ nextState(CellEnvironment(nw, n, ne, w, cell, e, sw, s, se)))
            
            case _ => throw new Error("Less than three columns to iterate over.")
            
        }
        
        iterateRows(marginNorth(marginSouth(marginWest(marginEast(table)))))(List())
        
    }
    
    def nextState(env: CellEnvironment[A]): A = {
    
        val pop: Int = population(env)
        if ((env.cell == liveState && (pop == 2 || pop == 3)) || (env.cell == deadState && pop == 3)) liveState else deadState
        
    }
    
    @annotation.tailrec
    def runGame(table: GameTable[A])(generations: Int): GameTable[A] =
        if (generations <= 0) table
        else runGame(nextGeneration(expandNorth(expandSouth(expandWest(expandEast(table))))))(generations - 1)
    
}

object Game {
    
    type GameTable[A] = List[List[A]]
    
    // We can model a cell environment like this
    final case class CellEnvironment[A](nw: A, n: A, ne: A, w: A, cell: A, e: A, sw: A, s: A, se: A)
    
    // A so easy Int,...
    implicit val gameOfInts: Game[Int] = new Game[Int] {
    
        val deadState: Int = 0
        val liveState: Int = 1
        
        def population(env: CellEnvironment[Int]): Int = env.nw + env.n + env.ne + env.w + env.e + env.sw + env.s + env.se
        
    }
    
    // ... String...
    implicit val gameOfStrings: Game[String] = new Game[String] {
    
        val deadState: String = "."
        val liveState: String = "X"
        
        def population(env: CellEnvironment[String]): Int =
            (env.nw :: env.n :: env.ne :: env.w :: env.e :: env.sw :: env.s :: env.se :: Nil).map(s => if (s == deadState) 0 else 1).sum
        
    }
    
    // ... and Boolean type class implementations!
    implicit val gameOfBooleans: Game[Boolean] = new Game[Boolean] {
    
        val deadState: Boolean = false
        val liveState: Boolean = true
        
        def population(env: CellEnvironment[Boolean]): Int =
            (env.nw :: env.n :: env.ne :: env.w :: env.e :: env.sw :: env.s :: env.se :: Nil).map(s => if (s == deadState) 0 else 1).sum
        
    }
    
    /*
     * We need to run a game table which game rules are implicitly loaded. How? Well, something like this will
     * help us.
     */
    def run[A](table: GameTable[A])(generations: Int)(implicit game: Game[A]): GameTable[A] = game.runGame(table)(generations)
    
}


object GameOfLife extends App {
    
    import Game.{GameTable, gameOfInts, gameOfStrings, gameOfBooleans}
    
    // Hey, that are tons of tables!
    val intTable: GameTable[Int] = List(List(0, 1, 0), List(1, 1, 1), List(0, 1, 0))
    val stringTable: GameTable[String] = List(List(".", "X", "."), List("X", "X", "X"), List(".", "X", "."))
    val booleanTable: GameTable[Boolean] = List(List(false, true, false), List(true, true, true), List(false, true, false))
    
    // Will our program know how to run each table? Make your bets, ladies and gentlemen!
    if (args.isEmpty) println("No generations specified. Aborting execution.") else {
        
        val generations: Int = args.head.toInt
            
        println("Running Game of Life ('Int' version):")
        Game.run(intTable)(generations).foreach(println)
        
        println("Running Game of Life ('String' version):")
        Game.run(stringTable)(generations).foreach(println)
        
        println("Running Game of Life ('Boolean' version):")
        Game.run(booleanTable)(generations).foreach(println)
        
    }
    
    // Would you be able to add more type class implementations?
    
}
