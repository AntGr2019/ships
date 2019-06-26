object GameWarShips {


  type Point = (Int, Int)
  type Field = Vector[Vector[Boolean]]
  type Ship = List[Point]
  type Fleet = Map[String, Ship]
  type Game = (Field, Fleet)

  val fleet : Fleet = Map[String, Ship]()
  val field : Field = Vector(Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false),
    Vector(false,false,false,false,false,false,false,false,false,false))
  val game1 : Game =  (field, fleet)

  def validateShip(ship: Ship): Boolean = { var flag = true
    if (ship.size == 1) flag = true
    else {
      if (Math.abs(ship.size) <= 4) {
        for (i <- 0 until ship.size) {
          if (i != 0) {
            if (!((ship(i)._1 == ship(i - 1)._1) || (ship(i)._2 == ship(i - 1)._2))) flag = false
          }
          else {
            if (!((ship(i)._1 == ship(i + 1)._1) || (ship(i)._2 == ship(i + 1)._2))) flag = false
          }
        }
      } else flag = false}
    flag
  }

  def validatePosition(ship: Ship, field: Field): Boolean = {
    var flag = true

    var i0 = ship(0)._1-1
    var in = ship(ship.size-1)._1+1
    var j0 = ship(0)._2-1
    var jn = ship(ship.size-1)._2+1

    for(i<- 0 to ship.size-1) {
      if (ship(i)._1 == 0) {i0 = ship(0)._1; in = ship(ship.size-1)._1+1}
      else if (ship(i)._1 == 9) {in = ship(ship.size-1)._1; i0 = ship(0)._1-1}

      if (ship(i)._2 == 0) {j0 = ship(0)._2; jn = ship(ship.size-1)._2+1}
      else if (ship(i)._2 == 9) {jn = ship(ship.size-1)._2; j0 = ship(0)._2-1}
    }

    for(i<- i0 to in; j <- j0 to jn) {
      if (field(i)(j) == true) flag = false
    }
    flag
  }
  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
    var fleetNew = fleet
    val s = (name, ship)
    fleetNew += s
    fleetNew
  }
  def markUsedCells(field: Field, ship: Ship): Field = {
    var fieldNew : Field = field
    def go1(field: Field, ship: Ship, i : Int) : Field = i match {
      case i if i < ship.size => {
        val fieldUp : Field = field.
          updated(ship(i)._1, field(ship(i)._1).updated(ship(i)._2, true))
        go1(fieldUp, ship, i+1)
      }
      case i => field
    }
    go1(field, ship, 0)
  }

  def tryAddShip(game: Game, name: String, ship: Ship): Game = {
    var updateGame : Game = game
    if (validateShip(ship) == true)
      if (validatePosition(ship, game._1) == true) {
        val updateFleet : Fleet = enrichFleet(game._2, name, ship)
        val updateField : Field = markUsedCells(game._1, ship)
        updateGame = (updateField, updateFleet)
      }
    updateGame
  }

  def inputShip(lSh : Ship, lN : String) : (Ship,String) = {
    (lSh, lN)
  }

  val listShips : List[Ship] = List(List((1,6),(1,7),(1,8)), List((2,5),(3,5),(4,5),(5,5)), List((9,9)))
  val listNames : List[String] = List("Black Perl","MillenniumFalcon","Varyag")

  val numberOfShips = 3

  def recurs1(game : Game, max : Int, currentNumber : Int):Game = {
    def go(game: Game, currentNumber: Int):(Game, Int) = currentNumber match {
      case i if i < max => {
        val ship_Name = inputShip(listShips(currentNumber), listNames(currentNumber))
        val ship = ship_Name._1
        val name = ship_Name._2
        println(ship_Name)
        val nGame = tryAddShip(game, name, ship)
        go(nGame, currentNumber+1)
      }
      case i => println//"(curNum = "+currentNumber)
        (game, currentNumber)
    }

    go(game, 0)._1
  }


  val newGame = recurs1(game1, numberOfShips, 0)
  newGame._2.keys.toList.foreach(println)

}
