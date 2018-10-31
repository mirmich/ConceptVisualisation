package visualisation

import scala.collection.mutable.ListBuffer


object ConceptVisualisation {

  class Concept(concept: (Set[Int],Set[Char]), smaller: Array[Boolean], bigger: Array[Boolean], un: Array[Boolean]) {
    def getConcept = concept
    def getSmaller = smaller
    def getBigger = bigger
    def getUnComparable = un

    override def toString = {
      s"Concept: ${getConcept toString()}\nSmallerConcepts ${(getSmaller deep) toString}\n" +
        s"BiggerConcepts: ${(getBigger deep) toString}\nUnComparables: ${(getUnComparable deep) toString}"
    }

  }

  class SuperConceptFinder (var concepts:List[(Set[Int],Set[Char])]){

    private def isSubconcept(concept1: Concept, concept2: Concept) : Boolean = {

      (((concept1 getConcept) _1) subsetOf ((concept2 getConcept) _1)) &&
        (((concept2 getConcept) _2) subsetOf ((concept1 getConcept) _2))
    }

    private def isUnComparable(concept1: Concept, concept2: Concept) : Boolean = {
      !isSubconcept(concept1,concept2) && !isSubconcept(concept2,concept1)
    }

    private def getRelations(concept: Concept, listOfConcepts: ListBuffer[Concept]): Concept ={

      var bigger = new Array[Boolean](listOfConcepts.size)
      var smaller = new Array[Boolean](listOfConcepts.size)
      var unComparable = new Array[Boolean](listOfConcepts.size)

      for(e <- listOfConcepts) {
        val position : Int = listOfConcepts indexOf e
        smaller(position) = isSubconcept(e,concept)
        bigger(position) = isSubconcept(concept,e)
        unComparable(position) = isUnComparable(concept,e)
      }

      new Concept(concept getConcept,smaller,bigger,unComparable)

    }

    def getConcepts(): ListBuffer[Concept] = {

      val listOfConcepts : ListBuffer[Concept] = new ListBuffer[Concept]()
      val numberOfConcepts = concepts.size

      concepts foreach(
        listOfConcepts += new Concept( _,
          new Array[Boolean](numberOfConcepts),
          new Array[Boolean](numberOfConcepts),
          new Array[Boolean](numberOfConcepts)))

      listOfConcepts.map(e => getRelations(e,listOfConcepts))
    }
  }

  def main(args: Array[String]): Unit = {

    val c0 = (Set(1,2,3,4,5,6,7,8), Set('a'))
    val c1 = (Set(1,2,3,4),         Set('a','g'))
    val c2 = (Set(2,3,4),           Set('a','g','h'))
    val c3 = (Set(5,6,7,8),         Set('a','d'))
    val c4 = (Set(5,6,8),           Set('a','d','f'))
    val c5 = (Set(3,4,6,7,8),       Set('a','c'))
    val c6 = (Set(3,4),             Set('a','c','g','h'))
    val c7 = (Set(4),               Set('a','c','g','h','i'))
    val c8 = (Set(6,7,8),           Set('a','c','d'))
    val c9 = (Set(6,8),             Set('a','c','d','f'))
    val c10 = (Set(7),              Set('a','c','d','e'))
    val c11 = (Set(1,2,3,5,6),      Set('a','b'))
    val c12 = (Set(1,2,3),          Set('a','b','g'))
    val c13 = (Set(2,3),            Set('a','b','g','h'))
    val c14 = (Set(5,6),            Set('a','b','d','f'))
    val c15 = (Set(3,6),            Set('a','b','c'))
    val c16 = (Set(3),              Set('a','b','c','g','h'))
    val c17 = (Set(6),              Set('a','b','c','d','f'))
    val c18 = (Set[Int](),               Set('a','b','c','d','e','f','g','h','i'))

    val list = List(c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18)

    def countTrue(boolArray : Array[Boolean]) = boolArray.count(v => v)





    val superConceptFinder = new SuperConceptFinder(list)
    var concepts = superConceptFinder getConcepts()
    concepts.foreach(e => println(countTrue(e getSmaller)))

    //println(countTrue(concept getSmaller))

    println(concepts(5) toString)
    //println()
    //println((superConceptFinder getConcepts())(1) toString)
  }
}