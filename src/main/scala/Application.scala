import scala._

object Main extends App {
  override def main(args:Array[String]){

    val testEvent = new Event[Int]
    testEvent.trigger(1)
    testEvent.trigger(2)
    testEvent.trigger(3)

    val evt = testEvent.publish()

    val (odd, even) = Event.partition[Int](_ % 2 == 0)(evt)

    odd.subscribe(i => println("Odd: " + i))
    even.subscribe(i => println("Even: " + i))

    for (i <- 1 to 100)
    {
      testEvent.trigger(i)
    }
  }
}