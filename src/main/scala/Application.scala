import scala._

object Main extends App {
  override def main(args:Array[String]){

    val testEvent = new Event[Int]
    testEvent.Trigger(1)
    testEvent.Trigger(2)
    testEvent.Trigger(3)

    val evt = testEvent.Publish()

    val (odd, even) = Event.Partition[Int](_ % 2 == 0)(evt)

    odd.Subscribe(i => println("Odd: " + i))
    even.Subscribe(i => println("Even: " + i))

    for (i <- 1 to 100)
    {
      testEvent.Trigger(i)
    }
  }
}