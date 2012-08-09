import collection.mutable

trait IEvent[TArgs] {
  def subscribe(action: TArgs => Unit): Disposable
}

class Event[A] {

  val listeners: mutable.ArrayBuffer[(A) => Unit] = mutable.ArrayBuffer.empty

  def trigger(args: A) {
    for (val listener <- listeners) {
      listener(args)
    }
  }

  def publish(): IEvent[A] = {
    new IEvent[A] {
      def subscribe(action: (A) => Unit): Disposable = {
        listeners += action

        new Disposable {
          def dispose() {
            if (listeners.contains(action)) {
              listeners -= action
            }
          }
        }
      }
    }
  }
}

object Event {

  def map[A, B](mapping: A => B)(evt: IEvent[A]): IEvent[B] = {
    new IEvent[B] {
      def subscribe(action: B => Unit) = evt.subscribe(a => action(mapping(a)))
    }
  }

  def filter[A](filter: A => Boolean)(evt: IEvent[A]): IEvent[A] = {
    new IEvent[A] {
      def subscribe(action: (A) => Unit) = evt.subscribe(args => filter(args) match {
        case true => action(args)
        case false => ()
      })
    }
  }

  def choose[A](choose: A => Option[A])(evt: IEvent[A]): IEvent[A] = {
    filter((a: A) => choose(a) != None)(evt)
  }

  def merge[A](first: IEvent[A])(second: IEvent[A]): IEvent[A] = {
    new IEvent[A] {
      def subscribe(action: (A) => Unit) = {
        val disposable1 = first.subscribe(action(_))
        val disposable2 = second.subscribe(action(_))

        new ActionDisposable(() => {
          disposable1.dispose()
          disposable2.dispose()
        })
      }
    }
  }

  def pairwise[A](evt: IEvent[A]): IEvent[(A, A)] = {
    new IEvent[(A, A)] {
      def subscribe(action: ((A, A)) => Unit) = {
        var last: Option[A] = None
        evt.subscribe(newA => {
          last match {
            case None => {
              last = Some(newA)
            }
            case Some(oldA) => {
              action(oldA, newA)
              last = Some(newA)
            }
          }
        })
      }
    }
  }

  def partition[A](predicate: A => Boolean)(evt: IEvent[A]): (IEvent[A], IEvent[A]) = {
    split[A, A, A](e => if (predicate(e)) Left(e) else Right(e))(evt)
  }

  def scan[U, T](aggregate: (U, T) => U)(seed: U)(source: IEvent[T]): IEvent[U] = {
    new IEvent[U] {
      def subscribe(action: (U) => Unit) = {
        var currentValue = seed

        source.subscribe(t => {
          currentValue = aggregate(currentValue, t)
          action(currentValue)
        })
      }
    }
  }

  def split[T, U1, U2](choice: T => scala.Either[U1, U2])(evt: IEvent[T]): (IEvent[U1], IEvent[U2]) = {

    val (evt1, evt2) = (new Event[U1], new Event[U2])

    map(choice)(evt).subscribe(_ match {
      case Left(a) => evt1.trigger(a)
      case Right(b) => evt2.trigger(b)
    })

    (evt1.publish(), evt2.publish())
  }
}