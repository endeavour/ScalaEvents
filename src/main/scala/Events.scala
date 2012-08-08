import collection.mutable

trait IEvent[TArgs] {
  def Subscribe(action: TArgs => Unit): Disposable
}

class Event[A] {

  val listeners: mutable.ArrayBuffer[(A) => Unit] = mutable.ArrayBuffer.empty

  def Trigger(args: A) {
    for (val listener <- listeners) {
      listener(args)
    }
  }

  def Publish(): IEvent[A] = {
    new IEvent[A] {
      def Subscribe(action: (A) => Unit): Disposable = {
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

  def Map[A, B](mapping: A => B)(evt: IEvent[A]): IEvent[B] = {
    new IEvent[B] {
      def Subscribe(action: B => Unit) = evt.Subscribe(a => action(mapping(a)))
    }
  }

  def Filter[A](filter: A => Boolean)(evt: IEvent[A]): IEvent[A] = {
    new IEvent[A] {
      def Subscribe(action: (A) => Unit) = evt.Subscribe(args => filter(args) match {
        case true => action(args)
        case false => ()
      })
    }
  }

  def Choose[A](choose: A => Option[A])(evt: IEvent[A]): IEvent[A] = {
    Filter((a: A) => choose(a) != None)(evt)
  }

  def Merge[A](first: IEvent[A])(second: IEvent[A]): IEvent[A] = {
    new IEvent[A] {
      def Subscribe(action: (A) => Unit) = {
        val disposable1 = first.Subscribe(action(_))
        val disposable2 = second.Subscribe(action(_))

        new ActionDisposable(() => {
          disposable1.dispose()
          disposable2.dispose()
        })
      }
    }
  }

  def Pairwise[A](evt: IEvent[A]): IEvent[(A, A)] = {
    new IEvent[(A, A)] {
      def Subscribe(action: ((A, A)) => Unit) = {
        var last: Option[A] = None
        evt.Subscribe(newA => {
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

  def Partition[A](predicate: A => Boolean)(evt: IEvent[A]): (IEvent[A], IEvent[A]) = {
    Split[A, A, A](e => if (predicate(e)) Left(e) else Right(e))(evt)
  }

  def Scan[U, T](aggregate: (U, T) => U)(seed: U)(source: IEvent[T]): IEvent[U] = {
    new IEvent[U] {
      def Subscribe(action: (U) => Unit) = {
        var currentValue = seed

        source.Subscribe(t => {
          currentValue = aggregate(currentValue, t)
          action(currentValue)
        })
      }
    }
  }

  def Split[T, U1, U2](choice: T => scala.Either[U1, U2])(evt: IEvent[T]): (IEvent[U1], IEvent[U2]) = {

    val (evt1, evt2) = (new Event[U1], new Event[U2])

    Map(choice)(evt).Subscribe(_ match {
      case Left(a) => evt1.Trigger(a)
      case Right(b) => evt2.Trigger(b)
    })

    (evt1.Publish(), evt2.Publish())
  }
}