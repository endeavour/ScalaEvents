import java.util.concurrent.atomic.AtomicBoolean

trait Disposable {
  def dispose()
}

class ActionDisposable(action: () => Unit) extends Disposable {
  var hasDisposed: AtomicBoolean = new AtomicBoolean(false)

  def dispose() {
    if (hasDisposed.compareAndSet(false, true)) {
      action()
    }
  }
}