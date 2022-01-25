package object utilities {

  def memoize_function[K, V](f: K => V): K => V = {
    val cache = collection.mutable.Map.empty[K, V]

    k =>
      cache.getOrElse(k, {
        cache.update(k, f(k))
        cache(k)
      })
  }


}
