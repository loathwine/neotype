package comptime

/** Exception thrown to signal an explicit compile-time error.
  *
  * When thrown during compile-time evaluation, this becomes a compile error
  * with the provided message. Use via the `comptimeError` helper:
  *
  * {{{
  * comptime {
  *   if condition then value
  *   else comptimeError("Condition not met")
  * }
  * }}}
  */
case class ComptimeAbort(message: String) extends Exception(message)

/** Exception used by compile-time evaluation paths that can only propagate
  * Throwables (e.g. inside Eval.run closures or PartialFunction methods). The
  * wrapped [[ComptimeError]] is preserved so MacroEntry can format it without
  * round-tripping through a string message.
  */
final case class ComptimeFailure(error: ComptimeError) extends Exception:
  override def getMessage: String = ComptimeError.format(error)
