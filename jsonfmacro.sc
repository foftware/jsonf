import scala.reflect.macros.blackbox

class JsonFLiteralMacros(val c: blackbox.Context) {
  import c.universe._



  final def jsonfStringContext(args: c.Expr[Any]*): c.Expr[String] = {
    c.prefix.tree match {
      case Apply(_, Apply(_, parts) :: Nil) => {
        val stringParts = parts map {
          case Literal(Constant(part: String)) => part
          case _ => c.abort(c.enclosingPosition, "should not ever happen")
        }  

        val jsonString = stringParts.zip(args).foldLeft(""){
          case (acc, (a, part)) => s"$acc$a$part"
        } + stringParts.last

        println(jsonString)


        ???
      }
    }
  }
}
