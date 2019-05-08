package io.circe.validator.literal

import org.typelevel.jawn.{SimpleFacade, Parser}
import io.circe.validator.Validator
import java.io.{PrintWriter, StringWriter}
import java.util.UUID
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

trait ValidatorMacrosTrait {
  val c: blackbox.Context
  import c.universe._

  case class Hole(hole: String, value: Tree)

  object Hole {
    // generate UUID and hope that its not part of context string...
    final def apply(value: Tree): Hole = Hole(UUID.randomUUID.toString, value)
  }

  def mkValidatorTreeFacade(holes: Seq[Hole]): SimpleFacade[Tree] =
    new SimpleFacade[Tree] {
      def jfalse(): Tree = q"_root_.io.circe.validator.falseValidator"
      def jtrue(): Tree  = q"_root_.io.circe.validator.trueValidator"
      def jnull(): Tree  = q"_root_.io.circe.validator.nullValidator"
      def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Tree = {
        val number = if (decIndex < 0 && expIndex < 0) {
          q"io.circe.JsonNumber.fromIntegralStringUnsafe(${s.toString})"
        } else {
          q"io.circe.JsonNumber.fromDecimalStringUnsafe(${s.toString})"
        }
        q"_root_.io.circe.validator.eqNumberValidator($number)"
      }
      def jstring(s: CharSequence): Tree = {
        val string = s.toString
        holes
          .find(_.hole == string)
          .fold(
            q"_root_.io.circe.validator.eqStringValidator($string)"
          )(_.value)
      }
      def jarray(vs: List[Tree]): Tree =
        q"_root_.io.circe.validator.arrayValidator(${vs.toVector})"

      def jobject(vs: Map[String, Tree]): Tree =
        q"_root_.io.circe.validator.objectValidator(${vs.toVector})"
    }

  final def parse(json: String, holes: Seq[Hole]): Either[Throwable, Tree] = {
    try Right(
      Parser.parseUnsafe(json)(mkValidatorTreeFacade(holes))
    )
    catch {
      case NonFatal(e) => Left(e)
    }
  }

  final def validatorStringContextTree(args: c.Expr[Any]*): Tree = {
    val Apply(_, Apply(_, parts) :: Nil) = c.prefix.tree
    val stringParts = parts map {
      case Literal(Constant(part: String)) => part
      case _                               => c.abort(c.enclosingPosition, "should not ever happen")
    }

    val holes = args.map(argument => Hole(argument.tree))

    val jsonString
        : String = stringParts.zip(holes.map(_.hole)).foldLeft("") {
      case (acc, (part, hole)) => {
        // quotation mark
        val qm = "\""
        s"$acc$part$qm$hole$qm"
      }
    } + stringParts.last

    parse(jsonString, holes) fold ({ e =>
      val sw = new StringWriter
      e.printStackTrace(new PrintWriter(sw))

      c.abort(
        c.enclosingPosition,
        s"Invalid JSON in interpolated string, ${sw.toString}"
      )
    }, identity)
  }

  final def validatorStringContext(args: c.Expr[Any]*): c.Expr[Validator] =
    c.Expr[Validator](validatorStringContextTree(args: _*))
}

class ValidatorMacros(val c: blackbox.Context) extends ValidatorMacrosTrait
