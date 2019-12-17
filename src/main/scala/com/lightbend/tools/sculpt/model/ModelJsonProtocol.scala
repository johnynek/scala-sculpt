// Copyright (C) 2015-2018 Lightbend Inc. <http://lightbend.com>

package com.lightbend.tools.sculpt.model

import spray.json._
import com.lightbend.tools.sculpt.util.RegexInterpolator

/** JSON serialization/deserialization for the Sculpt model types */
object ModelJsonProtocol extends DefaultJsonProtocol {

  implicit object entityFormat extends JsonFormat[Entity] {
    def write(e: Entity) = new JsString(e.kind.prefix + ":" + e.name)
    def read(value: JsValue) = value.convertTo[String] match {
      case r"ov:(.*)$n"  => Entity(n, EntityKind.Module)
      case r"def:(.*)$n" => Entity(n, EntityKind.Method)
      case r"var:(.*)$n" => Entity(n, EntityKind.Mutable)
      case r"mac:(.*)$n" => Entity(n, EntityKind.Macro)
      case r"pk:(.*)$n" =>  Entity(n, EntityKind.Package)
      case r"t:(.*)$n"   => Entity(n, EntityKind.Term)
      case r"tr:(.*)$n"  => Entity(n, EntityKind.Trait)
      case r"pkt:(.*)$n" => Entity(n, EntityKind.PackageType)
      case r"o:(.*)$n"   => Entity(n, EntityKind.ModuleClass)
      case r"cl:(.*)$n"  => Entity(n, EntityKind.Class)
      case r"tp:(.*)$n"  => Entity(n, EntityKind.Type)
      case r"file:(.*)$n"  => Entity(n, EntityKind.File)
      case _ => throw new DeserializationException("'EntityKind:Name' string expected")
    }
  }

  implicit object pathFormat extends JsonFormat[Path] {
    def write(p: Path) = p.elems.toJson
    def read(value: JsValue) = Path(value.convertTo[Vector[Entity]])
  }

  implicit object fullDependencyFormat extends JsonFormat[FullDependency] {
    val usesStr = "uses"
    val extendsStr = "extends"
    val declaresStr = "declares"

    def write(d: FullDependency) = {
      val data = Seq(
        "sym" -> d.from.toJson,
        (d.kind match {
          case DependencyKind.Extends => extendsStr
          case DependencyKind.Uses => usesStr
          case DependencyKind.Declares => declaresStr
        }) -> d.to.toJson
      )
      JsObject((if(d.count == 1) data else data :+ ("count" -> JsNumber(d.count))): _*)
    }

    def read(m: Map[String, JsValue], str: String, k: DependencyKind): Option[(DependencyKind, Path)] =
      m.get(str).map { v => (k, v.convertTo[Path]) }

    def read(value: JsValue) = {
      val m = value.asJsObject.fields
      val from = m("sym").convertTo[Path]
      val (kind, to) =
        read(m, usesStr, DependencyKind.Uses)
          .orElse(read(m, extendsStr, DependencyKind.Extends))
          .orElse(read(m, declaresStr, DependencyKind.Declares))
          .getOrElse(sys.error(s"expected one of {uses, extends, declares} in ${m.keys}"))

      val count = m.get("count").map(_.convertTo[Int]).getOrElse(1)
      FullDependency(from, to, kind, count)
    }
  }
}
