package code.snippet

import java.io._
import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import net.liftweb.http.{S, SHtml}
import SHtml._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.SetHtml
import scala.xml.{NodeSeq, Text}

import code.model._

object Reset {

    def render = {

        val n=S.param("file") match {
            case Full(n) => Some(n)
            case _ => None
        }

        println("reseting "+n)

        val success = n.flatMap(
            f=>if (FileManager.resetFile(f)) Some(f) else None )

        val msg = n.map("Successfully reset "+_).getOrElse("file not found")


        "div" #> <div>successfully reset {n}</div>
    }

}
