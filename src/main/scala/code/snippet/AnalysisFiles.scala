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




object AnalysisFilesTable {
    def render = {
        S.param("refresh") match {
            case Full(_) => FileManager.resetCache()
            case _ =>
        }
        val isFilter = S.param("failed") match {
            case Full(_) => true
            case _ => false
        }

        var status = FileManager.fileStatusList.filter(s=> !s._2 || !isFilter)



        "#my_tr *" #> status.map(values => "#my_td1 *" #> values._1 & "#my_td2 *" #> values._3 & "#my_td3 *" #> genButton(values._1))
    }

    def genButton(f:String) =
        <div><a href={"/details?file="+urlEncode(f)}>details</a>
            <a href={"/reset?file="+urlEncode(f)}>reset</a></div>
}

