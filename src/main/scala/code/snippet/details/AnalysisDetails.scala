package code.snippet

import code.model._
import code.snippet.details.AnalysisDetails
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.util.Helpers._

package details {

object AnalysisDetails {

    def fileparam = S.param("file") match {
        case Full(s) => Some(s);
        case _ => None
    }


    def filename = <span>
        {fileparam.getOrElse("file not found")}
    </span>


    def filepc = fileparam.map(FileManager.getFilePC).getOrElse(FeatureExprFactory.True)

    def filepresencecondition = <div><span>
        {filepc.toTextExpr}
    </span> ({genButton(filepc)})</div>

    def log = <pre>
        {fileparam.map(FileManager.getDbgOutput).getOrElse("no log file found")}
    </pre>
    def errorlog = <pre>
        {fileparam.map(FileManager.getErrorOutput).getOrElse("no log file found")}
    </pre>

    def errors = fileparam.map(FileManager.getErrors).getOrElse(List())

    def comments = <div>{fileparam.map(FileManager.getComments).getOrElse("")}</div>


    def genButton(pc:FeatureExpr) =
        <a href={"/pc?pc="+urlEncode(pc.toTextExpr)}>debug&nbsp;pc</a>

    def resetButton =
        <div><a href={"/reset?file="+urlEncode(fileparam.getOrElse(""))}>reset</a></div>
}

}

object ErrorTable {

    import code.snippet.details.AnalysisDetails._

    def render = {

        def presentErr(err: (String,FeatureExpr,String,(String,Int,Int))) = List(
            <span>{err._1}</span>,
            <span>{err._2.toString}<br />{genButton(filepc and err._2)}</span>,
            <span>{err._3}</span>,
            <span>{err._4._1 + ":" + err._4._2 + ":" + err._4._3}</span>
        )


        "#my_tr *" #> errors.map(err => "td *" #> presentErr(err))
    }


}
