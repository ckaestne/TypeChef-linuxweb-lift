package code.snippet.pc

import code.model.FileManager
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser, FeatureModel}
import net.liftweb.common.Full
import net.liftweb.http.S

import scala.xml.NodeSeq


object PCAnalysis {

    def getPC: Option[FeatureExpr] = S.param("pc") match {
        case Full(s) => Some(new FeatureExprParser(FeatureExprFactory.bdd).parse(s));
        case _ => None
    }

    def pc = <span>
        {getPC.map(_.toString).getOrElse("not a valid condition")}
    </span>

    def analysis: NodeSeq = getPC.map { fexpr =>

        def status(fm: FeatureModel): NodeSeq =
            if (!fexpr.isSatisfiable(fm)) <span style="color:red">contradiction</span>
            else if (fexpr.isTautology(fm)) <span style="color:red">tautology</span>
            else <span>satisfiable</span>


        return <div>
            <div>Plain:
                {status(FeatureExprFactory.bdd.featureModelFactory.empty)}
            </div>
            <div>Approx.fm:
                {status(FileManager.getFMApprox(FeatureExprFactory.bdd))}
            </div>
            <div>Dimacs.old:
                {status(FileManager.getFMDimacsOld(FeatureExprFactory.bdd))}
            </div>
            <div>Dimacs:
                {status(FileManager.getFMDimacs(FeatureExprFactory.bdd))}
            </div>
        </div>
    }.getOrElse(<span></span>)


    def solutions = getPC.map { fexpr =>
        <ul>
            {val solutions = fexpr.asInstanceOf[BDDFeatureExpr].getBddAllSat.toList.sortBy(_.length)
        for (cnf <- solutions.take(10))
        yield <li>
            {cnf.map(l => if (l._1 == 0) "!" + l._2 else l._2).mkString(" && ")}
        </li>}
        </ul>
    }.getOrElse(<span></span>)
}
