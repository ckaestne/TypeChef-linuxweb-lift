package code.model

import java.io.File

import de.fosd.typechef.featureexpr.{AbstractFeatureExprFactory, FeatureExpr, FeatureExprFactory, FeatureExprParser}

import scala.xml.{NodeSeq, XML}

object FileManager {


    val rootDir = new File("/usr0/home/ckaestne/work/TypeChef/LinuxAnalysis/")
    val linuxDir = new File(rootDir, "l")
    val fileListFile = new File(rootDir, "pcs/x86.flist")

    def resetCache(): Unit = {
        _fileList = None
        _fileStatusList = None
    }

    var _fileList: Option[List[String]] = None

    def fileList = {
        if (!_fileList.isDefined)
            _fileList = Some(getLines(fileListFile))
        _fileList.get
    }

    def getLines(file: File): List[String] = {
        val source = scala.io.Source.fromFile(file)
        val r = source.getLines().toList
        source.close()
        r
    }

    var _fileStatusList: Option[List[(String, Boolean, String)]] = None
    /**
     * gets a status. format:
     * (Filename, Succeeded?, ErrorMsg)
     */
    def fileStatusList: List[(String, Boolean, String)] = {
        if (!_fileStatusList.isDefined)
            _fileStatusList = Some(fileList map analyzeFile)
        _fileStatusList.get
    }


    def error(msg: String, isComment: Boolean): String = {
        if (isComment)
            "COMMENT"
        else
            "FAIL: %s".format(msg)
    }

    def analyzeFile(filename: String): (String, Boolean, String) = {
        val file = new File(linuxDir, filename + ".dbg")
        val commentfile = new File(linuxDir, filename + ".comment")
        val commentExists = commentfile.exists

        if (!file.exists) {
            (filename, false, "waiting for TypeChef")
        } else {
            val lines = getLines(file)

            if (lines.filterNot(_.trim.length == 0).isEmpty)
                (filename, false, error("file empty", commentExists))
            else
            if (!lines.exists(_ == "True\tlexing succeeded"))
                (filename, false, error("lexing failed", commentExists))
            else
            if (!lines.exists(_ == "True\tparsing succeeded"))
                (filename, false, error("parsing failed", commentExists))
            else
            if (!lines.exists(_ == "No type errors found."))
                (filename, false, error("type checking failed", commentExists))
            else
                (filename, true, "SUCCESS ")
        }
    }

    //deletes the .dbg file and returns whether successful
    def resetFile(filename: String): Boolean = {
        def cleanFile(ext: String) = {
            val file = new File(linuxDir, filename + ext)
            if (file.exists()) file.delete()
        }
        val file = new File(linuxDir, filename + ".dbg")
        cleanFile(".err")
        cleanFile(".c.xml")
        cleanupDebugOutput(filename)
        file.exists() && file.delete()
    }

    /**
     * removes files no longer needed after a successful run:
     *
     * .pi*
     */
    def cleanupDebugOutput(filename: String): Unit = {
        def cleanFile(ext: String) = {
            val file = new File(linuxDir, filename + ext)
            if (file.exists()) file.delete()
        }
        cleanFile(".pi")
        cleanFile(".pi.dbgSrc")
        cleanFile(".pi.macroDbg")
    }

    def getFilePC(filename: String): FeatureExpr = {
        val file = new File(linuxDir, filename + ".pc")
        if (file.exists())
            new FeatureExprParser().parseFile(file)
        else FeatureExprFactory.True
    }

    def getErrors(filename: String): Seq[(String, FeatureExpr, String, (String, Int, Int))] = {
        val file = new File(linuxDir, filename + ".c.xml")
        if (!file.exists()) return Nil
        val xml = XML.loadFile(file)

        def parsePosition(n: NodeSeq): (String, Int, Int) = (n \ "file" text, (n \ "line" text).trim.toInt, (n \ "col" text).trim.toInt)

        val pes = for (parsererror <- xml \\ "parsererror") yield {
            val fexpr = new FeatureExprParser().parse(parsererror \ "featurestr" text)
            val msg = parsererror \ "msg" text
            val pos = parsePosition(parsererror \ "position" head)
            (msg, fexpr, "parser error", pos)
        }
        val tes = for (typeerror <- xml \\ "typeerror") yield {
            val fexpr = new FeatureExprParser().parse(typeerror \ "featurestr" text)
            val msg = typeerror \ "msg" text
            val severity = (typeerror \ "severity" text) + " / " + (typeerror \ "severityextra" text)
            val pos = parsePosition(typeerror \ "position" head)
            (msg, fexpr, "parser error", pos)
        }
        pes ++ tes
    }

    def getDbgOutput(filename: String): String = {
        val file = new File(linuxDir, filename + ".dbg")
        if (file.exists())
            getLines(file).mkString("\n")
        else ""
    }
    def getErrorOutput(filename: String): String = {
        val file = new File(linuxDir, filename + ".err")
        if (file.exists())
            getLines(file).mkString("\n")
        else ""
    }
    def getComments(filename: String): String = {
        val file = new File(linuxDir, filename + ".comment")
        if (file.exists())
            getLines(file).mkString("\n")
        else ""
    }


    def getFMApprox(f: AbstractFeatureExprFactory = FeatureExprFactory.dflt) =
        f.featureModelFactory.create(new FeatureExprParser(f).parseFile(new File(rootDir, "approx.fm")))
    def getFMDimacsOld(f: AbstractFeatureExprFactory = FeatureExprFactory.dflt) =
        f.featureModelFactory.createFromDimacsFile(scala.io.Source.fromFile(new File(rootDir, "2.6.33.3-2var.dimacs")))
    def getFMDimacs(f: AbstractFeatureExprFactory = FeatureExprFactory.dflt) =
        f.featureModelFactory.createFromDimacsFile(scala.io.Source.fromFile(new File(rootDir, "pcs/x86.dimacs")))

}
