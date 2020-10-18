package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  import java.net.URLDecoder

  override def handle(request: Request): Response = {
    val pathSplit = request.path.split('?')
    val path = pathSplit(0)
    //    var queryMap: Map[String, String] = Map()
    println(path)
    val paramMap: Map[String, String] = request.body match {
      case Some(body) =>
        var paramMap: Map[String, String] = Map()
        val paramList = body.split('&')
        val Pattern = """(.*?)=(.*?)$""".r
        for (paramPair <- paramList) {
          val patternMatch = Pattern.findAllIn(paramPair)
          val name = patternMatch.group(1)
          val value = URLDecoder.decode(patternMatch.group(2), "UTF-8")
          paramMap += (name -> value)
          //          println(paramPair)
        }
        paramMap
      case None => Map()
    }
    if (request.method == "POST") {
      println(1)
      path match {
        case "/name" => name(paramMap)
        case "/sex" => sex(paramMap)
        case "/message" => message(paramMap)
        case "/confirm" => confirm(paramMap)
        case "/submit" => submit()
        case _ => NotFound(s"Requested resource '$path' for ${request.method} is not found.")
      }
    } else if (request.method == "GET") {
      println(2)
      path match {
        case "/" => index()
        case _ => NotFound(s"Requested resource '$path' for ${request.method} is not found.")
      }
    } else {
      println(3)
      NotFound(s"Requested resource '$path' for ${request.method} is not found.")
    }
  }

  def hiddenTags(paramMap: Map[String, String]): String = {
    var hiddenStr = ""
    for ((name, value) <- paramMap) {
    hiddenStr += s"""<input type="hidden" name="$name" value="$value">\n"""
  }
    hiddenStr
  }

  def index(): Response = {
    Ok(
      s"""<html>
         |<body>
         |    <form action="/name" method="post">
         |        <input type="submit" value="start" />
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  def name(paramMap: Map[String, String]): Response = {
    val name = paramMap.getOrElse("name","")
    val hiddenStr =hiddenTags(paramMap-  "name")
      Ok(
        s"""<html>
           |<body>
           |<form id="name-form" action="/sex" method="post">
           |    <label>名前: </label>
           |    <input name="name" type="text" value="$name"><br>
           |        $hiddenStr
           |    <input type="submit" value="next">
           | </form>
           |</body>
           |</html>""".stripMargin)
  }

  def sex(paramMap: Map[String, String]): Response = {
    val sex = paramMap.getOrElse("sex","")
    val hiddenStr =hiddenTags(paramMap-  "sex")
    Ok(
      s"""<html>
         |<body>
         |<form id="sex-form" action="/message" method="post">
         |     <label for="sex">性別: </label>
         |     <input type="radio" name="sex" value="male" ${if(sex == "male")"""checked="checked"""" else ""}>男性
         |     <input type="radio" name="sex" value="female" ${if(sex == "female")"""checked="checked"""" else ""}>女性
         |     <br>
         |    $hiddenStr
         |    <input type="submit" value="next">
         | </form>
         |<form id="sex-back" action="/name" method="post">
         |    $hiddenStr
         |    <input type="submit" value="back">
         | </form>
         |</body>
         |</html>""".stripMargin)
  }

  def message(paramMap: Map[String, String]): Response = {
    val impressions = paramMap.getOrElse("impressions","")
    val hiddenStr =hiddenTags(paramMap-  "impressions")
    Ok(
      s"""<html>
         |<body>
         |<form id="impressions-form" action="/confirm" method="post">
         |     <label>メッセージ: </label><br>
         |    <textarea name="impressions" form="impressions-form">$impressions</textarea><br>
         |    $hiddenStr
         |    <input type="submit" value="next">
         |</form>
         |<form id="sex-back" action="/sex" method="post">
         |    $hiddenStr
         |    <input type="submit" value="back">
         | </form>
         |</body>
         |</html>""".stripMargin)
  }

  def confirm(paramMap: Map[String, String]): Response = {
    val hiddenStr =hiddenTags(paramMap)
    Ok(
      s"""<html>
         |<body>
         |    <label>名前: </label><span id="submitted-name">${paramMap("name")}</span><br>
         |    <label>性別: </label><span id="submitted-sex">${paramMap("sex")}</span><br>
         |    <label>メッセージ</label><br>
         |    <textarea name="submitted-impressions" disabled="">${paramMap("impressions")}</textarea>
         |    <form action="/submit" method="post">
         |    <input type="submit" value="submit">
         |    </form>
         |    <form id="impressions-back" action="/message" method="post">
         |        $hiddenStr
         |        <input type="submit" value="back">
         |    </form>
         |    </div>
         |</body>
         |</html>""".stripMargin)
  }

  def submit(): Response = {
    Ok(
      """<html>
        |<body>
        |    <p>送信完了</p>
        |    <form action="/" method="get">
        |        <input type="submit" value="start" />
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }
}
