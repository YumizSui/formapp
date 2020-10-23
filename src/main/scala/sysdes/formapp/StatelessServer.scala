package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  import java.net.URLDecoder

  override def handle(request: Request): Response = request match {
      case Request("GET", "/", _, _, _) => index()
      case Request("GET", path, _, _, _) if path.startsWith("/?") => index()
      case Request("POST", "/name", _, _, Some(body)) => name(body)
      case Request("POST", "/name", _, _,  None) => name("")
      case Request("POST", "/sex", _, _, Some(body)) => sex(body)
      case Request("POST", "/message", _, _, Some(body)) => message(body)
      case Request("POST", "/confirm", _, _, Some(body)) => confirm(body)
      case Request("POST", "/submit", _, _, Some(_)) => submit()
      case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
    }

  // a=hoge&b=huga の形式のStringからMap(a->hoge, b->huga)の形式のMapを生成する
  def generateParamMap(body :String): Map[String, String]={
    var paramMap: Map[String, String] = Map()
    if(body=="")
      return paramMap
    val paramList = body.split('&')
    val Pattern = """(.*?)=(.*?)$""".r
    for (paramPair <- paramList) {
      val patternMatch = Pattern.findAllIn(paramPair)
      val name = htmlEscape(patternMatch.group(1))
      val value = htmlEscape(URLDecoder.decode(patternMatch.group(2), "UTF-8"))
      paramMap += (name -> value)
    }
    paramMap
  }

  // paramMapからhiddenタグを生成する．
  def hiddenTags(paramMap: Map[String, String]): String = {
    var hiddenStr = ""
    for ((name, value) <- paramMap) {
    hiddenStr += s"""<input type="hidden" name="$name" value="$value">\n"""
  }
    hiddenStr
  }

  //  htmlの特殊文字をエスケープして返す
  def htmlEscape(inputStr: String): String = {
    var outputStr = inputStr
    outputStr = outputStr.replace("&", "&amp;")
    outputStr=outputStr.replace("\"", "&quot;")
    outputStr=outputStr.replace("<", "&lt;")
    outputStr=outputStr.replace(">", "&gt;")
    outputStr=outputStr.replace("'", "&#39;")
    outputStr
  }

  //以下はレスポンス

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

  def name(body: String): Response = {
    val paramMap: Map[String, String] = generateParamMap(body)
    val name = paramMap.getOrElse("name","")
    val hiddenStr =hiddenTags(paramMap-  "name")
      Ok(
        s"""<html>
           |<body>
           |<form id="name-form" method="post">
           |    <label>名前: </label>
           |    <input name="name" type="text" value="$name"><br>
           |        $hiddenStr
           |    <button type="submit" formaction="/sex">next</button>
           | </form>
           |</body>
           |</html>""".stripMargin)
  }

  def sex(body: String): Response = {
    val paramMap: Map[String, String] = generateParamMap(body)
    val sex = paramMap.getOrElse("sex","")
    val hiddenStr =hiddenTags(paramMap-  "sex")
    Ok(
      s"""<html>
         |<body>
         |<form id="sex-form" action="" method="post">
         |     <label for="sex">性別: </label>
         |     <input type="radio" name="sex" value="male" ${if(sex == "male")"""checked="checked"""" else ""}>男性
         |     <input type="radio" name="sex" value="female" ${if(sex == "female")"""checked="checked"""" else ""}>女性
         |     <br>
         |    $hiddenStr
         |    <button type="submit" formaction="/message">next</button>
         |    <button type="submit" formaction="/name">back</button>
         |</body>
         |</html>""".stripMargin)
  }

  def message(body: String): Response = {
    val paramMap: Map[String, String] = generateParamMap(body)
    println(paramMap)
    val impressions = paramMap.getOrElse("impressions","")
    val hiddenStr =hiddenTags(paramMap-  "impressions")
    Ok(
      s"""<html>
         |<body>
         |<form id="impressions-form" action="/confirm" method="post">
         |     <label>メッセージ: </label><br>
         |    <textarea name="impressions" form="impressions-form">$impressions</textarea><br>
         |    $hiddenStr
         |    <button type="submit" formaction="/confirm">next</button>
         |    <button type="submit" formaction="/sex">back</button>
         |</form>
         |</body>
         |</html>""".stripMargin)
  }

  def confirm(body: String): Response = {

    val paramMap: Map[String, String] = generateParamMap(body)
    val hiddenStr =hiddenTags(paramMap)
    val name = paramMap.getOrElse("name","")
    val sex = paramMap.getOrElse("sex","")
    val impressions = paramMap.getOrElse("impressions","")
    Ok(
      s"""<html>
         |<body>
         |    <label>名前: </label><span id="submitted-name">$name</span><br>
         |    <label>性別: </label><span id="submitted-sex">$sex</span><br>
         |    <label>メッセージ</label><br>
         |    <textarea name="submitted-impressions" disabled="">$impressions</textarea>
         |    <form action="" method="post">
         |        $hiddenStr
         |    <button type="submit" formaction="/submit">submit</button>
         |    <button type="submit" formaction="/message">back</button>
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
