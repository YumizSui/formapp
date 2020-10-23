package sysdes.formapp

import java.net.{Socket, URLDecoder}

import sysdes.formapp.server.{Handler, Server}
import java.util.UUID
import scala.collection.mutable
object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
  val states: mutable.HashMap[UUID, State]= mutable.HashMap[UUID, State]()
  //指定されたセッションIDに対して状態を追加
  private def addState(sessionID: UUID): Unit = {
    states(sessionID) = new State("", "", "")
  }
  //新しいセッションを追加
  def generateSession(): UUID = {
    val newSessionID = UUID.randomUUID()
    addState(newSessionID)
    newSessionID
  }

  //セッション情報をparamMapで更新
  def updateState(sessionID: UUID, paramMap: Map[String, String]): Unit ={
    SessionServerHandler.states.get(sessionID) match {
      case Some(_) =>
        for ((name, value) <- paramMap) {
          name match {
            case "name"=> SessionServerHandler.states(sessionID).name= value
            case "sex"=>SessionServerHandler.states(sessionID).sex = value
            case "impressions"=>SessionServerHandler.states(sessionID).impressions = value
          }
        }
    }
  }

}

//状態を管理するクラス
class State(var name: String, var sex: String, var impressions: String)

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{NotFound, Ok, BadRequest, Request, Response}

  def handle(request: Request): Response = {
    // Cookieが指定されていたらセッションIDを取り出す．指定されていなければ新しく生成する．
    val sessionID  = request.headers.get("Cookie") match {
      case Some(cookieStr) => getSessionID(cookieStr)
      case None => Some(SessionServerHandler.generateSession())
    }
    sessionID match {
      case Some(sessionID)=>
        // pathに付与されたクエリパラメータからparamMapを生成する．
        val paramMap  = pathToQueryMap(request.path)
        // paramMapからサーバに保存された状態を更新
        SessionServerHandler.updateState(sessionID, paramMap)
        //現在の状態を関数に渡してレスポンスを生成し，Set-Cookieする．
        val nowState: State = SessionServerHandler.states.getOrElse(sessionID, new State("","",""))
        val response = request match {
          case Request("GET", "/", _, _, _) => index()
          case Request("GET", "/?", _, _, _)=> index()
          case Request("GET", path, _, _, _) if path.startsWith("/name?") => name(nowState)
          case Request("GET", path, _, _, _) if path.startsWith("/sex?") => sex(nowState)
          case Request("GET", path, _, _, _) if path.startsWith("/message?") => message(nowState)
          case Request("GET", path, _, _, _) if path.startsWith("/confirm?") => confirm(nowState)
          case Request("POST", "/submit", _, _, _) => submit()
          case _                            => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
        }
        response.addHeader("Set-Cookie",s"""session-id=$sessionID""")
        response
      case  None =>
        //無効なセッションIDであればBadRequestを返して新しいセッションIDでSet-Cookieし直す
        val newSessionID = SessionServerHandler.generateSession()
        val response = BadRequest("""sessionID Not Found""")
        response.addHeader("Set-Cookie",s"""session-id=$newSessionID""")
        response
    }
  }
  //pathからクエリパラメータを取り出してMap形式で返す．
def pathToQueryMap(path: String): Map[String, String] = {
  val pathSplit = path.split('?')
  var paramMap = Map[String, String]()
  if( pathSplit.length>1 ) {
    val queryStr = pathSplit(1)
    paramMap = generateParamMap(queryStr)
    println(paramMap)
  }
  paramMap
}

  //getSessionIDはOption[UUID]を返す．sessionIDがSessionServerHandler.statesに登録されていればセッションIDの値をSome(UUID)で，されていなければNoneを返す
  def getSessionID(cookieStr: String): Option[UUID] = {
    val CookieList = cookieStr.split('=')
    val sessionID = UUID.fromString(CookieList(1))
    Some(sessionID).filter(SessionServerHandler.states.contains)
  }

  // a=hoge&b=huga の形式のStringからMap(a->hoge, b->huga)の形式のMapを生成する
    def generateParamMap(body :String): Map[String, String]={
    var paramMap = Map[String, String]()
    if(body=="")
      return paramMap
    val paramList = body.split('&')
    val Pattern = """(.*?)=(.*?)$""".r
    for (paramPair <- paramList) {
      val patternMatch = Pattern.findAllIn(paramPair)
      val name = patternMatch.group(1)
      val value = htmlEscape(URLDecoder.decode(patternMatch.group(2), "UTF-8"))
      paramMap += (name->value)
    }
    paramMap
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
         |    <form action="/name" method="get">
         |        <input type="submit" value="start" />
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  def name(state: State): Response = {

    val name = state.name
    Ok(
      s"""<html>
         |<body>
         |<form id="name-form"  method="get">
         |    <label>名前: </label>
         |    <input name="name" type="text" value="$name"><br>
         |    <button type="submit" formaction="/sex">next</button>
         | </form>
         |</body>
         |</html>""".stripMargin)
  }

  def sex(state: State): Response = {
    val sex = state.sex
    Ok(
      s"""<html>
         |<body>
         |<form id="sex-form" method="get">
         |     <label for="sex">性別: </label>
         |     <input type="radio" name="sex" value="male" ${if(sex == "male")"""checked="checked"""" else ""}>男性
         |     <input type="radio" name="sex" value="female" ${if(sex == "female")"""checked="checked"""" else ""}>女性
         |     <br>
         |    <button type="submit" formaction="/message">next</button>
         |    <button type="submit" formaction="/name">back</button>
         | </form>
         |</body>
         |</html>""".stripMargin)
  }

  def message(state: State): Response = {
    val impressions = state.impressions
    Ok(
      s"""<html>
         |<body>
         |<form id="impressions-form" method="get">
         |     <label>メッセージ: </label><br>
         |    <textarea name="impressions" form="impressions-form">$impressions</textarea><br>
         |    <button type="submit" formaction="/confirm">next</button>
         |    <button type="submit" formaction="/sex">back</button>
         |</form>
         |</body>
         |</html>""".stripMargin)
  }

  def confirm(state: State): Response = {
    val name = state.name
    val sex = state.sex
    val impressions = state.impressions
    Ok(
      s"""<html>
         |<body>
         |    <label>名前: </label><span id="submitted-name">$name</span><br>
         |    <label>性別: </label><span id="submitted-sex">$sex</span><br>
         |    <label>メッセージ</label><br>
         |    <textarea name="submitted-impressions" disabled="">$impressions</textarea>
         |    <form action="/submit">
         |    <button type="submit" formaction="/submit" formmethod="post">next</button>
         |    <button type="submit" formaction="/sex" formmethod="get">back</button>
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
