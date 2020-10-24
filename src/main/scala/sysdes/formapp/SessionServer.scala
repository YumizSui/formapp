package sysdes.formapp

import java.net.Socket
import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}

import sysdes.formapp.Form.SessionResponse._
import sysdes.formapp.Form.State
import sysdes.formapp.Form.Utils.{generateParamMap, isAuthenticate, pathToQueryMap}
import sysdes.formapp.server.{Handler, Server, Unauthorized}

import scala.collection.mutable
object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
  val states: mutable.HashMap[UUID, State]= mutable.HashMap[UUID, State]()
  //指定されたセッションIDに対して状態を追加
  private def addState(sessionID: UUID): Unit = {
    states(sessionID) = State("", "", "")
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

  //getSessionIDはOption[UUID]を返す．sessionIDがSessionServerHandler.statesに登録されていればセッションIDの値をSome(UUID)で，されていなければNoneを返す
  def getSessionID(cookieStr: String): Option[UUID] = {
    val CookieList = cookieStr.split('=')
    val sessionID = UUID.fromString(CookieList(1))
    Some(sessionID).filter(SessionServerHandler.states.contains)
  }

}


class SessionServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Request, Response}

  def handle(request: Request): Response = {

    // Cookieが指定されていたらセッションIDを取り出す．指定されていなければ新しく生成する．
    val sessionID = request.headers.get("Cookie") match {
      case Some(cookieStr) => SessionServerHandler.getSessionID(cookieStr)
      case None => None
    }
    sessionID match {
      case Some(sessionID) =>
        // pathに付与されたクエリパラメータからparamMapを生成する．
        val paramMap = pathToQueryMap(request.path)
        // paramMapからサーバに保存された状態を更新
        SessionServerHandler.updateState(sessionID, paramMap)
        //現在の状態を関数に渡してレスポンスを生成し，Set-Cookieする．
        val nowState: State = SessionServerHandler.states.getOrElse(sessionID, State("", "", ""))
        val response = request match {
          case Request("GET", "/", _, _, _) => index()
          case Request("GET", "/?", _, _, _) => index()
          case Request("GET", path, _, _, _) if path.startsWith("/name?") => name(nowState)
          case Request("GET", path, _, _, _) if path.startsWith("/sex?") => sex(nowState)
          case Request("GET", path, _, _, _) if path.startsWith("/message?") => message(nowState)
          case Request("GET", path, _, _, _) if path.startsWith("/confirm?") => confirm(nowState)
          case Request("POST", "/submit", _, _, _) => submit()
          case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
        }
        response.addHeader("Set-Cookie", s"""session-id=$sessionID""")
        response
      case None =>
        request.headers.get("Authorization") match {
          case Some(auth) =>
            //Basic認証している場合
            val basicStr = auth.split(' ')(1)
            println(basicStr)
            val paramBytes = Base64.getDecoder.decode(basicStr)
            val paramStr = new String(paramBytes, StandardCharsets.UTF_8)
            val username = paramStr.split(':')(0)
            val password = paramStr.split(':')(1)
            if (isAuthenticate(username, password)) {
              // 認証に成功　クッキーをセットして返す
              val sessionID = SessionServerHandler.generateSession()
              val response = index()
              response.addHeader("Set-Cookie", s"ｖ""session-id=$sessionID""")
              response
            } else {
              // 認証に失敗
              val response = Unauthorized("Unauthorized")
              response.addHeader("WWW-Authenticate", s"""Basic realm="base 64" """) // 再認証を要求
              response
            }
          case None =>
            //Basic認証していないのでするよう要求
            val response = Unauthorized("Unauthorized")
            response.addHeader("WWW-Authenticate", s"""Basic realm="base 64"""")
            response
        }
    }
  }
}
