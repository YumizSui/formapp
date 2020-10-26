package sysdes.formapp

import java.net.Socket
import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}

import sysdes.formapp.Form.StatelessResponse._
import sysdes.formapp.Form.State
import sysdes.formapp.Form.Utils._
import sysdes.formapp.Form.SessionManager._
import sysdes.formapp.server.{Handler, Server, Unauthorized}

import scala.collection.mutable
object HybridServer extends Server(8003) {
  override def getHandler(socket: Socket) = new HybridServerHandler(socket)
}

object HybridServerHandler {
  val sessionFile = "src/main/session_hybrid.txt"
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
  val states: mutable.HashMap[UUID, State]= getSession(sessionFile)
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
  def updateState(sessionID: UUID, state: State): Unit ={
    println("out!")
    states.get(sessionID) match {
      case Some(_) =>
        states(sessionID).name= state.name
        states(sessionID).sex = state.sex
        states(sessionID).impressions = state.impressions
    }
    writeSession(states, sessionFile)
  }

  //getSessionIDはOption[UUID]を返す．sessionIDがHybridServerHandler.statesに登録されていればセッションIDの値をSome(UUID)で，されていなければNoneを返す
  def getSessionID(cookieStr: String): Option[UUID] = {
    val CookieList = cookieStr.split('=')
    val sessionID = UUID.fromString(CookieList(1))
    Some(sessionID).filter(HybridServerHandler.states.contains)
  }
}


class HybridServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Request, Response}

  def handle(request: Request): Response = {

    // Cookieが指定されていたらセッションIDを取り出す．指定されていなければ新しく生成する．
    val sessionID = request.headers.get("Cookie") match {
      case Some(cookieStr) => HybridServerHandler.getSessionID(cookieStr)
      case None => None
    }
    sessionID match {
      case Some(sessionID) =>
        // pathに付与されたクエリパラメータからparamMapを生成する．
        val state = bodyToState(request.body)
        //現在の状態を関数に渡してレスポンスを生成し，Set-Cookieする．
        val response = request match {
          case Request("GET", "/", _, _, _) => index()
          case Request("GET", path, _, _, _) if path.startsWith("/?") => index()
          case Request("GET", path, _, _, _) if path.startsWith("/name?") => name(state)
          case Request("POST", "/name", _, _,  _) => name(state)
          case Request("POST", "/sex", _, _, _) => sex(state)
          case Request("POST", "/message", _, _, _) => message(state)
          case Request("POST", "/confirm", _, _, _) => confirm(state)
          case Request("POST", "/submit", _, _, _) =>
            // submit時のみparamMapからサーバに保存された状態を更新
            HybridServerHandler.updateState(sessionID, state)
            submit()
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
              val sessionID = HybridServerHandler.generateSession()
              val response = index()
              response.addHeader("Set-Cookie", s"""session-id=$sessionID""")
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
