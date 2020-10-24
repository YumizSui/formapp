package sysdes.formapp

import java.net.Socket

import sysdes.formapp.Form.State
import sysdes.formapp.Form.StatelessResponse._
import sysdes.formapp.Form.Utils.{generateParamMap, setStateByParamMap}
import sysdes.formapp.server.{Handler, Server}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Request, Response}

  override def handle(request: Request): Response = {
    var state = State("","","")
    request.body match {
      case Some(body) =>
        val paramMap: Map[String, String] = generateParamMap(body)
        state = setStateByParamMap(state, paramMap)
      case None =>
    }

    request match {
      case Request("GET", "/", _, _, _) => index()
      case Request("GET", path, _, _, _) if path.startsWith("/?") => index()
      case Request("GET", path, _, _, _) if path.startsWith("/name?") => name(state)
      case Request("POST", "/name", _, _,  _) => name(state)
      case Request("POST", "/sex", _, _, _) => sex(state)
      case Request("POST", "/message", _, _, _) => message(state)
      case Request("POST", "/confirm", _, _, _) => confirm(state)
      case Request("POST", "/submit", _, _, _) => submit()
      case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
    }
  }
}
