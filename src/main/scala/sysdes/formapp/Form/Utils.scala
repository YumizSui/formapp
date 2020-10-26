package sysdes.formapp.Form

import java.net.URLDecoder

object Utils {

  //  htmlの特殊文字をエスケープして返す
  def htmlEscape(inputStr: String): String = {
    var outputStr = inputStr
    outputStr = outputStr.replace("&", "&amp;")
    outputStr = outputStr.replace("\"", "&quot;")
    outputStr = outputStr.replace("<", "&lt;")
    outputStr = outputStr.replace(">", "&gt;")
    outputStr = outputStr.replace("'", "&#39;")
    outputStr
  }

  // a=hoge&b=huga の形式のStringからMap(a->hoge, b->huga)の形式のMapを生成する
  def generateParamMap(body: String): Map[String, String] = {
    var paramMap = Map[String, String]()
    if (body == "")
      return paramMap
    val paramList = body.split('&')
    val Pattern = """(.*?)=(.*?)$""".r
    for (paramPair <- paramList) {
      val patternMatch = Pattern.findAllIn(paramPair)
      val name = patternMatch.group(1)
      val value = htmlEscape(URLDecoder.decode(patternMatch.group(2), "UTF-8"))
      paramMap += (name -> value)
    }
    paramMap
  }

  //pathからクエリパラメータを取り出してMap形式で返す．
  def pathToQueryMap(path: String): Map[String, String] = {
    val pathSplit = path.split('?')
    var paramMap = Map[String, String]()
    if (pathSplit.length > 1) {
      val queryStr = pathSplit(1)
      paramMap = generateParamMap(queryStr)
      println(paramMap)
    }
    paramMap
  }

  // ユーザ，パスワードの認証
  def isAuthenticate(user: String, password: String): Boolean = {
    user=="user" && password == "password"
  }

  // paramMapからStateをセットする．
  def setStateByParamMap(state: State, paramMap: Map[String, String]): State = {
    for ((name, value) <- paramMap) {
      name match {
        case "name" => state.name = value
        case "sex" => state.sex = value
        case "impressions" => state.impressions = value
      }
    }
    state
  }
  def bodyToState(body: Option[String]): State = {
    var state = State("","","")
    body match {
      case Some(body) =>
        val paramMap: Map[String, String] = generateParamMap(body)
        state = setStateByParamMap(state, paramMap)
      case None =>
    }
    state
  }
}
