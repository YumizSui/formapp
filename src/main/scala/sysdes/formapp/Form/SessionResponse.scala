package sysdes.formapp.Form

import sysdes.formapp.server.{Ok, Response, Unauthorized}

object SessionResponse {
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
         |     <input type="radio" name="sex" value="male" ${if (sex == "male") """checked="checked"""" else ""}>男性
         |     <input type="radio" name="sex" value="female" ${if (sex == "female") """checked="checked"""" else ""}>女性
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

  def authenticate(): Response = {
    Unauthorized(
      s"""<html>
         |<body>
         |<form id="name-form"  method="post">
         |    <label>username: </label>
         |    <input name="username" type="text"><br>
         |    <label>password: </label>
         |    <input name="password" type="password"><br>
         |    <button type="submit" formaction="/login">next</button>
         | </form>
         |</body>
         |</html>""".stripMargin)
  }
}
