package sysdes.formapp.Form
import scala.io.Source
import java.io.{PrintWriter, File}
import java.util.UUID

import scala.collection.mutable

object SessionManager {

  // Sessionを読み込む
  def getSession(sessionFile: String): mutable.HashMap[UUID, State] = {
    var states = mutable.HashMap[UUID, State]()
    if (new File(sessionFile).exists) {
      val source = Source.fromFile(sessionFile, "utf8")
      val lines = source.getLines()
      for (line <- lines) {
        states += decodeFromCSV(line)
      }
    }
    states
  }

  // 新規書き込み
  def writeSession(states: mutable.HashMap[UUID, State], sessionFile: String): Unit = {
    val file = new File(sessionFile)
    if (!file.exists) {
      file.createNewFile
    }
    val out = new PrintWriter(sessionFile, "utf8")
    states.foreach{
      case (sessionID, state) =>
        out.write(encodeToCSV(sessionID, state))
    }
    out.close()
  }

  private def encodeToCSV(sessionID: UUID, state: State): String = {
    s"""$sessionID,"${state.name}","${state.sex}","${state.impressions}"\n"""
  }

  private def decodeFromCSV(line: String): (UUID, State) = {
    val list = line.split(',')
    val sessionID = UUID.fromString(list(0))
    val name = list(1).stripPrefix("\"").stripSuffix("\"")
    val sex = list(2).stripPrefix("\"").stripSuffix("\"")
    val impressions = list(3).stripPrefix("\"").stripSuffix("\"")
    val state = State(name, sex, impressions)
    sessionID -> state
  }

}
