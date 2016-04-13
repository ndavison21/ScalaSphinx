package sphinx.test.dissertation

import java.io.PrintWriter
import sphinx.params.Params
import java.io.FileWriter

object Framework {
  def partialCleanup {
    Params.pseudonymServer.db.clear()
    System.gc
  }
  
  def cleanup {
    Params.pki.clear()
    Params.clients.clear()
    Params.pseudonymServer.db.clear()
    assert(Params.pki.size == 0)
    assert(Params.clients.size == 0)
    assert(Params.pseudonymServer.db.size == 0)
    
    System.gc()
  }

  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def writeToFile(fileName: String, data: String) =
    using(new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  def appendToFile(fileName: String, textData: String) =
    using(new FileWriter(fileName, true)) {
      fileWriter =>
        using(new PrintWriter(fileWriter)) {
          printWriter => printWriter.println(textData)
        }
    }
}