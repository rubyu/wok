
package wok

import java.io.{ByteArrayInputStream, IOException, ByteArrayOutputStream, Closeable}
import java.nio.charset.{StandardCharsets, Charset}

object Helpers {
  trait CloseCheck extends Closeable {
    def closed = _closed
    private var _closed = false
    override def close() = _closed = true
  }

  class TestOutputStream extends ByteArrayOutputStream with CloseCheck {
    override def write(b: Int) = {
      if (closed) throw new IOException()
      super.write(b)
    }
  }

  class TestInputStream(bytes: Array[Byte]) extends ByteArrayInputStream(bytes) with CloseCheck {

    def this(s: String) = this(s.getBytes(StandardCharsets.UTF_8))
    def this(s: String, c: Charset) = this(s.getBytes(c))
    def this(s: String, c: String) = this(s.getBytes(c))

    override def read() = {
      if (closed) throw new IOException()
      super.read()
    }
  }

  class AttemptToExitException(val status: Int) extends RuntimeException

  class MockExitSecurityManager extends java.rmi.RMISecurityManager {
    override def checkExit(status: Int) { throw new AttemptToExitException(status) }
    override def checkPermission(perm: java.security.Permission) {}
  }
}
