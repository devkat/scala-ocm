package net.devkat.scalaocm
import javax.jcr.Repository
import javax.jcr.SimpleCredentials
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.DynamicVariable
import javax.jcr.Session

object Ocm extends Logging {

  val currentSession = new DynamicVariable[Session](null)

  def jcrSession = currentSession.value match {
    case null => throw new RuntimeException("Not logged in to repository.")
    case s => s
  }

  def namespaceRegistry = jcrSession.getWorkspace.getNamespaceRegistry
  val scalaOcmNamespace = Namespace("ljr", "http://liftweb.net/record/jcr")

  def withRepo[T](repo: Repository)(f: => T): T = {
    val session = repo.login(new SimpleCredentials("admin", "admin".toCharArray))
    currentSession.withValue(session) {
      
      if (!namespaceRegistry.getURIs.contains(scalaOcmNamespace.uri))
        namespaceRegistry.registerNamespace(scalaOcmNamespace.prefix, scalaOcmNamespace.uri)

      try {
        val user = session.getUserID
        val name = repo.getDescriptor(Repository.REP_NAME_DESC)
        logger.info("Logged in as %s to a %s repository.".format(user, name))
        val ret = f
        session.save()
        ret
      } finally {
        session.logout()
      }
    }
  }

}