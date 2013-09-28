package net.devkat.scalaocm

import org.specs2.specification.AroundExample
import org.specs2.mutable.Specification
import org.specs2.execute.Result
import org.specs2.execute.AsResult
import Path._
import Extensions.toIterator
import com.typesafe.scalalogging.slf4j.Logging
import org.specs2.specification.Fragments
import org.specs2.specification.Step
import org.apache.jackrabbit.core.TransientRepository
import javax.jcr.SimpleCredentials

object RepoMgr {

  lazy val repository = {
    val repo = new TransientRepository
    val keepaliveSession = repo.login(new SimpleCredentials("admin", "admin".toCharArray))
    repo
  }

}

trait ScalaOcmSpec extends Specification with AroundExample with Logging {

  import Extensions._
  import JcrHelpers._
  import DirectTypeOcm._

  sequential

  protected def around[T: AsResult](t: => T): Result = {
    withRepo(RepoMgr.repository) {
      AsResult(t)
    }
  }

}