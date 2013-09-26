package net.devkat.scalaocm

import org.specs2.specification.AroundExample
import org.specs2.mutable.Specification
import org.specs2.execute.Result
import org.specs2.execute.AsResult
import org.apache.jackrabbit.core.TransientRepository
import Path._
import Extensions.toIterator
import com.typesafe.scalalogging.slf4j.Logging

abstract class ScalaOcmSpec extends Specification with AroundExample with Logging {

  import Extensions._
  import JcrHelpers._
  import DirectTypeOcm._

  sequential

  protected def around[T: AsResult](t: => T): Result = {
    logger.info("Creating repo")
    val repo = new TransientRepository
    try {
      logger.info("Starting with repo")
      withRepo(repo) {
        AsResult(t)
      }
    } finally {
      /*
      logger.info("Shutting down repo")
      repo.shutdown()
      logger.info("Repo shut down")
      */
    }
  }

}