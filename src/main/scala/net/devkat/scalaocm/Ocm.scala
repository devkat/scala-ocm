package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging

trait Ocm extends SessionHolder with Mapper with Crud with Query
