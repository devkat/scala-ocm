package net.devkat.ocm

import com.typesafe.scalalogging.slf4j.Logging

trait Ocm extends SessionHolder with Mapper with Crud with Query
