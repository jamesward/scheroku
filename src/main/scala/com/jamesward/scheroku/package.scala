package com.jamesward.scheroku

case class DynoInfo(
  attach_url: String,
  command: String,
  created_at: String,
  id: String,
  name: String,
  release: Map[String, String],
  size: String,
  state: String,
  `type`: String,
  updated_at: String
)

/** Provide strong typing for API parameters */
case class HerokuApiKey(apiKey: String) extends AnyVal {
  override def toString = apiKey
}

/** Provide strong typing for API parameters */
case class HerokuAppName(appName: String) extends AnyVal {
  override def toString = appName
}
