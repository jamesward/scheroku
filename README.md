scheroku
========

Non-blocking calls to the Heroku REST API from Scala.

## Testing ##
By default, the credentials stored in `~/.netrc` are used for authentication with Heroku.
If this file is not present, environment variables `HEROKU_USERNAME` and `HEROKU_PASSWORD` are used.

## Deployment ##
When the `sbt-heroku` plugin is available for Scala 2.11, or if your project uses Scala 2.10, then
use the `bin/deploy` script in favor of the slower `git push heroku` mechanism.
In order to do that, uncomment this line in `build.sbt`:
Set `herokuAppName` in `build.sbt`
