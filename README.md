scheroku
========

Non-blocking calls to the Heroku REST API from Scala. To use, `import com.jamesward.scheroku` and use the `HerokuClient` object methods, as well as methods from `HerokuAPI` and `HerokuApp`. The unit tests show examples of how to work with the API.

## Testing ##
By default, the credentials stored in `~/.netrc` are used for authentication with Heroku.
If this file is not present, environment variables `HEROKU_USERNAME` and `HEROKU_PASSWORD` are used.

Each test creates a new Heroku app. App names start with `tmp`. Sometimes apps don't get cleaned up, so `src/main/scala/com/jamesward/scheroku/DeleteTestApps.scala` can be run to delete them. `DeleteTestApps.scala` also acts as an example of how to use `HerokuClient`.

## Deployment ##
When the `sbt-heroku` plugin is available for Scala 2.11, or if your project uses Scala 2.10, then
use the `bin/deploy` script in favor of the slower `git push heroku` mechanism.
In order to do that, uncomment this line in `build.sbt`:
Set `herokuAppName` in `build.sbt`
