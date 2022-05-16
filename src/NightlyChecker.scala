//> using lib "io.get-coursier:coursier_2.13:2.1.0-M5-24-g678b31710"
//> using lib "com.lihaoyi::os-lib:0.8.0"
//> using lib "com.github.alexarchambault:case-app_2.13:2.1.0-M13"
//> using scala "2.13"

import coursier.Versions
import coursier.cache.FileCache
import coursier.core.{Version, Versions => CoreVersions}
import coursier.util.{Artifact, Task}
import coursier.util.StringInterpolators.safeModule
import coursier.core.Module

import scala.concurrent.duration.DurationInt
import scala.language.experimental.macros
import coursier.core.Organization
import coursier.core.ModuleName
import caseapp._

case class Options(
    nightlyCheckerCode: String
)
object Options {
  implicit lazy val parser: Parser[Options] = Parser.derive
}

object NightlyChecker extends CaseApp[Options] {

  def run(options: Options, arg: RemainingArgs): Unit = {
    val cache: FileCache[Task] = FileCache()
    val scala3Library = Module.apply(
      Organization("org.scala-lang"),
      ModuleName("scala3-library_3"),
      Map.empty
    )

    val scala3Versions = cache
      .withTtl(0.seconds)
      .logger
      .use {
        Versions(cache)
          .withModule(scala3Library)
          .result()
          .unsafeRun()(cache.ec)
      }
      .versions
      .available
      .sorted

    val code = options.nightlyCheckerCode
    val scala3stable = scala3Versions
      .filterNot(_.endsWith("-NIGHTLY"))
      .filterNot(_.contains("RC"))
      .reverse

    val latestWorkingStable =
      searchLatestStableWorkingVersion(code, scala3stable)
        .getOrElse {
          println("Not found stable scala version working with input code")
          sys.exit(0)
        }

    val scala3nigthlies = scala3Versions
      .filter(_.endsWith("-NIGHTLY"))
      .dropWhile(v => !v.startsWith(latestWorkingStable))
      .dropWhile(_.startsWith(latestWorkingStable))

    val latestWorkingNightly =
      searchNigthlyStableWorkingVersion(code, scala3nigthlies)
        .getOrElse {
          println("Not found nightly scala version working with input code")
          sys.exit(0)
        }

    println(
      s"Found the first nightly version not working with the snippet code: $latestWorkingNightly"
    )
  }

  private def searchNigthlyStableWorkingVersion(
      code: String,
      scala3Versions: List[String]
  ): Option[String] = {
    scala3Versions.collectFirst {
      case stable if !isWorkedScalaVersion(stable, code) => stable
    }
  }

  private def searchLatestStableWorkingVersion(
      code: String,
      scala3Versions: List[String]
  ): Option[String] = {
    scala3Versions.collectFirst {
      case stable if isWorkedScalaVersion(stable, code) => stable
    }
  }

  private def isWorkedScalaVersion(version: String, code: String): Boolean = {
    println(s"Test $version")
    val res = os
      .proc("scala-cli", "-S", version, "-q", "-")
      .call(
        cwd = os.pwd,
        stdin = code,
        check = false,
        mergeErrIntoOut = true
      )
    val output = res.out.text().trim
    res.exitCode == 0
  }
}
