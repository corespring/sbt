package sbt

import java.io.File
import java.net.URL
import org.apache.ivy.core
import core.module.descriptor.ModuleDescriptor
import sbt.serialization._
import java.net.{ URLEncoder, URLDecoder }

private[sbt] object JsonUtil {
  def sbtOrgTemp = "org.scala-sbt.temp"
  def fakeCallerOrganization = "org.scala-sbt.temp-callers"

  def parseUpdateReport(md: ModuleDescriptor, path: File, cachedDescriptor: File, log: Logger): UpdateReport =
    {
      try {
        val lite = fromJsonFile[UpdateReportLite](path).get
        fromLite(lite, cachedDescriptor)
      } catch {
        case e: Throwable =>
          log.error("Unable to parse mini graph: " + path.toString)
          throw e
      }
    }
  def writeUpdateReport(ur: UpdateReport, graphPath: File, log: Logger): Unit =
    {
      IO.createDirectory(graphPath.getParentFile)
      toJsonFile(toLite(ur, log), graphPath)
    }
  private def toLite(ur: UpdateReport, log: Logger): UpdateReportLite =
    UpdateReportLite(ur.configurations map { cr =>
      ConfigurationReportLite(cr.configuration, cr.details map { oar =>
        new OrganizationArtifactReport(oar.organization, oar.name, oar.modules map { mr =>
          new ModuleReport(
            mr.module, mr.artifacts, mr.missingArtifacts, mr.status,
            mr.publicationDate, mr.resolver, mr.artifactResolver,
            mr.evicted, mr.evictedData, mr.evictedReason,
            mr.problem, mr.homepage, mr.extraAttributes,
            mr.isDefault, mr.branch, mr.configurations, mr.licenses,
            if(evicted) filterOutArtificialCallers(mr.callers, log) else Seq.empty)
        })
      })
    })
  // #1763/#2030. Caller takes up 97% of space, so we need to shrink it down,
  // but there are semantics associated with some of them.
  def filterOutArtificialCallers(callers: Seq[Caller], log: Logger): Seq[Caller] = {
    //println(s"!! [filterOutArtificialCallers] ---- || prove that the filtering is the cause of the issue return an empty Seq - caller size: ${callers.size}")
    //Seq.empty
    //}
    //{

    val forceEmptyCallers = java.lang.System.getenv("FORCE_EMPTY_CALLERS") != null
    if (callers.isEmpty || forceEmptyCallers) {
      log.debug(s"forceEmptyCallers: $forceEmptyCallers")
      Seq.empty
    } else {
      log.debug(s"[filterOutArtificialCallers] callers.size: ${callers.size}")

      val nonArtificial = callers.filter { c =>
        (c.caller.organization != sbtOrgTemp) &&
          (c.caller.organization != fakeCallerOrganization)
      }.distinct

      log.debug(s"[filterOutArtificialCallers] nonArtificial.size: ${nonArtificial.size}")

      val interProj = (callers filter { c =>
        (c.caller.organization == sbtOrgTemp)
      }).headOption.toList

      val out = interProj ::: nonArtificial.toList
      //val out = nonArtificial.toList

      /*if(out.lengthCompare(1000) == 1){
        log.debug("!!-------------")
        log.debug(callers)
      }*/

      log.debug(s"[filterOutArtificialCallers] return out.size: ${out.size}")
      out
    }
  }

  def fromLite(lite: UpdateReportLite, cachedDescriptor: File): UpdateReport =
    {
      val stats = new UpdateStats(0L, 0L, 0L, false)
      val configReports = lite.configurations map { cr =>
        val details = cr.details
        val modules = details flatMap {
          _.modules filter { mr =>
            !mr.evicted && mr.problem.isEmpty
          }
        }
        val evicted = details flatMap {
          _.modules filter { mr =>
            mr.evicted
          }
        } map { _.module }
        new ConfigurationReport(cr.configuration, modules, details, evicted)
      }
      new UpdateReport(cachedDescriptor, configReports, stats)
    }
}

private[sbt] case class UpdateReportLite(configurations: Seq[ConfigurationReportLite])
private[sbt] object UpdateReportLite {
  implicit val pickler: Pickler[UpdateReportLite] with Unpickler[UpdateReportLite] = PicklerUnpickler.generate[UpdateReportLite]
}

private[sbt] case class ConfigurationReportLite(configuration: String, details: Seq[OrganizationArtifactReport])
private[sbt] object ConfigurationReportLite {
  implicit val pickler: Pickler[ConfigurationReportLite] with Unpickler[ConfigurationReportLite] = PicklerUnpickler.generate[ConfigurationReportLite]
}
