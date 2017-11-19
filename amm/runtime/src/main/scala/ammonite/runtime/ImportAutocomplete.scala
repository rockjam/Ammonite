package ammonite.runtime

import upickle.json._
import scalaj.http._

object ImportAutocomplete {
  final case class SearchResult(repository: String, organization: String)
  final case class ProjectResult(groupId: String, artifactId: String, version: String)

  def complete(query: String): Seq[String] = {
    val searchResult = extractSearchResult(autocomplete(query))
    val projectResults = extractProjectResult(project(searchResult.organization, searchResult.repository))
    projectResults.map(toImportString)
  }

  private val searchApiUri = "https://index.scala-lang.org/api"

  private def search(query: String, scalaVersion: String = "2.12"): String =
    Http(s"${searchApiUri}/search?q=${query}&target=JVM&scalaVersion=${scalaVersion}").asString.body

  private def project(organization: String, repository: String): String =
    Http(s"${searchApiUri}/project?organization=${organization}&repository=${repository}").asString.body

  private def autocomplete(query: String) =
    Http(s"${searchApiUri}/autocomplete?q=${query}").asString.body

  private def extractSearchResult(searchResults: String) = {
    val firstResult = read(searchResults).arr.head.obj
    SearchResult(firstResult("repository").str, firstResult("organization").str)
  }

  private def extractProjectResult(projectResults: String) = {
    val result = read(projectResults).obj
    result("artifacts").arr.map { artifact =>
      ProjectResult(
        result("groupId").str,
        artifact.str,
        result("version").str
      )
    }
  }

  private def toImportString(project: ProjectResult) = s"$$ivy.`${project.groupId}::${project.artifactId}:${project.version}`"

}
