package se.brfparlan.model.knowledge

import scala.language.dynamics

class Taxonomy(topics: Seq[Topic]):
  private val topicMap: Map[Topic.Id, Topic] = topics.map(t => t.id -> t).toMap
  private val childrenMap: Map[Topic.Id, Seq[Topic]] = topics.iterator
    .flatMap{ t => t.parents.map(p => p -> t) }
    .toSeq
    .groupMap(_._1)(_._2)

  val top: TaxonomyTopic =
    val candidates = topics.filter(_.parents.isEmpty)
    require(candidates.size == 1, "There must be exactly one top-level topic")
    TaxonomyTopic(candidates.head)

  def childrenOf(topicId: Topic.Id): Seq[Topic] = childrenMap.getOrElse(topicId, Nil)
  export topicMap.get as lookup

  /** A Topic that can navigate to its children by id via dynamic field selection.
    * Ids that clash with real members of Topic (e.g. `name`, `id`, `description`)
    * resolve to those members instead.
    */
  final class TaxonomyTopic private[Taxonomy](topic: Topic) extends Topic, Dynamic:
    export topic.*

    def childLookup(id: String): Option[TaxonomyTopic] = Topic.parseId(id)
      .flatMap: tid =>
        childrenOf(topic.id).find(_.id == tid)
      .map(TaxonomyTopic(_))

    def selectDynamic(id: String): TaxonomyTopic = childLookup(id)
      .getOrElse(throw java.util.NoSuchElementException(s"Topic '${this.id}' has no child with id '$id'"))

  end TaxonomyTopic

end Taxonomy

object Taxonomy:
  def fromFilePath(path: String): Taxonomy =
    Taxonomy(Topic.parseFile(java.nio.file.Paths.get(path)))
