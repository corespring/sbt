/* sbt -- Simple Build Tool
 * Copyright 2011 Mark Harrah
 */
package sbt

import scala.concurrent.duration.Duration
import java.io.File
import Def.{ displayFull, dummyState, ScopedKey, Setting }
import Keys.{ streams, Streams, TaskStreams }
import Keys.{ dummyRoots, dummyStreamsManager, executionRoots, pluginData, streamsManager, taskDefinitionKey, transformState }
import Project.richInitializeTask
import Scope.{ GlobalScope, ThisScope }
import Types.const
import scala.Console.RED
import std.Transform.{ DummyTaskMap, TaskAndValue }
import TaskName._

@deprecated("Use EvaluateTaskConfig instead.", "0.13.5")
final case class EvaluateConfig(cancelable: Boolean, restrictions: Seq[Tags.Rule], checkCycles: Boolean = false, progress: ExecuteProgress[Task] = EvaluateTask.defaultProgress)

/**
 * An API that allows you to cancel executing tasks upon some signal.
 *
 *  For example, this is implemented by the TaskEngine; invoking `cancel()` allows you
 *  to cancel the current task exeuction.   A `TaskCancel` is passed to the
 *  [[TaskEvalautionCancelHandler]] which is responsible for calling `cancel()` when
 *  appropriate.
 */
trait RunningTaskEngine {
  /** Attempts to kill and shutdown the running task engine.*/
  def cancelAndShutdown(): Unit
}
/**
 * A startegy for being able to cancle tasks.
 *
 * Implementations of this trait determine what will trigger `cancel()` for
 * the task engine, providing in the `start` method.
 *
 * All methods on this API are expected to be called from the same thread.
 */
trait TaskCancellationStrategy {
  /** The state used by this task. */
  type State
  /**
   * Called when task evaluation starts.
   *
   * @param canceller An object that can cancel the current task evaluation session.
   * @return Whatever state you need to cleanup in your finish method.
   */
  def onTaskEngineStart(canceller: RunningTaskEngine): State
  /** Called when task evaluation completes, either in success or failure. */
  def onTaskEngineFinish(state: State): Unit
}
object TaskCancellationStrategy {
  /** An empty handler that does not cancel tasks. */
  object Null extends TaskCancellationStrategy {
    type State = Unit
    def onTaskEngineStart(canceller: RunningTaskEngine): Unit = ()
    def onTaskEngineFinish(state: Unit): Unit = ()
    override def toString: String = "Null"
  }
  /** Cancel handler which registers for SIGINT and cancels tasks when it is received. */
  object Signal extends TaskCancellationStrategy {
    type State = Signals.Registration
    def onTaskEngineStart(canceller: RunningTaskEngine): Signals.Registration = {
      Signals.register(() => canceller.cancelAndShutdown())
    }
    def onTaskEngineFinish(registration: Signals.Registration): Unit =
      registration.remove()
    override def toString: String = "Signal"
  }
}

/**
 * The new API for running tasks.
 *
 * This represents all the hooks possible when running the task engine.
 * We expose this trait so that we can, in a binary compatible way, modify what is used
 * inside this configuration and how to construct it.
 */
sealed trait EvaluateTaskConfig {
  def restrictions: Seq[Tags.Rule]
  def checkCycles: Boolean
  def progressReporter: ExecuteProgress[Task]
  def cancelStrategy: TaskCancellationStrategy
  /**
   * If true, we force a finalizer/gc run (or two) after task execution completes when needed.
   */
  def forceGarbageCollection: Boolean

  /**
   * Interval to force GC.
   */
  def minForcegcInterval: Duration
}
final object EvaluateTaskConfig {
  /** Pulls in the old configuration format. */
  def apply(old: EvaluateConfig): EvaluateTaskConfig = {
    object AdaptedTaskConfig extends EvaluateTaskConfig {
      def restrictions: Seq[Tags.Rule] = old.restrictions
      def checkCycles: Boolean = old.checkCycles
      def progressReporter: ExecuteProgress[Task] = old.progress
      def cancelStrategy: TaskCancellationStrategy =
        if (old.cancelable) TaskCancellationStrategy.Signal
        else TaskCancellationStrategy.Null
      def forceGarbageCollection = GCUtil.defaultForceGarbageCollection
      def minForcegcInterval = GCUtil.defaultMinForcegcInterval
    }
    AdaptedTaskConfig
  }

  @deprecated("Use the alternative that specifies minForcegcInterval", "0.13.9")
  def apply(restrictions: Seq[Tags.Rule],
    checkCycles: Boolean,
    progressReporter: ExecuteProgress[Task],
    cancelStrategy: TaskCancellationStrategy,
    forceGarbageCollection: Boolean): EvaluateTaskConfig =
    apply(restrictions, checkCycles, progressReporter, cancelStrategy, forceGarbageCollection,
      GCUtil.defaultMinForcegcInterval)

  /** Raw constructor for EvaluateTaskConfig. */
  def apply(restrictions: Seq[Tags.Rule],
    checkCycles: Boolean,
    progressReporter: ExecuteProgress[Task],
    cancelStrategy: TaskCancellationStrategy,
    forceGarbageCollection: Boolean,
    minForcegcInterval: Duration): EvaluateTaskConfig = {
    val r = restrictions
    val check = checkCycles
    val cs = cancelStrategy
    val pr = progressReporter
    val fgc = forceGarbageCollection
    val mfi = minForcegcInterval
    object SimpleEvaluateTaskConfig extends EvaluateTaskConfig {
      def restrictions = r
      def checkCycles = check
      def progressReporter = pr
      def cancelStrategy = cs
      def forceGarbageCollection = fgc
      def minForcegcInterval = mfi
    }
    SimpleEvaluateTaskConfig
  }
}

final case class PluginData(dependencyClasspath: Seq[Attributed[File]], definitionClasspath: Seq[Attributed[File]], resolvers: Option[Seq[Resolver]], report: Option[UpdateReport], scalacOptions: Seq[String]) {
  val classpath: Seq[Attributed[File]] = definitionClasspath ++ dependencyClasspath
}
object PluginData {
  @deprecated("Use the alternative that specifies the compiler options and specific classpaths.", "0.13.1")
  def apply(dependencyClasspath: Seq[Attributed[File]], definitionClasspath: Seq[Attributed[File]], resolvers: Option[Seq[Resolver]], report: Option[UpdateReport]): PluginData =
    PluginData(dependencyClasspath, definitionClasspath, resolvers, report, Nil)
  @deprecated("Use the alternative that specifies the specific classpaths.", "0.13.0")
  def apply(classpath: Seq[Attributed[File]], resolvers: Option[Seq[Resolver]], report: Option[UpdateReport]): PluginData =
    PluginData(classpath, Nil, resolvers, report, Nil)
}

object EvaluateTask {
  import std.{ TaskExtra, Transform }
  import TaskExtra._
  import Keys.state

  private[sbt] def defaultProgress: ExecuteProgress[Task] =
    if (java.lang.Boolean.getBoolean("sbt.task.timings")) new TaskTimings else ExecuteProgress.empty[Task]

  val SystemProcessors = Runtime.getRuntime.availableProcessors

  @deprecated("Use extractedTaskConfig.", "0.13.0")
  def defaultConfig(state: State): EvaluateConfig =
    {
      val extracted = Project.extract(state)
      extractedConfig(extracted, extracted.structure, state)
    }

  @deprecated("Use extractedTaskConfig.", "0.13.0")
  def defaultConfig(extracted: Extracted, structure: BuildStructure) =
    EvaluateConfig(false, restrictions(extracted, structure), progress = defaultProgress)

  @deprecated("Use other extractedTaskConfig", "0.13.2")
  def extractedConfig(extracted: Extracted, structure: BuildStructure): EvaluateConfig =
    {
      val workers = restrictions(extracted, structure)
      val canCancel = cancelable(extracted, structure)
      EvaluateConfig(cancelable = canCancel, restrictions = workers, progress = defaultProgress)
    }
  @deprecated("Use other extractedTaskConfig", "0.13.5")
  def extractedConfig(extracted: Extracted, structure: BuildStructure, state: State): EvaluateConfig =
    {
      val workers = restrictions(extracted, structure)
      val canCancel = cancelable(extracted, structure)
      val progress = executeProgress(extracted, structure, state)
      EvaluateConfig(cancelable = canCancel, restrictions = workers, progress = progress)
    }
  def extractedTaskConfig(extracted: Extracted, structure: BuildStructure, state: State): EvaluateTaskConfig =
    {
      val rs = restrictions(extracted, structure)
      val canceller = cancelStrategy(extracted, structure, state)
      val progress = executeProgress(extracted, structure, state)
      val fgc = forcegc(extracted, structure)
      val mfi = minForcegcInterval(extracted, structure)
      EvaluateTaskConfig(rs, false, progress, canceller, fgc, mfi)
    }

  def defaultRestrictions(maxWorkers: Int) = Tags.limitAll(maxWorkers) :: Nil
  def defaultRestrictions(extracted: Extracted, structure: BuildStructure): Seq[Tags.Rule] =
    Tags.limitAll(maxWorkers(extracted, structure)) :: Nil

  def restrictions(state: State): Seq[Tags.Rule] =
    {
      val extracted = Project.extract(state)
      restrictions(extracted, extracted.structure)
    }
  def restrictions(extracted: Extracted, structure: BuildStructure): Seq[Tags.Rule] =
    getSetting(Keys.concurrentRestrictions, defaultRestrictions(extracted, structure), extracted, structure)
  def maxWorkers(extracted: Extracted, structure: BuildStructure): Int =
    if (getSetting(Keys.parallelExecution, true, extracted, structure))
      SystemProcessors
    else
      1
  def cancelable(extracted: Extracted, structure: BuildStructure): Boolean =
    getSetting(Keys.cancelable, false, extracted, structure)
  def cancelStrategy(extracted: Extracted, structure: BuildStructure, state: State): TaskCancellationStrategy =
    getSetting(Keys.taskCancelStrategy, { (_: State) => TaskCancellationStrategy.Null }, extracted, structure)(state)

  private[sbt] def executeProgress(extracted: Extracted, structure: BuildStructure, state: State): ExecuteProgress[Task] = {
    import Types.const
    val maker: State => Keys.TaskProgress = getSetting(Keys.executeProgress, const(new Keys.TaskProgress(defaultProgress)), extracted, structure)
    maker(state).progress
  }
  // TODO - Should this pull from Global or from the project itself?
  private[sbt] def forcegc(extracted: Extracted, structure: BuildStructure): Boolean =
    getSetting(Keys.forcegc in Global, GCUtil.defaultForceGarbageCollection, extracted, structure)
  // TODO - Should this pull from Global or from the project itself?
  private[sbt] def minForcegcInterval(extracted: Extracted, structure: BuildStructure): Duration =
    getSetting(Keys.minForcegcInterval in Global, GCUtil.defaultMinForcegcInterval, extracted, structure)

  def getSetting[T](key: SettingKey[T], default: T, extracted: Extracted, structure: BuildStructure): T =
    key in extracted.currentRef get structure.data getOrElse default

  def injectSettings: Seq[Setting[_]] = Seq(
    (state in GlobalScope) ::= dummyState,
    (streamsManager in GlobalScope) ::= dummyStreamsManager,
    (executionRoots in GlobalScope) ::= dummyRoots
  )

  def evalPluginDef(log: Logger)(pluginDef: BuildStructure, state: State): PluginData =
    {
      val root = ProjectRef(pluginDef.root, Load.getRootProject(pluginDef.units)(pluginDef.root))
      val pluginKey = pluginData
      val config = extractedConfig(Project.extract(state), pluginDef, state)
      val evaluated = apply(pluginDef, ScopedKey(pluginKey.scope, pluginKey.key), state, root, config)
      val (newS, result) = evaluated getOrElse sys.error("Plugin data does not exist for plugin definition at " + pluginDef.root)
      Project.runUnloadHooks(newS) // discard states
      processResult(result, log)
    }

  @deprecated("This method does not apply state changes requested during task execution and does not honor concurrent execution restrictions.  Use 'apply' instead.", "0.11.1")
  def evaluateTask[T](structure: BuildStructure, taskKey: ScopedKey[Task[T]], state: State, ref: ProjectRef, checkCycles: Boolean = false, maxWorkers: Int = SystemProcessors): Option[Result[T]] =
    apply(structure, taskKey, state, ref, EvaluateConfig(false, defaultRestrictions(maxWorkers), checkCycles)).map(_._2)

  /**
   * Evaluates `taskKey` and returns the new State and the result of the task wrapped in Some.
   * If the task is not defined, None is returned.  The provided task key is resolved against the current project `ref`.
   * Task execution is configured according to settings defined in the loaded project.
   */
  def apply[T](structure: BuildStructure, taskKey: ScopedKey[Task[T]], state: State, ref: ProjectRef): Option[(State, Result[T])] =
    apply[T](structure, taskKey, state, ref, extractedTaskConfig(Project.extract(state), structure, state))

  /**
   * Evaluates `taskKey` and returns the new State and the result of the task wrapped in Some.
   * If the task is not defined, None is returned.  The provided task key is resolved against the current project `ref`.
   * `config` configures concurrency and canceling of task execution.
   */
  @deprecated("Use EvaluateTaskConfig option instead.", "0.13.5")
  def apply[T](structure: BuildStructure, taskKey: ScopedKey[Task[T]], state: State, ref: ProjectRef, config: EvaluateConfig): Option[(State, Result[T])] =
    apply(structure, taskKey, state, ref, EvaluateTaskConfig(config))
  def apply[T](structure: BuildStructure, taskKey: ScopedKey[Task[T]], state: State, ref: ProjectRef, config: EvaluateTaskConfig): Option[(State, Result[T])] = {
    withStreams(structure, state) { str =>
      for ((task, toNode) <- getTask(structure, taskKey, state, str, ref)) yield runTask(task, state, str, structure.index.triggers, config)(toNode)
    }
  }
  def logIncResult(result: Result[_], state: State, streams: Streams) = result match { case Inc(i) => logIncomplete(i, state, streams); case _ => () }
  def logIncomplete(result: Incomplete, state: State, streams: Streams) {
    val all = Incomplete linearize result
    val keyed = for (Incomplete(Some(key: ScopedKey[_]), _, msg, _, ex) <- all) yield (key, msg, ex)
    val un = all.filter { i => i.node.isEmpty || i.message.isEmpty }

    import ExceptionCategory._
    for ((key, msg, Some(ex)) <- keyed) {
      def log = getStreams(key, streams).log
      ExceptionCategory(ex) match {
        case AlreadyHandled => ()
        case m: MessageOnly => if (msg.isEmpty) log.error(m.message)
        case f: Full        => log.trace(f.exception)
      }
    }

    for ((key, msg, ex) <- keyed if (msg.isDefined || ex.isDefined)) {
      val msgString = (msg.toList ++ ex.toList.map(ErrorHandling.reducedToString)).mkString("\n\t")
      val log = getStreams(key, streams).log
      val display = contextDisplay(state, log.ansiCodesSupported)
      log.error("(" + display(key) + ") " + msgString)
    }
  }
  private[this] def contextDisplay(state: State, highlight: Boolean) = Project.showContextKey(state, if (highlight) Some(RED) else None)
  def suppressedMessage(key: ScopedKey[_])(implicit display: Show[ScopedKey[_]]): String =
    "Stack trace suppressed.  Run 'last %s' for the full log.".format(display(key))

  def getStreams(key: ScopedKey[_], streams: Streams): TaskStreams =
    streams(ScopedKey(Project.fillTaskAxis(key).scope, Keys.streams.key))
  def withStreams[T](structure: BuildStructure, state: State)(f: Streams => T): T =
    {
      val str = std.Streams.closeable(structure.streams(state))
      try { f(str) } finally { str.close() }
    }

  def getTask[T](structure: BuildStructure, taskKey: ScopedKey[Task[T]], state: State, streams: Streams, ref: ProjectRef): Option[(Task[T], NodeView[Task])] =
    {
      val thisScope = Load.projectScope(ref)
      val resolvedScope = Scope.replaceThis(thisScope)(taskKey.scope)
      for (t <- structure.data.get(resolvedScope, taskKey.key)) yield (t, nodeView(state, streams, taskKey :: Nil))
    }
  def nodeView[HL <: HList](state: State, streams: Streams, roots: Seq[ScopedKey[_]], dummies: DummyTaskMap = DummyTaskMap(Nil)): NodeView[Task] =
    Transform((dummyRoots, roots) :: (dummyStreamsManager, streams) :: (dummyState, state) :: dummies)

  @deprecated("Use new EvaluateTaskConfig option to runTask", "0.13.5")
  def runTask[T](root: Task[T], state: State, streams: Streams, triggers: Triggers[Task], config: EvaluateConfig)(implicit taskToNode: NodeView[Task]): (State, Result[T]) =
    {
      val newConfig = EvaluateTaskConfig(config)
      runTask(root, state, streams, triggers, newConfig)(taskToNode)
    }
  def runTask[T](root: Task[T], state: State, streams: Streams, triggers: Triggers[Task], config: EvaluateTaskConfig)(implicit taskToNode: NodeView[Task]): (State, Result[T]) =
    {
      import ConcurrentRestrictions.{ completionService, TagMap, Tag, tagged, tagsKey }

      val log = state.log
      log.debug(s"Running task... Cancel: ${config.cancelStrategy}, check cycles: ${config.checkCycles}, forcegc: ${config.forceGarbageCollection}")
      val tags = tagged[Task[_]](_.info get tagsKey getOrElse Map.empty, Tags.predicate(config.restrictions))
      val (service, shutdownThreads) = completionService[Task[_], Completed](tags, (s: String) => log.warn(s))

      def shutdown(): Unit = {
        // First ensure that all threads are stopped for task execution.
        shutdownThreads()

        // Now we run the gc cleanup to force finalizers to clear out file handles (yay GC!)
        if (config.forceGarbageCollection) {
          GCUtil.forceGcWithInterval(config.minForcegcInterval, log)
        }
      }
      // propagate the defining key for reporting the origin
      def overwriteNode(i: Incomplete): Boolean = i.node match {
        case Some(t: Task[_]) => transformNode(t).isEmpty
        case _                => true
      }
      def run() = {
        val x = new Execute[Task](Execute.config(config.checkCycles, overwriteNode), triggers, config.progressReporter)(taskToNode)
        val (newState, result) =
          try {
            val results = x.runKeep(root)(service)
            storeValuesForPrevious(results, state, streams)
            applyResults(results, state, root)
          } catch { case inc: Incomplete => (state, Inc(inc)) }
          finally shutdown()
        val replaced = transformInc(result)
        logIncResult(replaced, state, streams)
        (newState, replaced)
      }
      object runningEngine extends RunningTaskEngine {
        def cancelAndShutdown(): Unit = {
          println("")
          log.warn("Canceling execution...")
          shutdown()
        }
      }
      // Register with our cancel handler we're about to start.
      val strat = config.cancelStrategy
      val cancelState = strat.onTaskEngineStart(runningEngine)
      try run()
      finally strat.onTaskEngineFinish(cancelState)
    }

  private[this] def storeValuesForPrevious(results: RMap[Task, Result], state: State, streams: Streams): Unit =
    for (referenced <- Previous.references in Global get Project.structure(state).data)
      Previous.complete(referenced, results, streams)

  def applyResults[T](results: RMap[Task, Result], state: State, root: Task[T]): (State, Result[T]) =
    (stateTransform(results)(state), results(root))
  def stateTransform(results: RMap[Task, Result]): State => State =
    Function.chain(
      results.toTypedSeq flatMap {
        case results.TPair(Task(info, _), Value(v)) => info.post(v) get transformState
        case _                                      => Nil
      }
    )

  def transformInc[T](result: Result[T]): Result[T] =
    // taskToKey needs to be before liftAnonymous.  liftA only lifts non-keyed (anonymous) Incompletes.
    result.toEither.left.map { i => Incomplete.transformBU(i)(convertCyclicInc andThen taskToKey andThen liftAnonymous) }
  def taskToKey: Incomplete => Incomplete = {
    case in @ Incomplete(Some(node: Task[_]), _, _, _, _) => in.copy(node = transformNode(node))
    case i => i
  }
  type AnyCyclic = Execute[Task]#CyclicException[_]
  def convertCyclicInc: Incomplete => Incomplete = {
    case in @ Incomplete(_, _, _, _, Some(c: AnyCyclic)) =>
      in.copy(directCause = Some(new RuntimeException(convertCyclic(c))))
    case i => i
  }
  def convertCyclic(c: AnyCyclic): String =
    (c.caller, c.target) match {
      case (caller: Task[_], target: Task[_]) =>
        c.toString + (if (caller eq target) "(task: " + name(caller) + ")" else "(caller: " + name(caller) + ", target: " + name(target) + ")")
      case _ => c.toString
    }

  def liftAnonymous: Incomplete => Incomplete = {
    case i @ Incomplete(node, tpe, None, causes, None) =>
      causes.find(inc => !inc.node.isDefined && (inc.message.isDefined || inc.directCause.isDefined)) match {
        case Some(lift) => i.copy(directCause = lift.directCause, message = lift.message)
        case None       => i
      }
    case i => i
  }

  def processResult[T](result: Result[T], log: Logger, show: Boolean = false): T =
    onResult(result, log) { v => if (show) println("Result: " + v); v }
  def onResult[T, S](result: Result[T], log: Logger)(f: T => S): S =
    result match {
      case Value(v) => f(v)
      case Inc(inc) => throw inc
    }

  // if the return type Seq[Setting[_]] is not explicitly given, scalac hangs
  val injectStreams: ScopedKey[_] => Seq[Setting[_]] = scoped =>
    if (scoped.key == streams.key)
      Seq(streams in scoped.scope <<= streamsManager map { mgr =>
        val stream = mgr(scoped)
        stream.open()
        stream
      })
    else
      Nil
}
