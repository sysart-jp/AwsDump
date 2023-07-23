// This describes an AWS ALB listener rules
// collapsing `source-ip` conditions split by the 5 conditions per rule limit.

// Example run:
// scala-cli AlbDescribeRulesCollapsed.scala -- --listener-arn arn:aws:elasticloadbalancing:ap-northeast-1:066614912499:listener/app/caw0034-stg-ecs-alb-pub/23e8edc906ea0b98/b8e1ebd807389106

//> using scala 3.3
//> using dep co.fs2::fs2-io:3.7.0
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.23.2
//> using dep com.monovore::decline-effect:2.4.1

import fs2.io.process.ProcessBuilder
import fs2.io.stderr
import fs2.text
import cats.effect.{Sync, IO, IOApp, ExitCode}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{JsonCodecMaker, CodecMakerConfig}
import com.monovore.decline.{Opts, Command}
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.UTF_8

object AlbDescribeRulesCollapsed extends IOApp:
  type F[A] = IO[A]

  case class Response(
    Rules: Seq[Response.Rule]
  )
  object Response:
    case class Rule(
      RuleArn: String,
      Priority: String,
      Conditions: Seq[Condition],
      Actions: Seq[Action],
      IsDefault: Boolean,
    ):
      def prioritySortValue: Int = Priority match
        case "default" => Int.MaxValue
        case intText => intText.toInt

    case class Condition(
      Field: String,
      Values: Seq[String],
      SourceIpConfig: Option[Values],
      HostHeaderConfig: Option[Values],
      PathPatternConfig: Option[Values],
    ):
      def sameAs(other: Condition): Boolean = Field match
        case "source-ip"    => other.SourceIpConfig    == SourceIpConfig // no Condition.Values
        // http request method
        // http header
        case "host-header"  => other.HostHeaderConfig  == HostHeaderConfig // Condition.Values seems to be the same as  HostHeaderConfig.Values
        case "path-pattern" => other.PathPatternConfig == PathPatternConfig // Condition.Values seems to be the same as  PathPatternConfig.Values
        // query string

    case class Values(
      Values: Seq[String],
    )
    case class Action(
      Type: String,
      TargetGroupArn: Option[String],
      Order: Int,
      ForwardConfig: Option[ForwardConfig],
      FixedResponseConfig: Option[FixedResponseConfig],
    )
    case class ForwardConfig(
      TargetGroups: Seq[TargetGroup],
      TargetGroupStickinessConfig: TargetGroupStickinessConfig,
    )
    case class TargetGroup(
      TargetGroupArn: String,
      Weight: Int,
    )
    case class TargetGroupStickinessConfig(
      Enabled: Boolean,
    )
    case class FixedResponseConfig(
      MessageBody: String,
      StatusCode: String,
      ContentType: String,
    )
  given JsonValueCodec[Response] = JsonCodecMaker.make

  def run(args: List[String]): F[ExitCode] =
    for
      args <- Sync[F].fromEither(command.parse(args).left.map(help => RuntimeException(help.toString)))
      jsonText <- exec("aws", "elbv2", "describe-rules",
          "--listener-arn", args.listenerArn)
      resp = readFromString[Response](jsonText)

      rulesCollapsed =
        if resp.Rules.nonEmpty then
          val sorted = resp.Rules.sortBy(_.prioritySortValue)
          sorted.tail.foldLeft(sorted.take(1)): (results, rule) =>
            collapseRules(results.last, rule) match
              case Some(collapsed) => results.init :+ collapsed
              case None => results :+ rule
        else
          Nil

      // giving JsonCodecMaker.makeWithRequiredCollectionFields
      // outputs `"Conditions": [ ]` like aws cli v2, but also
      // outputs `""Values": [ ]` unlike aws cli v2 (making Values Option might solve this)
      _ = println(writeToString(Response(rulesCollapsed),
          WriterConfig.withIndentionStep(4))) // 4 indent like aws cli v2

    yield ExitCode.Success // the value is 0

  import Response.{Rule, Condition, Values}
  // collapse if the actions are the same with the previous rule
  // and the conditions are the same except one
  // @return the collapsed rule if the conditions are met
  def collapseRules(left: Rule, right: Rule): Option[Rule] =
    if left.IsDefault then
      throw IllegalArgumentException("left rule should not be the default")

    else if left.Actions == right.Actions && !right.IsDefault then
      def collapsedArn = s"${left.RuleArn}, ${right.RuleArn}"

      val diff = computeDiff(left.Conditions, right.Conditions)
      if diff.isEmpty then
        Some(left.copy(RuleArn = collapsedArn))

      else if diff.lefts .size == 1 && diff.lefts .head.Field == "source-ip" &&
              diff.rights.size == 1 && diff.rights.head.Field == "source-ip" then
        Some(left.copy(RuleArn = collapsedArn,
          Conditions = diff.commons :+ diff.lefts.head.copy(SourceIpConfig = Some(Values(
            diff.lefts .head.SourceIpConfig.get.Values ++
            diff.rights.head.SourceIpConfig.get.Values,
          ))),
        ))

      else
        None

    else
      None

  case class CondsDiff(commons: Seq[Condition], lefts: Seq[Condition], rights: Seq[Condition]):
    def isEmpty: Boolean = lefts.isEmpty && rights.isEmpty

  def computeDiff(lefts: Seq[Condition], rights: Seq[Condition],
      base: CondsDiff = CondsDiff(Nil, Nil, Nil)): CondsDiff =
    if lefts.isEmpty then
      base.copy(rights = base.rights ++ rights)

    else
      rights.indexWhere(_.sameAs(lefts.head)) match
        case -1 =>
          computeDiff(lefts.tail, rights,
              base.copy(lefts = base.lefts :+ lefts.head))

        case index =>
          val (rightsPrefix, rightsSuffix) = rights.splitAt(index)
          computeDiff(lefts.tail, rightsPrefix ++ rightsSuffix.tail,
              base.copy(commons = base.commons :+ lefts.head))

  /** Command line usage */
  val command = Command("AlbDescribeRulesCollapsed", ""):
    val listenerArn = Opts.option[String]("listener-arn", "")
    listenerArn.map(Args.apply)
  case class Args(listenerArn: String)

  /**
   * Executes an OS command.
   * Fails unless the exit code is 0.
   * @return the standard output
   */
  def exec(command: String, args: String*)(using charset: Charset = UTF_8): F[String] =
    ProcessBuilder(command, args: _*).spawn[F].use: proc =>
      val stdoutCollect = proc.stdout.through(text.decodeWithCharset(charset))
      val stderrReport = proc.stderr.through(stderr)
      for
        out <- stderrReport.merge(stdoutCollect).compile.string
        _ <- proc.exitValue.flatMap: exitValue =>
          if exitValue == 0 then Sync[F].unit
          else Sync[F].raiseError(RuntimeException(s"exitValue is $exitValue: $command ${args.mkString(" ")}"))
      yield out
