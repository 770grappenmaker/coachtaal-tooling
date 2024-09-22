package com.grappenmaker.coachtaal

import kotlinx.serialization.Serializable
import java.text.DecimalFormat
import java.util.*

@Serializable
data class FormatterConfig(
    val crlf: Boolean = false,
    val indent: Int = 2,
    val assignment: AssignmentOperatorFormat = AssignmentOperatorFormat.WALRUS
)

@Serializable
enum class AssignmentOperatorFormat {
    EQUALS, WALRUS, WORD
}

val FormatterConfig.newLine get() = if (crlf) "\r\n" else "\n" // not supporting CR i hate it
val FormatterConfig.indentString get() = " ".repeat(indent)

fun FormatterConfig.assignmentString(language: Language) = when (assignment) {
    AssignmentOperatorFormat.EQUALS -> "="
    AssignmentOperatorFormat.WALRUS -> ":="
    AssignmentOperatorFormat.WORD -> language.assignmentLiteral
}

data class FormatterContext(val language: Language, val config: FormatterConfig)

val FormatterContext.assignmentString get() = config.assignmentString(language)
val FormatterContext.newLine get() = config.newLine
val FormatterContext.indentString get() = config.indentString

fun ParsedProgram.asText(config: FormatterConfig = FormatterConfig()) =
    with(FormatterContext(language, config)) { (functions + lines).asText() }

context(FormatterContext)
fun List<Expr>.asText() = joinToString(newLine) { it.asText() }

context(FormatterContext)
fun List<Expr>.asBlock() = asText().indent()

context(FormatterContext)
private fun String.indent() = lines().joinToString(newLine) { indentString + it }

val Language.locale: Locale get() = if (this is DutchLanguage) Locale.forLanguageTag("nl") else Locale.US
fun Language.formatConstant(cst: Float): String = DecimalFormat.getNumberInstance(locale)
    .apply { maximumFractionDigits = 6 }
    .format(cst)

// TODO: translation
context(FormatterContext)
fun Expr.asText(): String = when (this) {
    is AssignmentExpr -> "${left.value} $assignmentString ${right.asText()}"
    is BinaryOperatorExpr -> "${left.asText()} ${
        when (operatorToken) {
            "&&" -> language.andOperator
            "||" -> language.orOperator
            else -> operatorToken
        }
    } ${right.asText()}"

    is CallExpr -> name.value +
            if (arguments.isNotEmpty()) "(${arguments.joinToString(";") { it.asText() }})" else ""

    is ConditionalExpr -> "${language.ifStatement} ${condition.asText()} ${language.ifThen}" +
            "$newLine${whenTrue.asBlock()}$newLine" +
            (if (whenFalse != null) "${language.elseStatement}$newLine${whenFalse.asBlock()}$newLine" else "") +
            language.endIfStatement

    is IdentifierExpr -> value.value
    is LiteralExpr -> language.formatConstant(value)
    is NotExpr -> "${language.notOperator} ${on.asText()}"
    is RepeatUntilExpr -> "${language.doWhileStatement}$newLine${body.asBlock()}" +
            "$newLine${language.doWhileUntil} ${condition.asText()}"

    is RepeatingExpr ->
        "${language.redoStatement} ${repetitions.asText()}$newLine${body.asBlock()}$newLine${language.endRedo}"

    is UnaryMinusExpr -> "-${on.asText()}"
    is WhileExpr -> "${language.whileStatement} ${condition.asText()} ${language.startDo}$newLine" +
            "${body.asBlock()}$newLine${language.endDo}"

    is FunctionExpr -> "${language.startFunction} ${name.value}" +
            (if (parameters.isNotEmpty()) "(${parameters.joinToString(";") { it.value }})" else "") +
            "$newLine${body.asBlock()}$newLine${language.endFunction}"

    is EOLExpr -> if (comment != null) "' $comment" else ""
}
