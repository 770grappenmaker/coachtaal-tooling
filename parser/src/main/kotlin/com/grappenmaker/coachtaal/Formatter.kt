package com.grappenmaker.coachtaal

import java.text.DecimalFormat
import java.util.*

fun ParsedProgram.asText() = lines.asText(language)
fun List<Expr>.asText(language: Language) = joinToString("\n") { it.asText(language) }
fun List<Expr>.asBlock(language: Language) = asText(language).indent()
private fun String.indent() = lines().joinToString("\n") { "  $it" }

val Language.locale: Locale get() = if (this is DutchLanguage) Locale.forLanguageTag("nl") else Locale.US
fun Language.formatConstant(cst: Float): String = DecimalFormat.getNumberInstance(locale).format(cst)

// TODO: translation
fun Expr.asText(language: Language): String = when (this) {
    is AssignmentExpr -> "${left.value} := ${right.asText(language)}"
    is BinaryOperatorExpr -> "${left.asText(language)} ${
        when (operatorToken) {
            "&&" -> language.andOperator
            "||" -> language.orOperator
            else -> operatorToken
        }
    } ${right.asText(language)}"

    is CallExpr -> name.value +
            if (arguments.isNotEmpty()) "(${arguments.joinToString(";") { it.asText(language) }})" else ""

    is ConditionalExpr -> "${language.ifStatement} ${condition.asText(language)} ${language.ifThen}" +
            "\n${whenTrue.asBlock(language)}\n" +
            (if (whenFalse != null) "${language.elseStatement}\n${whenFalse.asBlock(language)}\n" else "") +
            language.endIfStatement

    is IdentifierExpr -> value.value
    is LiteralExpr -> language.formatConstant(value)
    is NotExpr -> "${language.notOperator} ${on.asText(language)}"
    is RepeatUntilExpr -> "${language.doWhileStatement}\n${body.asBlock(language)}" +
            "\n${language.doWhileUntil} ${condition.asText(language)}"

    is RepeatingExpr ->
        "${language.redoStatement} ${repetitions.asText(language)}\n${body.asBlock(language)}\n${language.endRedo}"

    is UnaryMinusExpr -> "-${on.asText(language)}"
    is WhileExpr -> "${language.whileStatement} ${condition.asText(language)} ${language.startDo}\n" +
            "${body.asBlock(language)}\n${language.endDo}"

    is FunctionExpr -> "${language.startFunction} ${name.value}" +
            (if (parameters.isNotEmpty()) "(${parameters.joinToString(";") { it.value }})" else "") +
            "\n${body.asBlock(language)}\n${language.endFunction}"

    is NewLineExpr -> "" // joined into newline! krazy
}