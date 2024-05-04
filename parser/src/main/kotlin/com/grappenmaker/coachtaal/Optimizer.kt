package com.grappenmaker.coachtaal

import kotlin.math.ceil

fun List<Expr>.collectAssignments(): Set<Identifier> = flatMapTo(hashSetOf()) { it.collectAssignments() }
fun Expr.collectAssignments(): Set<Identifier> = when (this) {
    is AssignmentExpr -> setOf(left)
    // <These technically should never contain assignments>
    is BinaryOperatorExpr -> left.collectAssignments() + right.collectAssignments()
    is CallExpr -> arguments.collectAssignments()
    is NotExpr -> on.collectAssignments()
    is UnaryMinusExpr -> on.collectAssignments()
    // </These technically should never contain assignments>
    is ConditionalExpr ->
        condition.collectAssignments() + whenTrue.collectAssignments() + (whenFalse?.collectAssignments() ?: emptySet())

    is RepeatUntilExpr -> condition.collectAssignments() + body.collectAssignments()
    is RepeatingExpr -> repetitions.collectAssignments() + body.collectAssignments()
    is WhileExpr -> condition.collectAssignments() + body.collectAssignments()
    else -> emptySet()
}

class LanguageConstantsHolder(
    private val delegate: MutableMap<Identifier, Float>,
    private val language: Language,
) : MutableMap<Identifier, Float> by delegate {
    private val ref = language.createConstants()
    override fun get(key: Identifier) = ref[key.value] ?: delegate[key]

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as LanguageConstantsHolder

        if (delegate != other.delegate) return false
        if (language != other.language) return false

        return true
    }

    override fun hashCode(): Int {
        var result = delegate.hashCode()
        result = 31 * result + language.hashCode()
        return result
    }

    override fun toString() = "LanguageConstantsHolder($language, $delegate)"
}

fun ParsedProgram.optimizeWithInit(init: ParsedProgram): Pair<ParsedProgram, ParsedProgram> {
    require(language == init.language)

    val initConstantsTarget = LanguageConstantsHolder(hashMapOf(), language)
    val optimizedInit = init.optimizeInternal(initConstantsTarget)
    initConstantsTarget -= lines.collectAssignments()

    return optimizeInternal(initConstantsTarget) to optimizedInit
}

fun ParsedProgram.optimize() = optimizeInternal(LanguageConstantsHolder(hashMapOf(), language))
private fun ParsedProgram.optimizeInternal(knownConstants: MutableMap<Identifier, Float>) =
    copy(lines = lines.optimize(knownConstants))

fun List<Expr>.optimize(knownConstants: MutableMap<Identifier, Float> = hashMapOf()): List<Expr> =
    flatMap { it.optimize(knownConstants) }

fun Expr.optimizeSingle(knownConstants: MutableMap<Identifier, Float>) =
    optimize(knownConstants).singleOrNull() ?: this

fun Expr.constantOrNull(knownConstants: Map<Identifier, Float>): Float? = when (this) {
    is LiteralExpr -> value
    is IdentifierExpr -> knownConstants[value]
    else -> null
}

// Very very basic and poorly written peephole optimizer
context(OptimizerContext)
fun Expr.optimize(knownConstants: MutableMap<Identifier, Float>): List<Expr> = when (this) {
    is ConditionalExpr -> {
        val iter = copy(
            condition.optimizeSingle(knownConstants),
            whenTrue.optimize(knownConstants),
            whenFalse?.optimize(knownConstants)
        )

        val (c1, t1, f1) = iter
        val c1c = c1.constantOrNull(knownConstants)
        when {
            t1.isEmpty() && f1?.isEmpty() != false -> listOf(c1)
            c1c != null -> when {
                c1c.asCoachBoolean -> t1
                else -> f1 ?: listOf()
            }

            else -> listOf(iter)
        }
    }

    is RepeatUntilExpr -> {
        val iter = copy(condition.optimizeSingle(knownConstants), body.optimize(knownConstants))
        val (c1, b1) = iter

        val c1c = c1.constantOrNull(knownConstants)
        when {
            b1.isEmpty() -> listOf(c1)
            c1c != null -> when {
                c1c.asCoachBoolean -> b1
                else -> error("Detected infinite repeat-until loop! This is not allowed (TODO debug)")
            }

            else -> listOf(iter)
        }
    }

    is RepeatingExpr -> {
        val iter = copy(body.optimize(knownConstants), repetitions.optimizeSingle(knownConstants))
        val (b1, r1) = iter

        val r1c = r1.constantOrNull(knownConstants)
        if (r1c != null) when (ceil(r1c).toInt()) {
            0 -> listOf()
            1 -> b1
            else -> listOf(iter)
        } else listOf(iter)
    }

    is WhileExpr -> {
        val iter = copy(condition.optimizeSingle(knownConstants), body.optimize(knownConstants))
        val (c1, b1) = iter

        val c1c = c1.constantOrNull(knownConstants)
        when {
            b1.isEmpty() -> listOf(c1)
            c1c != null -> when {
                c1c.asCoachBoolean -> error("Detected infinite while loop! This is not allowed (TODO debug)")
                else -> b1
            }

            else -> listOf(iter)
        }
    }

    else -> listOf(
        when (this) {
            is AssignmentExpr -> copy(right = right.optimizeSingle(knownConstants)).also {
                if (it.right is LiteralExpr) knownConstants[it.left] = it.right.value else knownConstants -= it.left
            }

            is BinaryOperatorExpr -> {
                val iter = copy(left.optimizeSingle(knownConstants), right.optimizeSingle(knownConstants))
                val (l1, r1) = iter

                val (l1c, r1c) = l1.constantOrNull(knownConstants) to r1.constantOrNull(knownConstants)
                if (l1c != null && r1c != null) LiteralExpr(operator(l1c, r1c)) else iter
            }

            is CallExpr -> copy(arguments = arguments.map { it.optimizeSingle(knownConstants) })
            is NotExpr -> {
                val l1 = on.optimizeSingle(knownConstants)
                val l1c = l1.constantOrNull(knownConstants)
                if (l1c != null) LiteralExpr((!l1c.asCoachBoolean).asCoach) else NotExpr(l1)
            }

            is UnaryMinusExpr -> {
                val l1 = on.optimizeSingle(knownConstants)
                val l1c = l1.constantOrNull(knownConstants)
                if (l1c != null) LiteralExpr(-l1c) else UnaryMinusExpr(l1)
            }

            is IdentifierExpr -> knownConstants[value]?.let(::LiteralExpr) ?: this

            else -> this
        }
    )
}

class OptimizerContext {

}