package com.grappenmaker.coachtaal

import kotlin.math.ceil

class StopAnalysis(program: ParsedProgram) {
    val stopCalls = mutableMapOf(program.language.stop to true)

    private val byName = program.functions.associateBy { it.name }
    fun Identifier.check() = stopCalls.getOrPut(value) { byName[this]?.body?.callsStop() ?: false }

    private fun List<Expr>.callsStop() = any { it.callsStop() }

    private fun Expr.callsStop(): Boolean = when (this) {
        is CallExpr -> name.check()
        is ConditionalExpr -> {
            whenTrue.callsStop() || whenFalse?.callsStop() ?: false
        }

        is RepeatUntilExpr -> body.callsStop()
        is RepeatingExpr -> body.callsStop()
        is WhileExpr -> body.callsStop()
        else -> false
    }
}

fun ParsedProgram.terminableFunctions(): Set<FunctionExpr> {
    val target = StopAnalysis(this)
    return with(target) { functions.filterTo(hashSetOf()) { it.name.check() } }
}

// not WILL terminate since halting problem xd
fun ParsedProgram.couldTerminate() = with(StopAnalysis(this)) { lines.couldTerminate() }

context(StopAnalysis)
fun List<Expr>.couldTerminate() = any { it.couldTerminate() }

context(StopAnalysis)
fun Expr.couldTerminate(): Boolean = when (this) {
    is CallExpr -> name.check()
    is ConditionalExpr -> {
        whenTrue.couldTerminate() || whenFalse?.couldTerminate() ?: false
    }

    is RepeatUntilExpr -> body.couldTerminate()
    is RepeatingExpr -> body.couldTerminate()
    is WhileExpr -> body.couldTerminate()
    else -> false
}

infix fun <T> Set<T>.xor(other: Set<T>) = (this - other) + (other - this)

data class ConstantNode(
    val identifier: Identifier,
    val value: Float,
    val dependants: MutableSet<Identifier> = hashSetOf()
)

data class ConstantsCollector(
    val possibleConstants: MutableMap<Identifier, ConstantNode> = hashMapOf(),
    val notConstant: MutableSet<Identifier> = hashSetOf(),
) {
    // bad code
    data class ConstantExprEval(val value: Float, val dependencies: Set<Identifier>)

    private fun Expr.evalOrNull(): ConstantExprEval? {
        val target = hashSetOf<Identifier>()
        return evalOrNullBottom(target)?.let { ConstantExprEval(it, target.toSet()) }
    }

    private fun Expr.evalOrNullBottom(dependencies: MutableSet<Identifier>): Float? {
        return when (this) {
            is BinaryOperatorExpr -> {
                val le = left.evalOrNullBottom(dependencies) ?: return null
                val re = right.evalOrNullBottom(dependencies) ?: return null
                operator(le, re)
            }

            is IdentifierExpr -> possibleConstants[value]?.value?.also { dependencies += value }
            is LiteralExpr -> value
            is NotExpr -> on.evalOrNullBottom(dependencies)?.let { (!it.asCoachBoolean).asCoach }
            is UnaryMinusExpr -> on.evalOrNullBottom(dependencies)?.let { -it }
            else -> null
        }
    }

    private fun Identifier.deleteRecursive(): Boolean {
        var result = false

        // No actual recursion though
        val seen = hashSetOf(this)
        val queue = ArrayDeque<Identifier>()
        queue += this

        while (queue.isNotEmpty()) {
            val id = queue.removeLast()
            val node = possibleConstants.remove(id) ?: continue

            result = true
            notConstant += id
            node.dependants.forEach { if (!seen.add(it)) queue.addLast(it) }
        }

        return result
    }

    fun flow(item: Identifier, value: Expr) {
        if (item in notConstant) return
        if (item.deleteRecursive()) return

        val eval = value.evalOrNull()
        if (eval == null) {
            notConstant += item
            return
        }

        possibleConstants[item] = ConstantNode(item, eval.value)
        eval.dependencies.forEach { possibleConstants.getValue(it).dependants += item }
    }
}

data class VariablesAnalysis(val constants: Map<Identifier, Float>, val assignments: Set<Identifier>)

// bad pattern once again!
fun ParsedProgram.analyzeVariables(): VariablesAnalysis {
    val target = ConstantsCollector()
    target.possibleConstants += language.createConstants().map { (k, v) ->
        val id = Identifier(k)
        id to ConstantNode(id, v)
    }

    with(target) {
        lines.analyzeVariables()
        functions.forEach {
            it.body.analyzeVariables()

            // Pretend a function gets assigned multiple times
            // (its name is technically not assigned, except in its body)
            flow(it.name, IdentifierExpr(it.name))
        }
    }

    val constants = target.possibleConstants.mapValues { it.value.value }
    val assignments = target.possibleConstants.keys + target.notConstant
    return VariablesAnalysis(constants, assignments)
}

context(ConstantsCollector)
private fun List<Expr>.analyzeVariables() = forEach { it.analyzeVariables() }

context(ConstantsCollector)
private fun Expr.analyzeVariables() {
    when (this) {
        is AssignmentExpr -> flow(left, right)
        // <These technically should never contain assignments>
        is BinaryOperatorExpr -> {
            left.analyzeVariables()
            right.analyzeVariables()
        }

        is CallExpr -> arguments.analyzeVariables()
        is NotExpr -> on.analyzeVariables()
        is UnaryMinusExpr -> on.analyzeVariables()
        // </These technically should never contain assignments>
        is ConditionalExpr -> {
            condition.analyzeVariables()
            whenTrue.analyzeVariables()
            whenFalse?.analyzeVariables()
        }

        is RepeatUntilExpr -> {
            condition.analyzeVariables()
            body.analyzeVariables()
        }

        is RepeatingExpr -> {
            repetitions.analyzeVariables()
            body.analyzeVariables()
        }

        is WhileExpr -> {
            condition.analyzeVariables()
            body.analyzeVariables()
        }

        else -> { /* not interesting */
        }
    }
}

fun ParsedProgram.optimizeWithInit(init: ParsedProgram): Pair<ParsedProgram, ParsedProgram> {
    require(language == init.language)

    val (initConstants, initAssignments) = init.analyzeVariables()
    val (iterConstants) = analyzeVariables()
    val bothConstantIds = (initConstants.keys xor iterConstants.keys) - initAssignments
    val bothConstants = (initConstants + iterConstants).filterKeys { it in bothConstantIds }

    return optimizeInternal(bothConstants).assertTermination() to init.optimizeInternal(initConstants)
}

fun ParsedProgram.assertTermination(): ParsedProgram {
    if (!couldTerminate()) error(
        "Static analysis showed that there is no possible way the iteration program could terminate!"
    )

    return this
}

fun ParsedProgram.optimize() = optimizeInternal(analyzeVariables().constants)
private fun ParsedProgram.optimizeInternal(constants: Map<Identifier, Float>): ParsedProgram {
    val ctx = OptimizerContext(constants)
    return copy(
        lines = with(ctx) { lines.optimize() },
        functions = functions.map { it.copy(body = with(ctx) { it.body.optimize() }) }
    )
}

context(OptimizerContext)
fun List<Expr>.optimize(): List<Expr> = flatMap { it.optimize() }

context(OptimizerContext)
fun Expr.optimizeSingle() = optimize().singleOrNull() ?: this

context(OptimizerContext)
fun Expr.constantOrNull(): Float? = when (this) {
    is LiteralExpr -> value
    is IdentifierExpr -> constants[value]
    else -> null
}

// Very very basic and poorly written peephole optimizer
context(OptimizerContext)
fun Expr.optimize(): List<Expr> = when (this) {
    is ConditionalExpr -> {
        val iter = copy(condition.optimizeSingle(), whenTrue.optimize(), whenFalse?.optimize())

        val (c1, t1, f1) = iter
        val c1c = c1.constantOrNull()

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
        val iter = copy(condition.optimizeSingle(), body.optimize())
        val (c1, b1) = iter

        val c1c = c1.constantOrNull()
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
        val iter = copy(body.optimize(), repetitions.optimizeSingle())
        val (b1, r1) = iter

        val r1c = r1.constantOrNull()
        if (r1c != null) when (ceil(r1c).toInt()) {
            0 -> listOf()
            1 -> b1
            else -> listOf(iter)
        } else listOf(iter)
    }

    is WhileExpr -> {
        val iter = copy(condition.optimizeSingle(), body.optimize())
        val (c1, b1) = iter

        val c1c = c1.constantOrNull()
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
            is AssignmentExpr -> copy(right = right.optimizeSingle())
            is BinaryOperatorExpr -> {
                val iter = copy(left.optimizeSingle(), right.optimizeSingle())
                val (l1, r1) = iter

                val (l1c, r1c) = l1.constantOrNull() to r1.constantOrNull()
                val constant = l1c ?: r1c

                when {
                    l1c != null && r1c != null -> LiteralExpr(operator(l1c, r1c))
                    else -> if (constant != null) {
                        val nonConstant = if (constant == l1c) r1 else l1
                        when (operatorToken) {
                            "&&" -> if (constant.asCoachBoolean) nonConstant else LiteralExpr(false.asCoach)
                            "||" -> if (constant.asCoachBoolean) LiteralExpr(true.asCoach) else nonConstant
                            else -> iter
                        }
                    } else iter
                }
            }

            is CallExpr -> copy(arguments = arguments.map { it.optimizeSingle() })
            is NotExpr -> {
                val l1 = on.optimizeSingle()
                val l1c = l1.constantOrNull()
                if (l1c != null) LiteralExpr((!l1c.asCoachBoolean).asCoach) else NotExpr(l1)
            }

            is UnaryMinusExpr -> {
                val l1 = on.optimizeSingle()
                val l1c = l1.constantOrNull()
                if (l1c != null) LiteralExpr(-l1c) else UnaryMinusExpr(l1)
            }

            is IdentifierExpr -> constants[value]?.let(::LiteralExpr) ?: this

            is FunctionExpr -> copy(body = body.optimize())

            else -> this
        }
    )
}

class OptimizerContext(val constants: Map<Identifier, Float>)