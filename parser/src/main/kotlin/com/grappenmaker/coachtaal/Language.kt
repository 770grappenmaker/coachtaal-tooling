package com.grappenmaker.coachtaal

import kotlin.reflect.full.declaredMemberProperties

sealed interface Language {
    val abs: String
    val arcsin: String
    val arccos: String
    val arctan: String
    val sin: String
    val cos: String
    val tan: String
    val exp: String
    val ln: String
    val log: String
    val sqr: String
    val sqrt: String
    val floor: String
    val round: String
    val factorial: String
    val max: String
    val min: String
    val random: String
    val sign: String
    val step: String
    val stop: String
    val on: String
    val off: String
    val pi: String

    val ifStatement: String
    val ifThen: String
    val elseStatement: String
    val endIfStatement: String

    val doWhileStatement: String
    val doWhileUntil: String

    val whileStatement: String
    val startDo: String
    val endDo: String

    val redoStatement: String
    val endRedo: String

    val assignmentLiteral: String
    val notOperator: String
    val andOperator: String
    val orOperator: String

    val onceInvalidStatement: String

    val startFunction: String
    val endFunction: String
    val startProcedure: String
    val endProcedure: String
}

val Language.allBuiltins
    get() = setOf(
        abs, arcsin, arccos, arctan, sin, cos, tan, exp, ln, log, sqr, sqrt, floor,
        round, factorial, max, min, random, sign, step, stop, on, off, pi
    )

val Language.builtinFunctions
    get() = setOf(
        abs, arcsin, arccos, arctan, sin, cos, tan, exp, ln, log, sqr, sqrt, floor,
        round, factorial, max, min, random, sign, step, stop
    )

val Language.builtinConstants get() = setOf(on, off, pi)

val Language.keywords
    get() = setOf(
        redoStatement,
        ifStatement,
        elseStatement,
        whileStatement,
        doWhileStatement,
        endFunction,
        endDo,
        endProcedure,
        endRedo,
        endIfStatement,
        startDo,
        ifThen,
        startFunction,
        startProcedure
    )

// FIXME: hacky
val Language.lookup get() = this::class.declaredMemberProperties.associate { it.name to it.call(this) as String }
val Language.inverseLookup get() = lookup.asSequence().associate { (k, v) -> v to k }.toMap()

data object DutchLanguage : Language {
    override val abs = "abs"
    override val arcsin = "arcsin"
    override val arccos = "arccos"
    override val arctan = "arctan"
    override val sin = "sin"
    override val cos = "cos"
    override val tan = "tan"
    override val exp = "exp"
    override val ln = "ln"
    override val log = "log"
    override val sqr = "sqr"
    override val sqrt = "sqrt"
    override val floor = "entier"
    override val round = "round"
    override val factorial = "fac"
    override val max = "max"
    override val min = "min"
    override val random = "rand"
    override val sign = "teken"
    override val step = "eenheidstap"
    override val stop = "stop"
    override val on = "aan"
    override val off = "uit"
    override val pi = "pi"

    override val ifStatement = "als"
    override val ifThen = "dan"
    override val elseStatement = "anders"
    override val endIfStatement = "eindals"

    override val doWhileStatement = "herhaal"
    override val doWhileUntil = "totdat"

    override val whileStatement = "zolang"
    override val startDo = "doe"
    override val endDo = "einddoe"

    override val redoStatement = "repeteer"
    override val endRedo = "tothier"

    override val assignmentLiteral = "wordt"
    override val notOperator = "niet"
    override val andOperator = "en"
    override val orOperator = "of"

    override val onceInvalidStatement = "zodra"

    override val startFunction = "functie"
    override val endFunction = "eindfunctie"
    override val startProcedure = "procedure"
    override val endProcedure = "eindprocedure"
}

data object EnglishLanguage : Language {
    override val abs = "abs"
    override val arcsin = "arcsin"
    override val arccos = "arccos"
    override val arctan = "arctan"
    override val sin = "sin"
    override val cos = "cos"
    override val tan = "tan"
    override val exp = "exp"
    override val ln = "ln"
    override val log = "log"
    override val sqr = "sqr"
    override val sqrt = "sqrt"
    override val floor = "entier"
    override val round = "round"
    override val factorial = "fac"
    override val max = "max"
    override val min = "min"
    override val random = "rand"
    override val sign = "sign"
    override val step = "unitstep"
    override val stop = "stop"
    override val on = "on"
    override val off = "off"
    override val pi = "pi"

    override val ifStatement = "if"
    override val ifThen = "then"
    override val elseStatement = "else"
    override val endIfStatement = "endif"

    override val doWhileStatement = "repeat"
    override val doWhileUntil = "until"

    override val whileStatement = "while"
    override val startDo = "do"
    override val endDo = "enddo"

    override val redoStatement = "redo"
    override val endRedo = "endredo"

    override val assignmentLiteral = "becomes"
    override val notOperator = "not"
    override val andOperator = "and"
    override val orOperator = "or"

    override val onceInvalidStatement = "once"

    override val startFunction = "function"
    override val endFunction = "endfunction"
    override val startProcedure = "procedure"
    override val endProcedure = "endprocedure"
}

data object SkibidiLanguage : Language by EnglishLanguage {
    override val on = "real"
    override val off = "unreal"
    override val stop = "sus"
    override val ifStatement = "skibidi"
    override val ifThen = "toilet"
    override val elseStatement = "ohio"
    override val endIfStatement = "joever"
    override val doWhileStatement = "copium"
    override val doWhileUntil = "til"
    override val whileStatement = "edge"
    override val startDo = "rizz"
    override val endDo = "joever"
    override val redoStatement = "cooking"
    override val endRedo = "joever"
}

fun Identifier.translate(from: Language, to: Language) =
    from.inverseLookup[value.lowercase()]?.let { to.lookup[it] }?.let(::Identifier) ?: this

fun Expr.translate(from: Language, to: Language): Expr = when (this) {
    is AssignmentExpr -> copy(right = right.translate(from, to))
    is BinaryOperatorExpr -> copy(
        left = left.translate(from, to),
        right = right.translate(from, to)
    )

    is CallExpr -> copy(
        name = name.translate(from, to),
        arguments = arguments.map { it.translate(from, to) }
    )

    is ConditionalExpr -> copy(
        condition = condition.translate(from, to),
        whenTrue = whenTrue.map { it.translate(from, to) },
        whenFalse = whenFalse?.map { it.translate(from, to) }
    )

    is FunctionExpr -> copy(body = body.map { it.translate(from, to) })
    is IdentifierExpr -> copy(value = value.translate(from, to))
    is NotExpr -> copy(on = on.translate(from, to))
    is RepeatUntilExpr -> copy(
        condition = condition.translate(from, to),
        body = body.map { it.translate(from, to) }
    )

    is RepeatingExpr -> copy(
        body = body.map { it.translate(from, to) },
        repetitions = repetitions.translate(from, to)
    )

    is UnaryMinusExpr -> copy(on = on.translate(from, to))
    is WhileExpr -> copy(
        condition = condition.translate(from, to),
        body = body.map { it.translate(from, to) }
    )

    else -> this
}

fun ParsedProgram.translate(language: Language) = copy(
    lines = lines.map { it.translate(this.language, language) },
    functions = functions.map { it.translate(this.language, language) as FunctionExpr },
    language = language
)

fun Language.createConstants() = mapOf(
    on to 255f,
    off to 0f,
    pi to Math.PI.toFloat()
)