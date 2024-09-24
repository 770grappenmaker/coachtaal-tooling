package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import com.grappenmaker.coachtaal.Project
import java.nio.file.Path
import kotlin.io.path.writeText
import kotlin.math.abs

data class Exercise(
    val language: EnumLanguage,
    val initSolution: String,
    val iterSolution: String,
    val variables: Set<String>,
    val instructions: String,
    val templateInit: String,
    val templateIter: String,
)

fun Exercise.check(project: Project) {
    val solutionModel = loadCliModel(iterSolution, initSolution, language.underlying, compile = true)
    val attemptModel = project.runner()

    val solutionResults = solutionModel.runner.run(variables)
    val attemptResults = attemptModel.run(variables)
    if (solutionResults.size != attemptResults.size) error("Too many timesteps! Check your solution!")

    val indices = solutionResults.indices
    repeat(solutionResults.size / 10) {
        val idx = indices.random()
        val lhs = solutionResults[idx]
        val rhs = attemptResults[idx]

        if (lhs.zip(rhs).any { (a, b) -> abs(a.value - b.value) > .01 })
            error("Final result is off! Check your solution!")
    }

    println("You passed!")
}

fun Exercise.init(dir: Path = cwd) = with(Project(dir, ProjectConfig(language))) {
    init()
    iterScriptPath.writeText(templateIter)
    initScriptPath.writeText(templateInit)
}

val exerciseRegistry = mapOf(
    "motion" to Exercise(
        EnumLanguage.ENGLISH,
        """
            y := 10
            g := -9.81
            t := 0
            dt := 0.001
        """.trimIndent(),
        """
            y := y + g * dt
            
            if y < 0 then
              stop
            endif
            
            t := t + dt
        """.trimIndent(),
        variables = setOf("t", "y"),
        """
            Implement simple motion of an object in freefall.
            The acceleration due to gravity is 9.81 m/s^2.
        """.trimIndent(),
        """
            y := 10
            t := 0
            dt := 0.001
        """.trimIndent(),
        """
            t := t + dt
        """.trimIndent()
    ),
    "pendulum" to Exercise(
        EnumLanguage.ENGLISH,
        """
            theta := 30 * pi / 180
            dtheta := 0
            ddtheta := 0
            g := 9.81
            l := 3
            mu := -0.3
            t := 0
            dt := 0.001
            k := -g / l
        """.trimIndent(),
        """
            ddtheta := k * sin(theta) + mu * dtheta
            dtheta := dtheta + ddtheta * dt
            theta := theta + dtheta * dt

            if t > 10 then
              stop
            endif

            t := t + dt
        """.trimIndent(),
        variables = setOf("t", "theta"),
        """
            Implement a simple pendulum using the differential equation of a pendulum.
            The pendulum has a length of 3 meters, the acceleration due to gravity is 9.81 m/s^2.
            Assume a friction coefficient of 0.3
        """.trimIndent(),
        """
            theta := 30 * pi / 180
            dtheta := 0
            ddtheta := 0
            t := 0
            dt := 0.001
        """.trimIndent(),
        """
            if t > 10 then
              stop
            endif

            t := t + dt
        """.trimIndent()
    )
)

object ExerciseCommand : CommandHolding() {
    override val name = "exercise"
    override val subCommands = listOf(ListExercise, InitExercise, CheckExercise)
    override val aliases = setOf("ex")
}

object InitExercise : Command() {
    override val name = "init"
    override val aliases = setOf("i")

    val id by string()

    override fun CommandContext.invoke() {
        val ex = exerciseRegistry.getValue(id[args])
        ex.init()
        println(ex.instructions)
    }
}

object CheckExercise : Command() {
    override val name = "check"
    override val aliases = setOf("c")

    val id by string()

    override fun CommandContext.invoke() {
        exerciseRegistry.getValue(id[args]).check(cwd.loadProject())
    }
}

object ListExercise : Command() {
    override val name = "list"
    override val aliases = setOf("l")

    override fun CommandContext.invoke() {
        exerciseRegistry.keys.forEach { println(it) }
    }
}