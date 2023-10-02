package com.grappenmaker.coachtaal

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.backends.lwjgl3.Lwjgl3Application
import com.badlogic.gdx.graphics.OrthographicCamera
import com.badlogic.gdx.graphics.g2d.BitmapFont
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.utils.viewport.ScreenViewport
import ktx.app.KtxGame
import ktx.app.KtxScreen
import ktx.graphics.center
import ktx.graphics.use
import kotlin.math.absoluteValue

fun main() {
    val initialize = parseProgram(
        """
            t = 0
            m = 0,0050
            g = 9,81
            Fz = m * g
            x = 0
            y = 1,5
            v = 20
            alfa = 15 * Pi / 180
            k = 0,003
            vx = v * cos(alfa)
            vy = v * sin(alfa)
            dt = 0,001
        """.trimIndent()
    )

    val iteration = parseProgram(
        """
           v = sqrt(vx^2 + vy^2)
           
           Fw = k * v^2
           Fwx = Fw * (vx / v)
           Fwy = Fw * (vy / v)
           
           Fresx = -Fwx
           Fresy = -Fz - Fwy
           
           ax = Fresx / m
           ay = Fresy / m
           
           vx = vx + ax * dt
           vy = vy + ay * dt
           
           x = x + vx * dt
           y = y + vy * dt
           
           t = t + dt
           als y <= 0 dan
               stop
               y = 0
           eindals
        """.trimIndent()
    )

    val interpreter = Interpreter(
        iteration,
        initialize,
        logVariables = setOf("y", "x")
    )

    interpreter.run()
    interpreter.logbook.visualize("x", "y")
}

fun List<List<LogbookEntry>>.visualize(xVariable: String, yVariable: String) =
    Lwjgl3Application(VisualizerApp(this, xVariable, yVariable))

class VisualizerApp(
    private val logbook: List<List<LogbookEntry>>,
    private val xVariable: String,
    private val yVariable: String,
) : KtxGame<KtxScreen>() {
    override fun create() {
        addScreen(VisualizerScreen(logbook, xVariable, yVariable))
        setScreen<VisualizerScreen>()
    }
}

class VisualizerScreen(
    private val logbook: List<List<LogbookEntry>>,
    private val xVariable: String,
    private val yVariable: String,
    private val lineThickness: Float = 0f,
    private val margin: Float = 10f,
) : KtxScreen {
    private val shapes = ShapeRenderer()
    private val sprites = SpriteBatch()
    private val font = BitmapFont()

    private val cam = OrthographicCamera()
    private val viewport = ScreenViewport(cam)
    private val plot = logbook.map { v ->
        // Oh no, inefficient!
        val xe = v.find { it.variable == xVariable }
            ?: error("Variable $xVariable not found for logbook entry $v!")

        val ye = v.find { it.variable == yVariable }
            ?: error("Variable $yVariable not found for logbook entry $v!")

        xe.value to ye.value
    }

    private val minX = plot.minOf { (x) -> x }
    private val maxX = plot.maxOf { (x) -> x }
    private val widthUnscaled = (maxX - minX).absoluteValue

    private val minY = plot.minOf { (_, y) -> y }
    private val maxY = plot.maxOf { (_, y) -> y }
    private val heightUnscaled = (maxY - minY).absoluteValue

    private val doubleMargin = margin * 2f
    private val tripleMargin = margin * 3f
    private val quadMargin = margin * 4f

    override fun render(delta: Float) {
        cam.update()

        val xScale = (viewport.worldWidth - doubleMargin) / widthUnscaled
        val yScale = (viewport.worldHeight - quadMargin) / heightUnscaled

        shapes.use(ShapeRenderer.ShapeType.Line, cam) {
            plot.windowed(2).forEach { (a, b) ->
                shapes.line(
                    a.first * xScale + margin,
                    a.second * yScale + margin,
                    b.first * xScale + margin,
                    b.second * yScale + margin,
                )
            }

            shapes.line(margin, margin, viewport.worldWidth - margin, margin)
            shapes.line(margin, margin, margin, viewport.worldHeight - tripleMargin)
        }

        shapes.use(ShapeRenderer.ShapeType.Filled, cam) {
            if (lineThickness > 0f) {
                plot.forEach { (x, y) -> shapes.circle(x * xScale + margin, y * yScale + margin, lineThickness) }
            }
        }

        sprites.use(cam) {
            font.draw(it, "($yVariable,$xVariable) diagram: ${logbook.size} points / " +
                    "Window: X = [${minX.formatShort()}, ${maxX.formatShort()}], " +
                    "Y = [${minY.formatShort()}, ${maxY.formatShort()}]", 5f, Gdx.graphics.height - 5f)

            font.draw(it, yVariable, doubleMargin, viewport.worldHeight - tripleMargin)
            font.draw(it, xVariable, viewport.worldWidth - doubleMargin, tripleMargin)
        }
    }

    override fun dispose() {
        shapes.dispose()
        sprites.dispose()
        font.dispose()
    }

    override fun resize(width: Int, height: Int) {
        viewport.update(width, height)
        cam.center()
    }

    private fun Float.formatShort() = "%.1f".format(this)
}