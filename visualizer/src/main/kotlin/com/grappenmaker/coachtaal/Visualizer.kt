package com.grappenmaker.coachtaal

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Input
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

    private var xOffset = 0f
    private var yOffset = 0f
    private var zoom = 1f

    override fun render(delta: Float) {
        if (Gdx.input.isButtonPressed(Input.Buttons.LEFT)) {
            xOffset += Gdx.input.deltaX
            yOffset -= Gdx.input.deltaY
        }

        if (Gdx.input.isKeyPressed(Input.Keys.UP)) zoom -= delta
        if (Gdx.input.isKeyPressed(Input.Keys.DOWN)) zoom += delta
        zoom = zoom.clamp(.1f, 3f)

        cam.update()

        val xScale = (viewport.worldWidth - doubleMargin) / widthUnscaled * zoom
        val yScale = (viewport.worldHeight - quadMargin) / heightUnscaled * zoom

        shapes.use(ShapeRenderer.ShapeType.Line, cam) {
            plot.windowed(2).forEach { (a, b) ->
                shapes.line(
                    (a.first - minX) * xScale + margin + xOffset,
                    (a.second - minY) * yScale + margin + yOffset,
                    (b.first - minX) * xScale + margin + xOffset,
                    (b.second - minY) * yScale + margin + yOffset,
                )
            }

            shapes.line(
                margin,
                margin - minY * yScale + yOffset,
                viewport.worldWidth - margin,
                margin - minY * yScale + yOffset
            )

            shapes.line(
                margin - minX * xScale + xOffset,
                margin,
                margin - minX * xScale + xOffset,
                viewport.worldHeight - tripleMargin
            )
        }

        val mx = Gdx.input.x.toFloat()
        if (mx - margin - xOffset in 0f..maxX * xScale) {
            val approxX = (mx - margin - xOffset) / xScale
            val approxY = valueAt(approxX)
            shapes.use(ShapeRenderer.ShapeType.Line, cam) {
                shapes.line(
                    mx - minX * xScale,
                    margin - minY * yScale + yOffset,
                    mx - minX * xScale,
                    margin + (approxY - minY) * yScale + yOffset
                )
            }

            sprites.use(cam) {
                font.draw(
                    it,
                    "$xVariable=${approxX.formatShort()},$yVariable=${approxY.formatShort()}",
                    mx - minX * xScale + margin,
                    margin + (approxY - minY) * yScale + yOffset
                )
            }
        }

        shapes.use(ShapeRenderer.ShapeType.Filled, cam) {
            if (lineThickness > 0f) {
                plot.forEach { (x, y) ->
                    shapes.circle(
                        (x - minX) * xScale + margin + xOffset,
                        (y - minY) * yScale + margin + yOffset,
                        lineThickness
                    )
                }
            }
        }

        sprites.use(cam) {
            val xoScaled = xOffset / xScale
            val yoScaled = yOffset / yScale

            font.draw(
                it, "($yVariable,$xVariable) diagram: ${logbook.size} points / " +
                        "Window: $xVariable = [${(minX / zoom + xoScaled).formatShort()}, " +
                        "${(maxX / zoom + xoScaled).formatShort()}], " +
                        "$yVariable = [${(minY / zoom + yoScaled).formatShort()}, " +
                        "${(maxY / zoom + yoScaled).formatShort()}]",
                5f, viewport.worldHeight - 5f
            )

            font.draw(
                it,
                yVariable,
                (doubleMargin - minX * xScale + xOffset).clamp(doubleMargin, viewport.worldWidth - doubleMargin),
                viewport.worldHeight - tripleMargin
            )

            font.draw(
                it,
                xVariable,
                viewport.worldWidth - doubleMargin,
                (tripleMargin - minY * yScale + yOffset).clamp(tripleMargin, viewport.worldHeight - tripleMargin)
            )
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

    private fun valueAt(targetX: Float, interpolate: Boolean = false): Float {
        var min = 0
        var max = plot.indices.last

        while (min <= max) {
            val pivot = (min + max) / 2
            val (x, y) = plot[pivot]
            when {
                x < targetX -> min = pivot + 1
                x > targetX -> max = pivot - 1
                else -> return y
            }
        }

        return when {
            min !in plot.indices -> plot.last().second
            interpolate -> {
                val (x1, y1) = plot[min]
                val (x2, y2) = plot[max]
                val slope = (y2 - y1) / (x2 - x1)
                y1 + slope * (targetX - x1)
            }
            else -> plot[min].second
        }
    }

    private fun Float.formatShort() = "%.1f".format(null, this)
}

fun Float.clamp(min: Float, max: Float) = coerceAtLeast(min).coerceAtMost(max)