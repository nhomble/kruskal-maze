package org.hombro.maze

import org.scalajs.dom
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}

object MazeApp {
  val length: Int = 16
  val offset: Int = 2
  val thickness: Int = offset * 2


  def main(args: Array[String]): Unit = {
    println("Init")
    val canvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    val Array(cw, ch) = Array(dom.window.innerWidth, dom.window.innerHeight)
      .map(l => (.95 * l).toInt)
      .map(r => r - (r % length))
    canvas.width = cw
    canvas.height = ch

    dom.document.body.appendChild(canvas)

    gameStart2(canvas, context)
  }

  private def gameStart(canvas: HTMLCanvasElement, context: CanvasRenderingContext2D): Unit = {
    println("Game start")

    val openings = MazeGenerator.create(canvas.width / length, canvas.height / length)
    println(s"Found ${openings.size} openings")
    /*
    Openings represents the edges we need to tunnel through, remember it's a scaled down version
     */
    context.fillStyle = "#000000"
    context.fillRect(0, 0, canvas.width, canvas.height)

    context.fillStyle = "#FFFFFF"
    for (e <- openings) yield {
      val r = e.tunnel(length)
      context.fillRect(r._1 * length + offset, r._2 * length + offset, r._3 - thickness, r._4 - thickness)
    }
    println("Game end")
  }

  private def gameStart2(canvas: HTMLCanvasElement, ctx: CanvasRenderingContext2D): Unit = {
    println("Game start")

    val params = MazeParams(canvas.width / length, canvas.height / length, canvas.width, canvas.height)
    val renderInfo = generateKruskal(params)
    ctx.lineWidth = 2
    ctx.strokeStyle = "black"
    ctx.beginPath()

    renderInfo.foreach {
      case ((x1, y1), (x2, y2)) =>
        ctx.moveTo(x1, y1)
        ctx.lineTo(x2, y2)
    }
    ctx.stroke()

    println("Game end")
  }
}
