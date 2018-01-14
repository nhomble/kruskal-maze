package org.hombro.maze

import org.scalajs.dom
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}

import scala.scalajs.js

object MazeApp {
  val length: Int = 16
  val offset: Int = 2
  val thickness: Int = offset * 2
  val timer: Int = 10

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

    context.fillStyle = "#000000"
    context.fillRect(0, 0, canvas.width, canvas.height)

    context.fillStyle = "#FFFFFF"
    var ptr = 0
    js.timers.setInterval(timer) {
      openings.lift(ptr) match {
        case Some(e) =>
          val r = e.tunnel(length)
          context.fillRect(r._1 * length + offset, r._2 * length + offset, r._3 - thickness, r._4 - thickness)
          ptr += 1
        case None =>
      }
    }
    println("Game end")
  }

  private def gameStart2(canvas: HTMLCanvasElement, ctx: CanvasRenderingContext2D): Unit = {
    println("Game start")
    val (nx,ny) = (canvas.width / length, canvas.height / length)
    val (dx,dy) = (length, length)
    val params = MazeParams(nx,ny)
    val abstractRenderInfo = generateKruskal(params)
    val renderInfo = abstractRenderInfo.map{
      case((x1,y1),(x2,y2)) => ((x1*dx,y1*dy),(x2*dx,y2*dy))
    }
    //draw maze boundary
    val (bl_x, bl_y, tr_x, tr_y) = (0, 0, canvas.width, canvas.height)
    ctx.lineWidth = 5
    ctx.strokeStyle = "green"
    ctx.beginPath()
    ctx.moveTo(bl_x,bl_y)
    ctx.lineTo(tr_x,bl_y)
    ctx.lineTo(tr_x,tr_y)
    ctx.lineTo(bl_x,tr_y)
    ctx.lineTo(bl_x,bl_y)
    ctx.stroke()

    //draw maze itself
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
