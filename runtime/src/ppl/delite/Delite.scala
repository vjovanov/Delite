package ppl.delite

import io.Config
import runtime.executor._
import walktime.graph.DeliteTaskGraph
import walktime.graph.ops.DeliteOP
import walktime.scheduler._

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 5:02:38 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Delite {


  private def printArgs(args: Array[String]) {
    if (args.length != 0) {
      println("This program does not take input")
      exit(-1)
    }
  }

  def main(args: Array[String]) {
    printArgs(args)

    val scheduler = Config.scheduler match {
      case "SMPStaticScheduler" => new SMPStaticScheduler
      case "default" => new SMPStaticScheduler
      case _ => throw new IllegalArgumentException("Requested scheduler is not recognized")
    }

    val executor = Config.executor match {
      case "SMPExecutor" => new SMPExecutor
      case "default" => new SMPExecutor
      case _ => throw new IllegalArgumentException("Requested executor type is not recognized")
    }

    //load task graph
    //TODO: this is a compile hack
    val graph = new DeliteTaskGraph {
      val node = new TestOP
      def root = node 
    }

    //load kernels

    //load data structures

    //schedule
    val schedule = scheduler.schedule(graph)

    //execute
    executor.run(schedule)
    
  }

  private class TestOP extends DeliteOP {
    def task { }
    def getDependencies = Seq()
    def getConsumers = Seq()
    def nested = null
    def cost = 0
    def size = 0
    def isDataParallel = false   
  }

}