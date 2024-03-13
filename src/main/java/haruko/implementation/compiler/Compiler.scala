package haruko.implementation.compiler

import haruko.lang.Core

import java.util.logging.Logger
import java.util.logging.Level

object CompilerExceptions {
  case class WrongArityException(msg: String) extends Exception(msg)
  case class UnknownFunction(msg: String) extends Exception(msg)
}

class Compiler(val name: String, val program: List[Expression]) extends Visitor {
  private val logger = Logger.getLogger("IR Code")
  private val logLevel = Level.INFO
  private val ASMWriter = new ASMWriter(name, this)
  private var currentStackSize = 0
  //first initialize global variables
  ASMWriter.initClass(name)
  program
    .filter(_.isInstanceOf[DefExpression])
    .asInstanceOf[List[DefExpression]]
    .foreach(e => {
      logger.log(logLevel, "FOUND DEF OF " + e.variableName.value)
      ASMWriter.addField(e.variableName.value.asInstanceOf[String])
    })

  //then initialize methods
  //absent for the moment

  logger.log(logLevel, "CODE START")
  ASMWriter.initMain()
  program.foreach(_.accept(this))
  ASMWriter.endClass()

  def getCode: Array[Byte] = {
    ASMWriter.getCode
  }

  override def visitConst(e: ConstExpression): Unit = {
    logger.log(logLevel, "PUSH " + e.constant.value)
    ASMWriter.pushConst(e.constant.value)
  }

  override def visitSymbol(e: SymExpression): Unit = {

  }

  override def visitDef(e: DefExpression): Unit = {
    e.assignedValue.accept(this)
    ASMWriter.putStatic(e.variableName.value.asInstanceOf[String])
    logger.log(logLevel, "PUTSTATIC " + e.variableName.value)
  }

  override def visitIf(e: IfExpression): Unit = {
    e.condition.accept(this)
    ASMWriter.branch(e.ifTrue, e.ifFalse)
  }

  override def visitFnCall(e: FnCallExpression): Unit = {
    val functionName = e.functionName.value.asInstanceOf[String]
    var variadic = false
    var num_args = e.arguments.size
    if (functionName.endsWith("$")) {
      ASMWriter.initArray(num_args)
      variadic = true
      num_args = 1
      e.arguments.zipWithIndex.foreach({ case (e, i) =>
        ASMWriter.prepareToStoreInArray(i)
        e.accept(this)
        ASMWriter.storeInArray()
      })
    } else e.arguments.foreach(_.accept(this))

    val method = Core.getClass.getMethods.find(_.getName == functionName)
    method match {
      case None => throw CompilerExceptions.WrongArityException("Call to unknown function: " + functionName)
      case _ =>
    }

    logger.log(logLevel, "CALL " + method.get)
    method.get.getParameterCount match {
      case x if x == num_args => ASMWriter.callMethod(method.get, variadic)
      case _ => throw CompilerExceptions.WrongArityException("Wrong arity when calling function: " + functionName)
    }
  }

  override def visitLet(e: LetExpression): Unit = {

  }

  override def visitCond(e: CondExpression): Unit = {

  }

  override def visitDo(e: DoExpression): Unit = {

  }

  override def visitDefn(e: DefnExpression): Unit = {

  }
}
