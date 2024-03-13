package haruko.implementation.compiler

import haruko.implementation.compiler.CompilerExceptions.{AlreadyDefinedMethod, UnboundVariable}
import haruko.lang.Core

import java.util.logging.Logger
import java.util.logging.Level
import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

object CompilerExceptions {
  case class WrongArityException(msg: String) extends Exception(msg)
  case class UnknownFunction(msg: String) extends Exception(msg)
  case class UnboundVariable(msg: String) extends Exception(msg)
  case class AlreadyDefinedMethod(msg: String) extends Exception(msg)
}

class Compiler(val name: String, val program: List[Expression]) extends Visitor {
  private val logger = Logger.getLogger("IR Code")
  private val logLevel = Level.ALL
  private val ASMWriter = new ASMWriter(name, this)
  private val global_variables: mutable.HashSet[String] = mutable.HashSet.empty
  private var local_variables: mutable.HashMap[String, Int] = mutable.HashMap.empty
  private val methods: mutable.HashMap[String, Int] = mutable.HashMap.empty
  ASMWriter.initClass(name)
  program
    .filter(_.isInstanceOf[DefExpression])
    .asInstanceOf[List[DefExpression]]
    .foreach(e => {
      logger.log(logLevel, "FOUND DEF OF " + e.variableName.value)
      global_variables.addOne(e.variableName.value.asInstanceOf[String])
    })

  global_variables.foreach(s => {
    ASMWriter.addField(s)
  })

  program
    .filter(_.isInstanceOf[DefnExpression])
    .asInstanceOf[List[DefnExpression]]
    .foreach(e => {
      logger.log(logLevel, "FOUND DEFN OF " + e.functionName.value)
      val functionName = e.functionName.value.asInstanceOf[String]
      if (methods.contains(functionName))
        throw AlreadyDefinedMethod("method already defined: " + functionName)
      methods.addOne(functionName, e.arguments.size)
      ASMWriter.beginMethod(functionName, e.arguments.size)
      e.accept(this)
      ASMWriter.endMethod()
    })


  //then initialize methods
  //absent for the moment

  logger.log(logLevel, "CODE START")
  ASMWriter.initMain()
  program.filter(!_.isInstanceOf[DefnExpression]).foreach(_.accept(this))
  ASMWriter.endClass()

  def getCode: Array[Byte] = {
    ASMWriter.getCode
  }

  override def visitConst(e: ConstExpression): Unit = {
    logger.log(logLevel, "PUSH " + e.constant.value)
    ASMWriter.pushConst(e.constant.value)
  }

  override def visitSymbol(e: SymExpression): Unit = {
    val sym = e.symbol.value.asInstanceOf[String]
    local_variables.get(sym) match {
      case Some(x) => ASMWriter.loadLocal(x)
      case None =>
        if (global_variables.contains(sym))
          ASMWriter.loadStatic(sym)
        else
          throw UnboundVariable("Use of unbound variable: " + sym)
    }
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
      case None =>
        if (!methods.contains(functionName))
          throw CompilerExceptions.UnknownFunction("Call to unknown function: " + functionName)
        else if (num_args != methods(functionName)) {
            throw CompilerExceptions.WrongArityException("Call to unknown function: " + functionName)
        } else {
          ASMWriter.callLocalMethod(functionName, num_args)
          return
        }
      case Some(x) =>
    }

    logger.log(logLevel, "CALL " + method.get)
    method.get.getParameterCount match {
      case x if x == num_args => ASMWriter.callMethod(method.get, variadic)
      case _ => throw CompilerExceptions.WrongArityException("Wrong arity when calling function: " + functionName)
    }
  }

  override def visitDefn(e: DefnExpression): Unit = {
    e.arguments.zipWithIndex.foreach {
      case (arg: String, i: Int) => local_variables.put(arg, i)
    }
    
    e.body.accept(this)
    local_variables.clear()
  }

  override def visitLet(e: LetExpression): Unit = {

  }

  override def visitCond(e: CondExpression): Unit = {

  }

  override def visitDo(e: DoExpression): Unit = {

  }
}
