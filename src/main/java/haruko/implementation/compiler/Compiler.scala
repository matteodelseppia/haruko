package haruko.implementation.compiler

import haruko.implementation.compiler.CompilerExceptions.{AlreadyDefinedLocalVariable, AlreadyDefinedMethod, UnboundVariable}
import haruko.lang.Core

import java.util.logging.Logger
import java.util.logging.Level
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

object CompilerExceptions {
  case class WrongArityException(msg: String) extends Exception(msg)
  case class UnknownFunction(msg: String) extends Exception(msg)
  case class UnboundVariable(msg: String) extends Exception(msg)
  case class AlreadyDefinedMethod(msg: String) extends Exception(msg)
  case class AlreadyDefinedLocalVariable(msg: String) extends Exception(msg)
}

class Compiler(val name: String, val program: List[Expression]) extends Visitor {
  private val logger = Logger.getLogger("IR Code")
  private val logLevel = Level.ALL
  private val ASMWriter = new ASMWriter(name, this)
  private val global_variables: mutable.HashSet[String] = mutable.HashSet.empty
  private var local_variables: immutable.HashMap[String, Int] = immutable.HashMap.empty

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
      e.accept(this, new Environment(e.arguments.size))
      ASMWriter.endMethod()
    })


  //then initialize methods
  //absent for the moment

  logger.log(logLevel, "CODE START")
  ASMWriter.initMain()
  program.filter(!_.isInstanceOf[DefnExpression]).foreach(_.accept(this, new Environment(1)))
  ASMWriter.endClass()

  def getCode: Array[Byte] = {
    ASMWriter.getCode
  }

  override def visitConst(e: ConstExpression, env: Environment): Unit = {
    logger.log(logLevel, "PUSH " + e.constant.value)
    ASMWriter.pushConst(e.constant.value)
  }

  override def visitSymbol(e: SymExpression, env: Environment): Unit = {
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

  override def visitDef(e: DefExpression, env: Environment): Unit = {
    e.assignedValue.accept(this, env)
    ASMWriter.putStatic(e.variableName.value.asInstanceOf[String])
    logger.log(logLevel, "PUTSTATIC " + e.variableName.value)
  }

  override def visitIf(e: IfExpression, env: Environment): Unit = {
    val old_local_variables = local_variables
    e.condition.accept(this, env)
    local_variables = old_local_variables
    ASMWriter.branch(e.ifTrue, e.ifFalse, env)
  }

  override def visitFnCall(e: FnCallExpression, env: Environment): Unit = {
    val functionName = e.functionName.value.asInstanceOf[String]
    var variadic = false
    var num_args = e.arguments.size
    if (functionName.endsWith("$")) {
      ASMWriter.initArray(num_args)
      variadic = true
      num_args = 1
      e.arguments.zipWithIndex.foreach({ case (e, i) =>
        ASMWriter.prepareToStoreInArray(i)
        e.accept(this, env)
        ASMWriter.storeInArray()
      })
    } else e.arguments.foreach(_.accept(this, env))

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

  override def visitDefn(e: DefnExpression, env: Environment): Unit = {
    e.arguments.zipWithIndex.foreach {
      case (arg: String, i: Int) =>
        local_variables += (arg -> i)
    }
    
    e.body.accept(this, env)
    local_variables = immutable.HashMap.empty
  }

  override def visitLet(e: LetExpression, env: Environment): Unit = {
    val identifier = e.variableName.value.asInstanceOf[String]
    local_variables.get(identifier) match {
      case Some(x) => throw AlreadyDefinedLocalVariable("Cannot assign already existing variable " + identifier)
      case None =>
        val index = local_variables.size
        local_variables += (identifier -> index)
        e.binding.accept(this, new Environment(index + 1))
        ASMWriter.storeLocal(index)
    }
  }

  override def visitDo(e: DoExpression, env: Environment): Unit = {
    val old_locals = local_variables
    e.expressions.zipWithIndex.filter({
      case (ex, i) if ((ex.isInstanceOf[ConstExpression] || ex.isInstanceOf[SymExpression])
        && i + 1 != e.expressions.size) => false
      case _ => true
    }).foreach(_._1.accept(this, env))

    local_variables = old_locals
  }
}
