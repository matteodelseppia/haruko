package haruko.implementation.compiler

import haruko.implementation.lexer.Lexeme
import org.objectweb.asm
import org.objectweb.asm.{ClassWriter, FieldVisitor, Label, MethodVisitor, Opcodes}

import java.lang.reflect.Method

object Access {
  val PUBLIC_FINAL: Int = Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL
  val PRIVATE_STATIC: Int = Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC
  val PUBLIC_STATIC: Int = Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC
}

object Descriptors {
  val L_OBJECT: String = "Ljava/lang/Object;"
  val OBJECT: String = "java/lang/Object"
  val MAIN: String = "([Ljava/lang/String;)V"
  val LONG: String = "java/lang/Long"
  val L_LONG: String = "Ljava/lang/Long;"
  val DOUBLE: String = "java/lang/Double"
  val L_DOUBLE: String = "Ljava/lang/Double;"
  val BOOLEAN: String = "java/lang/Boolean"
  val L_BOOLEAN: String = "Ljava/lang/Boolean;"
  val TYPE_DOUBLE: String = "D"
  val TYPE_LONG: String = "J"
  val TYPE_BOOLEAN: String = "Z"
  val TYPE_INT: String = "I"
}

object Methods {
  val valueOf: String = "valueOf"
  val valueOfLong: String = "(" + Descriptors.TYPE_LONG + ")" + Descriptors.L_LONG
  val valueOfDouble: String = "(" + Descriptors.TYPE_DOUBLE + ")" + Descriptors.L_DOUBLE
  val valueOfBoolean: String = "(" + Descriptors.TYPE_BOOLEAN + ")" + Descriptors.L_BOOLEAN
}

class ASMWriter(val className: String, val compiler: Compiler) {
  private val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  private var mw: MethodVisitor = _
  private var fw: FieldVisitor = _

  def initClass(name: String) : Unit = {
    cw.visit(Opcodes.V1_8, Access.PUBLIC_FINAL, name, null, Descriptors.OBJECT, null)
  }

  def addField(name: String): Unit = {
    fw = cw.visitField(Access.PRIVATE_STATIC, name, Descriptors.L_OBJECT, null, null)
    fw.visitEnd()
    fw = null
  }

  def putStatic(field: String) : Unit = {
    mw.visitFieldInsn(Opcodes.PUTSTATIC, className, field, Descriptors.L_OBJECT)
  }

  def loadStatic(field: String): Unit = {
    mw.visitFieldInsn(Opcodes.GETSTATIC, className, field, Descriptors.L_OBJECT)
  }
  
  def loadLocal(index: Int) : Unit = {
    mw.visitIntInsn(Opcodes.ALOAD, index)
  }

  def push(obj: Object) : Unit = {
    mw.visitLdcInsn(obj)
  }

  def pop() : Unit = {
    mw.visitInsn(Opcodes.POP)
  }

  def initArray(size: Int): Unit = {
    mw.visitIntInsn(Opcodes.BIPUSH, size)
    mw.visitTypeInsn(Opcodes.ANEWARRAY, Descriptors.OBJECT)
  }

  def callMethod(method: Method, variadic: Boolean): Unit = {
    val signature = {
      if (variadic)
        "([" + Descriptors.L_OBJECT + ")" + Descriptors.L_OBJECT
      else
        "(" + Descriptors.L_OBJECT*method.getParameterCount + ")" + Descriptors.L_OBJECT
    }

    mw.visitMethodInsn(Opcodes.INVOKESTATIC, "haruko/lang/Core", method.getName, signature, false)
  }

  def callLocalMethod(method: String, num_args: Int): Unit = {
    val signature = "(" + Descriptors.L_OBJECT * num_args + ")" + Descriptors.L_OBJECT
    mw.visitMethodInsn(Opcodes.INVOKESTATIC, className, method, signature, false)
  }
  
  def beginMethod(name: String, num_args: Int): Unit = {
    mw = cw.visitMethod(
      Access.PRIVATE_STATIC, 
      name,
      "(" + Descriptors.L_OBJECT*num_args + ")" + Descriptors.L_OBJECT, 
      null, 
      null)
  }
  
  def endMethod() : Unit = {
    mw.visitInsn(Opcodes.ARETURN)
    mw.visitMaxs(0,0)
    mw.visitEnd()
  }

  def branch(ifTrue: Expression, ifFalse: Expression) : Unit = {
    mw.visitTypeInsn(Opcodes.CHECKCAST, Descriptors.BOOLEAN)
    mw.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      "haruko/implementation/Runtime",
      "unboxBoolean",
      "(" + Descriptors.L_BOOLEAN + ")" + "B",
      false)

    mw.visitInsn(Opcodes.ICONST_1)
    val skipFalse = new Label
    val skipTrue = new Label
    mw.visitJumpInsn(Opcodes.IF_ICMPEQ, skipFalse)
    ifFalse.accept(compiler)
    mw.visitJumpInsn(Opcodes.GOTO, skipTrue)
    mw.visitLabel(skipFalse)
    ifTrue.accept(compiler)
    mw.visitLabel(skipTrue)
  }

  def unboxBoolean(a: Boolean) : Int = {
    if (a) 1
    else 0
  }

  def pushConst(obj: Object) : Unit = {
    mw.visitLdcInsn(obj)
    obj match {
      case o if o.isInstanceOf[Long] =>
        mw.visitMethodInsn(Opcodes.INVOKESTATIC, Descriptors.LONG, Methods.valueOf, Methods.valueOfLong, false)
      case o if o.isInstanceOf[Double] =>
        mw.visitMethodInsn(Opcodes.INVOKESTATIC, Descriptors.DOUBLE, Methods.valueOf, Methods.valueOfDouble, false)
      case o if o.isInstanceOf[Boolean] =>
        mw.visitMethodInsn(Opcodes.INVOKESTATIC, Descriptors.BOOLEAN, Methods.valueOf, Methods.valueOfBoolean, false)
      case _ =>
    }
  }

  def prepareToStoreInArray(pos: Int) : Unit = {
    mw.visitInsn(Opcodes.DUP)
    mw.visitLdcInsn(pos)
  }

  def storeInArray() : Unit = {
    mw.visitInsn(Opcodes.AASTORE)
  }

  def initMain() : Unit = {
    mw = cw.visitMethod(Access.PUBLIC_STATIC, "main", Descriptors.MAIN, null, null)
  }

  def endClass() : Unit = {
    mw.visitInsn(Opcodes.RETURN)
    mw.visitMaxs(0, 0)
    mw.visitEnd()
    cw.visitEnd()
  }

  def getCode: Array[Byte] = {
    cw.toByteArray
  }
}
