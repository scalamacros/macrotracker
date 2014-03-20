package scala.tools.nsc.palladium
package contexts

import scala.reflect.macros.whitebox.{Context => NscContext}
import scala.tools.nsc.palladium.{Context => PalladiumContext}

trait Root extends NscContext with Universes with Trees with Symbols with Types with Internals {
  self: Context =>
  import universe._

  val prefix: OurExpr[PrefixType] = c.prefix.asInstanceOf[CompilerExpr[PrefixType]].wrap
  def macroApplication: OurTree = c.macroApplication.wrap
  val mirror: OurMirror = universe.rootMirror

  def Expr[T: OurWeakTypeTag](tree: OurTree): OurExpr[T] = universe.createExpr[T](tree)
  def WeakTypeTag[T](tpe: OurType): OurWeakTypeTag[T] = universe.createWeakTypeTag[T](tpe)
  def TypeTag[T](tpe: OurType): OurTypeTag[T] = universe.createTypeTag[T](tpe)

  type CompilerImplicitCandidate = c.ImplicitCandidate
  type OurImplicitCandidate = self.ImplicitCandidate
  implicit class RichCompilerImplicitCandidate(ic: CompilerImplicitCandidate) { def wrap = new OurImplicitCandidate(ic.pre.wrap, ic.sym.wrap, ic.pt.wrap, ic.tree.wrap) }
  implicit class RichOurImplicitCandidate(ic: OurImplicitCandidate) { def unwrap = new CompilerImplicitCandidate(ic.pre.unwrap, ic.sym.unwrap, ic.pt.unwrap, ic.tree.unwrap) }
  def enclosingImplicits: List[OurImplicitCandidate] = c.enclosingImplicits.map(_.wrap)
  def openImplicits: List[OurImplicitCandidate] = c.openImplicits.map(_.wrap)
  type CompilerContext = scala.reflect.macros.whitebox.Context
  type OurContext = scala.tools.nsc.palladium.Context
  implicit class RichCompilerContext(m: CompilerContext) { def wrap = new OurContext(c) }
  def enclosingMacros: List[Context] = c.enclosingMacros.map(_.wrap)
  def openMacros: List[Context] = c.openMacros.map(_.wrap)

  def enclosingClass: OurTree = c.enclosingClass.wrap
  def enclosingDef: self.universe.DefDef = c.enclosingDef.wrap
  def enclosingImpl: self.universe.ImplDef = c.enclosingImpl.wrap
  def enclosingMethod: self.universe.Tree = c.enclosingMethod.wrap
  def enclosingPackage: self.universe.PackageDef = c.enclosingPackage.wrap
  def enclosingPosition: OurPosition = c.enclosingPosition.wrap
  def enclosingRun: OurRun = c.enclosingRun.wrap
  def enclosingTemplate: self.universe.Template = c.enclosingTemplate.wrap
  def enclosingUnit: OurCompilationUnit = c.enclosingUnit.wrap

  def eval[T](x: OurExpr[T]): T = c.eval(x.unwrap)

  def literal(x: Char): OurExpr[Char] = c.literal(x).wrap
  def literal(x: String): OurExpr[String] = c.literal(x).wrap
  def literal(x: Double): OurExpr[Double] = c.literal(x).wrap
  def literal(x: Float): OurExpr[Float] = c.literal(x).wrap
  def literal(x: Long): OurExpr[Long] = c.literal(x).wrap
  def literal(x: Int): OurExpr[Int] = c.literal(x).wrap
  def literal(x: Short): OurExpr[Short] = c.literal(x).wrap
  def literal(x: Byte): OurExpr[Byte] = c.literal(x).wrap
  def literal(x: Boolean): OurExpr[Boolean] = c.literal(x).wrap
  def literalFalse: OurExpr[Boolean] = c.literalFalse.wrap
  def literalNull: OurExpr[Null] = c.literalNull.wrap
  def literalTrue: OurExpr[Boolean] = c.literalTrue.wrap
  def literalUnit: OurExpr[Unit] = c.literalUnit.wrap

  def abort(pos: OurPosition, msg: String): Nothing = c.abort(pos.unwrap, msg)
  def echo(pos: OurPosition, msg: String): Unit = c.echo(pos.unwrap, msg)
  def error(pos: OurPosition, msg: String): Unit = c.error(pos.unwrap, msg)
  def hasErrors: Boolean = c.hasErrors
  def hasWarnings: Boolean = c.hasWarnings
  def info(pos: OurPosition, msg: String, force: Boolean): Unit = c.info(pos, msg, force)
  def warning(pos: OurPosition, msg: String): Unit = c.warning(pos.unwrap, msg)

  def classPath: List[java.net.URL] = c.classPath
  def compilerSettings: List[String] = c.compilerSettings
  def settings: List[String] = c.settings

  def fresh(): String = c.fresh()
  def fresh(name: String): String = c.fresh(name)
  def fresh[NameType <: Name](name: NameType): NameType = c.fresh(name.unwrap).wrap.asInstanceOf[NameType]
  def freshName(): String = c.freshName()
  def freshName(name: String): String = c.freshName(name)
  def freshName[NameType <: Name](name: NameType): NameType = c.freshName(name.unwrap).wrap.asInstanceOf[NameType]

  def parse(code: String): OurTree = c.parse(code).wrap

  def reifyEnclosingRuntimeClass: OurTree = c.reifyEnclosingRuntimeClass.wrap
  def reifyRuntimeClass(tpe: OurType, concrete: Boolean): OurTree = c.reifyRuntimeClass(tpe.unwrap, concrete).wrap
  def reifyTree(universe: OurTree, mirror: OurTree, tree: OurTree): OurTree = c.reifyTree(universe.unwrap, mirror.unwrap, tree.unwrap).wrap
  def reifyType(universe: OurTree, mirror: OurTree, tpe: OurType, concrete: Boolean): OurTree = c.reifyType(universe.unwrap, mirror.unwrap, tpe.unwrap, concrete).wrap
  def unreifyTree(tree: OurTree): OurTree = c.unreifyTree(tree.unwrap).wrap

  type TypecheckMode = c.TypecheckMode
  val PATTERNmode = c.PATTERNmode
  val TERMmode = c.TERMmode
  val TYPEmode = c.TYPEmode
  def inferImplicitValue(pt: OurType, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: OurPosition = enclosingPosition): OurTree = c.inferImplicitValue(pt.unwrap, silent, withMacrosDisabled, pos.unwrap).wrap
  def inferImplicitView(tree: OurTree, from: OurType, to: OurType, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: OurPosition = enclosingPosition): OurTree = c.inferImplicitView(tree.unwrap, from.unwrap, to.unwrap, silent, withMacrosDisabled, pos.unwrap).wrap
  def typecheck(tree: OurTree, mode: TypecheckMode, pt: OurType, silent: Boolean, withImplicitViewsDisabled: Boolean, withMacrosDisabled: Boolean): OurTree = c.typecheck(tree.unwrap, mode, pt.unwrap, silent, withImplicitViewsDisabled, withMacrosDisabled).wrap
  def resetLocalAttrs(tree: OurTree): OurTree = c.resetLocalAttrs(tree.unwrap).wrap
  def untypecheck(tree: OurTree): OurTree = c.untypecheck(tree.unwrap).wrap
}