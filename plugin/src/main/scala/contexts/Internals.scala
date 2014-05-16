package scala.tools.nsc.macrotracker
package contexts

import scala.language.implicitConversions
import scala.reflect.macros.Universe
import scala.reflect.ClassTag

trait Internals {
  self: Context =>

  trait UniverseInternals {
    self: universe.type =>

    type Internal = MacroInternalApi
    lazy val internal: Internal = new SymbolTableInternal {}

    type Compat = MacroCompatApi
    lazy val compat: Compat = new Compat {}

    type OurAttachments = AdaptingAttachments { type Pos = OurPosition }
    type CompilerAttachments = scala.reflect.macros.Attachments { type Pos = CompilerPosition }
    implicit class RichCompilerAttachments(att: CompilerAttachments) { def wrap = new AdaptingAttachments(att) }
    implicit class RichOurAttachments(att: OurAttachments) { def unwrap = att.att }
    class AdaptingAttachments(val att: CompilerAttachments) extends scala.reflect.macros.Attachments {
      type Pos = OurPosition
      def pos: Pos = att.pos.wrap
      def withPos(newPos: Pos): AdaptingAttachments = att.withPos(pos.unwrap).wrap
      // TODO: reflection artifact-carrying attachments need wrapping/unwrapping
      override def all: Set[Any] = att.all
    }

    trait SymbolTableInternal extends MacroInternalApi {
      lazy val reificationSupport: ReificationSupportApi = self.build

      // TODO: too much of a hassle, leaving for future work
      def createImporter(from0: scala.reflect.api.Universe): Importer { val from: from0.type } = ???

      def newScopeWith(elems: Symbol*): Scope = c.universe.internal.newScopeWith(elems.toList.map(_.unwrap): _*).wrap
      def enter(scope: Scope, sym: Symbol): scope.type = c.universe.internal.enter(scope.unwrap, sym.unwrap).wrap.asInstanceOf[scope.type]
      def unlink(scope: Scope, sym: Symbol): scope.type = c.universe.internal.unlink(scope.unwrap, sym.unwrap).wrap.asInstanceOf[scope.type]

      def freeTerms(tree: Tree): List[FreeTermSymbol] = c.universe.internal.freeTerms(tree.unwrap).map(_.wrap)
      def freeTypes(tree: Tree): List[FreeTypeSymbol] = c.universe.internal.freeTypes(tree.unwrap).map(_.wrap)
      def substituteSymbols(tree: Tree, from: List[Symbol], to: List[Symbol]): Tree = c.universe.internal.substituteSymbols(tree.unwrap, from.map(_.unwrap), to.map(_.unwrap)).wrap
      def substituteTypes(tree: Tree, from: List[Symbol], to: List[Type]): Tree = c.universe.internal.substituteTypes(tree.unwrap, from.map(_.unwrap), to.map(_.unwrap)).wrap
      def substituteThis(tree: Tree, clazz: Symbol, to: Tree): Tree = c.universe.internal.substituteThis(tree.unwrap, clazz.unwrap, to.unwrap).wrap
      def attachments(tree: Tree): OurAttachments = c.universe.internal.attachments(tree.unwrap).wrap
      def updateAttachment[T: ClassTag](tree: Tree, attachment: T): tree.type = { attachments(tree).update(attachment); tree }
      def removeAttachment[T: ClassTag](tree: Tree): tree.type = { attachments(tree).remove[T]; tree }
      def setPos(tree: Tree, newpos: Position): tree.type = c.universe.internal.setPos(tree.unwrap, newpos.unwrap).wrap.asInstanceOf[tree.type]
      def setType(tree: Tree, tp: Type): tree.type = c.universe.internal.setType(tree.unwrap, tp.unwrap).wrap.asInstanceOf[tree.type]
      def defineType(tree: Tree, tp: Type): tree.type = c.universe.internal.defineType(tree.unwrap, tp.unwrap).wrap.asInstanceOf[tree.type]
      def setSymbol(tree: Tree, sym: Symbol): tree.type = c.universe.internal.setSymbol(tree.unwrap, sym.unwrap).wrap.asInstanceOf[tree.type]
      def setOriginal(tt: TypeTree, tree: Tree): TypeTree = c.universe.internal.setOriginal(tt.unwrap, tree.unwrap).wrap

      def captureVariable(vble: Symbol): Unit = c.universe.internal.captureVariable(vble.unwrap)
      def referenceCapturedVariable(vble: Symbol): Tree = c.universe.internal.referenceCapturedVariable(vble.unwrap).wrap
      def capturedVariableType(vble: Symbol): Type = c.universe.internal.capturedVariableType(vble.unwrap).wrap

      def classDef(sym: Symbol, impl: Template): ClassDef = c.universe.internal.classDef(sym.unwrap, impl.unwrap).wrap
      def moduleDef(sym: Symbol, impl: Template): ModuleDef = c.universe.internal.moduleDef(sym.unwrap, impl.unwrap).wrap
      def valDef(sym: Symbol, rhs: Tree): ValDef = c.universe.internal.valDef(sym.unwrap, rhs.unwrap).wrap
      def valDef(sym: Symbol): ValDef = c.universe.internal.valDef(sym.unwrap).wrap
      def defDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef = c.universe.internal.defDef(sym.unwrap, mods.unwrap, vparamss.map(_.map(_.unwrap)), rhs.unwrap).wrap
      def defDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef = c.universe.internal.defDef(sym.unwrap, vparamss.map(_.map(_.unwrap)), rhs.unwrap).wrap
      def defDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef = c.universe.internal.defDef(sym.unwrap, mods.unwrap, rhs.unwrap).wrap
      def defDef(sym: Symbol, rhs: Tree): DefDef = c.universe.internal.defDef(sym.unwrap, rhs.unwrap).wrap
      def defDef(sym: Symbol, rhs: List[List[OurSymbol]] => Tree): DefDef = c.universe.internal.defDef(sym.unwrap, syms => rhs(syms.map(_.map(_.wrap))).unwrap).wrap
      def typeDef(sym: Symbol, rhs: Tree): TypeDef = c.universe.internal.typeDef(sym.unwrap, rhs.unwrap).wrap
      def typeDef(sym: Symbol): TypeDef = c.universe.internal.typeDef(sym.unwrap).wrap
      def labelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef = c.universe.internal.labelDef(sym.unwrap, params.map(_.unwrap), rhs.unwrap).wrap
      def changeOwner(tree: Tree, prev: Symbol, next: Symbol): tree.type = c.universe.internal.changeOwner(tree.unwrap, prev.unwrap, next.unwrap).wrap.asInstanceOf[tree.type]

      lazy val gen = self.treeBuild

      def isFreeTerm(symbol: Symbol): Boolean = c.universe.internal.isFreeTerm(symbol.unwrap)
      def asFreeTerm(symbol: Symbol): FreeTermSymbol = c.universe.internal.asFreeTerm(symbol.unwrap).wrap
      def isFreeType(symbol: Symbol): Boolean = c.universe.internal.isFreeType(symbol.unwrap)
      def asFreeType(symbol: Symbol): FreeTypeSymbol = c.universe.internal.asFreeType(symbol.unwrap).wrap
      def newTermSymbol(symbol: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol = c.universe.internal.newTermSymbol(symbol.unwrap, name.unwrap, pos.unwrap, flags).wrap
      def newModuleAndClassSymbol(symbol: Symbol, name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol) = {
        val (msym, csym) = c.universe.internal.newModuleAndClassSymbol(symbol.unwrap, name.unwrap, pos.unwrap, flags.unwrap)
        (msym.wrap, csym.wrap)
      }
      def newMethodSymbol(symbol: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol = c.universe.internal.newMethodSymbol(symbol.unwrap, name.unwrap, pos.unwrap, flags.unwrap).wrap
      def newTypeSymbol(symbol: Symbol, name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol = c.universe.internal.newTypeSymbol(symbol.unwrap, name.unwrap, pos.unwrap, flags.unwrap).wrap
      def newClassSymbol(symbol: Symbol, name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol = c.universe.internal.newClassSymbol(symbol.unwrap, name.unwrap, pos.unwrap, flags.unwrap).wrap
      def newFreeTerm(name: String, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTermSymbol = c.universe.internal.newFreeTerm(name, value, flags.unwrap, origin).wrap
      def newFreeType(name: String, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol = c.universe.internal.newFreeType(name, flags.unwrap, origin).wrap
      def isErroneous(symbol: Symbol): Boolean = c.universe.internal.isErroneous(symbol.unwrap)
      def isSkolem(symbol: Symbol): Boolean = c.universe.internal.isSkolem(symbol.unwrap)
      def deSkolemize(symbol: Symbol): Symbol = c.universe.internal.deSkolemize(symbol.unwrap).wrap
      def initialize(symbol: Symbol): symbol.type = c.universe.internal.initialize(symbol.unwrap).wrap.asInstanceOf[symbol.type]
      def fullyInitialize(symbol: Symbol): symbol.type = c.universe.internal.fullyInitialize(symbol.unwrap).wrap.asInstanceOf[symbol.type]
      def fullyInitialize(tp: Type): tp.type = c.universe.internal.fullyInitialize(tp.unwrap).wrap.asInstanceOf[tp.type]
      def fullyInitialize(scope: Scope): scope.type = c.universe.internal.fullyInitialize(scope.unwrap).wrap.asInstanceOf[scope.type]
      def flags(symbol: Symbol): FlagSet = c.universe.internal.flags(symbol.unwrap).wrap
      def attachments(symbol: Symbol): OurAttachments = c.universe.internal.attachments(symbol.unwrap).wrap
      def updateAttachment[T: ClassTag](symbol: Symbol, attachment: T): symbol.type = { attachments(symbol).update(attachment); symbol }
      def removeAttachment[T: ClassTag](symbol: Symbol): symbol.type = { attachments(symbol).remove[T]; symbol }
      def setOwner(symbol: Symbol, newowner: Symbol): symbol.type = c.universe.internal.setOwner(symbol.unwrap, newowner.unwrap).wrap.asInstanceOf[symbol.type]
      def setInfo(symbol: Symbol, tpe: Type): symbol.type = c.universe.internal.setInfo(symbol.unwrap, tpe.unwrap).asInstanceOf[symbol.type]
      def setAnnotations(symbol: Symbol, annots: Annotation*): symbol.type = c.universe.internal.setAnnotations(symbol.unwrap, annots.toList.map(_.unwrap): _*).wrap.asInstanceOf[symbol.type]
      def setName(symbol: Symbol, name: Name): symbol.type = c.universe.internal.setName(symbol.unwrap, name.unwrap).wrap.asInstanceOf[symbol.type]
      def setPrivateWithin(symbol: Symbol, sym: Symbol): symbol.type = c.universe.internal.setPrivateWithin(symbol.unwrap, sym.unwrap).wrap.asInstanceOf[symbol.type]
      def setFlag(symbol: Symbol, flags: FlagSet): symbol.type = c.universe.internal.setFlag(symbol.unwrap, flags.unwrap).wrap.asInstanceOf[symbol.type]
      def resetFlag(symbol: Symbol, flags: FlagSet): symbol.type = c.universe.internal.resetFlag(symbol.unwrap, flags.unwrap).wrap.asInstanceOf[symbol.type]

      def thisType(sym: Symbol): Type = c.universe.internal.thisType(sym.unwrap).wrap
      def singleType(pre: Type, sym: Symbol): Type = c.universe.internal.singleType(pre.unwrap, sym.unwrap).wrap
      def superType(thistpe: Type, supertpe: Type): Type = c.universe.internal.superType(thistpe.unwrap, supertpe.unwrap).wrap
      def constantType(value: Constant): ConstantType = c.universe.internal.constantType(value.unwrap).wrap
      def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = c.universe.internal.typeRef(pre.unwrap, sym.unwrap, args.map(_.unwrap)).wrap
      def refinedType(parents: List[Type], decls: Scope): RefinedType = c.universe.internal.refinedType(parents.map(_.unwrap), decls.unwrap).wrap
      def refinedType(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType = c.universe.internal.refinedType(parents.map(_.unwrap), decls.unwrap, clazz.unwrap).wrap
      def refinedType(parents: List[Type], owner: Symbol): Type = c.universe.internal.refinedType(parents.map(_.unwrap), owner.unwrap).wrap
      def refinedType(parents: List[Type], owner: Symbol, decls: Scope): Type = c.universe.internal.refinedType(parents.map(_.unwrap), owner.unwrap, decls.unwrap).wrap
      def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos: Position): Type = c.universe.internal.refinedType(parents.map(_.unwrap), owner.unwrap, decls.unwrap, pos.unwrap).wrap
      def intersectionType(tps: List[Type]): Type = c.universe.internal.intersectionType(tps.map(_.unwrap)).wrap
      def intersectionType(tps: List[Type], owner: Symbol): Type = c.universe.internal.intersectionType(tps.map(_.unwrap), owner.unwrap).wrap
      def classInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType = c.universe.internal.classInfoType(parents.map(_.unwrap), decls.unwrap, typeSymbol.unwrap).wrap
      def methodType(params: List[Symbol], resultType: Type): MethodType = c.universe.internal.methodType(params.map(_.unwrap), resultType.unwrap).wrap
      def nullaryMethodType(resultType: Type): NullaryMethodType = c.universe.internal.nullaryMethodType(resultType.unwrap).wrap
      def polyType(typeParams: List[Symbol], resultType: Type): PolyType = c.universe.internal.polyType(typeParams.map(_.unwrap), resultType.unwrap).wrap
      def existentialType(quantified: List[Symbol], underlying: Type): ExistentialType = c.universe.internal.existentialType(quantified.map(_.unwrap), underlying.unwrap).wrap
      def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type = c.universe.internal.existentialAbstraction(tparams.map(_.unwrap), tpe0.unwrap).wrap
      def annotatedType(annots: List[Annotation], underlying: Type): AnnotatedType = c.universe.internal.annotatedType(annots.map(_.unwrap), underlying.unwrap).wrap
      def typeBounds(lo: Type, hi: Type): TypeBounds = c.universe.internal.typeBounds(lo.unwrap, hi.unwrap).wrap
      def boundedWildcardType(bounds: TypeBounds): BoundedWildcardType = c.universe.internal.boundedWildcardType(bounds.unwrap).wrap

      def subpatterns(tree: Tree): Option[List[Tree]] = c.universe.internal.subpatterns(tree.unwrap).map(_.map(_.wrap))

      type Decorators = MacroDecoratorApi
      lazy val decorators: Decorators = new MacroDecoratorApi {
        override type ScopeDecorator[T <: Scope] = MacroScopeDecoratorApi[T]
        override implicit def scopeDecorator[T <: Scope](scope: T): ScopeDecorator[T] = new MacroScopeDecoratorApi[T](scope)
        override type TreeDecorator[T <: Tree] = MacroTreeDecoratorApi[T]
        override implicit def treeDecorator[T <: Tree](tree: T): TreeDecorator[T] = new MacroTreeDecoratorApi[T](tree)
        override type TypeTreeDecorator[T <: TypeTree] = MacroTypeTreeDecoratorApi[T]
        override implicit def typeTreeDecorator[T <: TypeTree](tt: T): TypeTreeDecorator[T] = new MacroTypeTreeDecoratorApi[T](tt)
        override type SymbolDecorator[T <: Symbol] = MacroSymbolDecoratorApi[T]
        override implicit def symbolDecorator[T <: Symbol](symbol: T): SymbolDecorator[T] = new MacroSymbolDecoratorApi[T](symbol)
        override type TypeDecorator[T <: Type] = TypeDecoratorApi[T]
        override implicit def typeDecorator[T <: Type](tp: T): TypeDecorator[T] = new TypeDecoratorApi[T](tp)
      }
    }

    val build: ReificationSupportApi = new ReificationSupportApi {
      def AnnotatedType(annotations: List[Annotation],underlying: Type): AnnotatedType = c.universe.build.AnnotatedType(annotations.map(_.unwrap), underlying.unwrap).wrap
      def BoundedWildcardType(bounds: TypeBounds): BoundedWildcardType = c.universe.build.BoundedWildcardType(bounds.unwrap).wrap
      def ClassInfoType(parents: List[Type],decls: Scope,typeSymbol: Symbol): ClassInfoType = c.universe.build.ClassInfoType(parents.map(_.unwrap), decls.unwrap, typeSymbol.unwrap).wrap
      def ConstantType(value: Constant): ConstantType = c.universe.build.ConstantType(value.unwrap).wrap
      def ExistentialType(quantified: List[Symbol],underlying: Type): ExistentialType = c.universe.build.ExistentialType(quantified.map(_.unwrap), underlying.unwrap).wrap
      val FlagsRepr: FlagsReprExtractor = new FlagsReprExtractor {
        def apply(value: Long): FlagSet = c.universe.build.FlagsRepr.apply(value)
        def unapply(flags: Long): Some[Long] = c.universe.build.FlagsRepr.unapply(flags)
      }
      val ImplicitParams: ImplicitParamsExtractor = new ImplicitParamsExtractor {
        def apply(paramss: List[List[Tree]], implparams: List[Tree]): List[List[Tree]] = c.universe.build.ImplicitParams.apply(paramss.map(_.map(_.unwrap)), implparams.map(_.unwrap)).map(_.map(_.wrap))
        def unapply(vparamss: List[List[ValDef]]): Some[(List[List[ValDef]], List[ValDef])] = {
          val Some((x, y)) = c.universe.build.ImplicitParams.unapply(vparamss.map(_.map(_.unwrap)))
          Some((x.map(_.map(_.wrap)), y.map(_.wrap)))
        }
      }
      def MethodType(params: List[Symbol],resultType: Type): MethodType = c.universe.build.MethodType(params.map(_.unwrap), resultType.unwrap).wrap
      def NullaryMethodType(resultType: Type): NullaryMethodType = c.universe.build.NullaryMethodType(resultType.unwrap).wrap
      def PolyType(typeParams: List[Symbol],resultType: Type): PolyType = c.universe.build.PolyType(typeParams.map(_.unwrap), resultType.unwrap).wrap
      def RefinedType(parents: List[Type],decls: Scope,typeSymbol: Symbol): RefinedType = c.universe.build.RefinedType(parents.map(_.unwrap), decls.unwrap, typeSymbol.unwrap).wrap
      val ScalaDot: ScalaDotExtractor = new ScalaDotExtractor {
        def apply(name: Name): Tree = c.universe.build.ScalaDot.apply(name.unwrap).wrap
        def unapply(tree: Tree): Option[Name] = c.universe.build.ScalaDot.unapply(tree.unwrap).map(_.wrap)
      }
      def SingleType(pre: Type,sym: Symbol): Type = c.universe.build.SingleType(pre.unwrap, sym.unwrap).wrap
      def SuperType(thistpe: Type,supertpe: Type): Type = c.universe.build.SuperType(thistpe.unwrap, supertpe.unwrap).wrap
      val SyntacticApplied: SyntacticAppliedExtractor = new SyntacticAppliedExtractor {
        def apply(tree: Tree, argss: List[List[Tree]]): Tree = c.universe.build.SyntacticApplied.apply(tree.unwrap, argss.map(_.map(_.unwrap))).wrap
        def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
          val Some((x, y)) = c.universe.build.SyntacticApplied.unapply(tree.unwrap)
          Some((x.wrap, y.map(_.map(_.wrap))))
        }
      }
      val SyntacticAppliedType: SyntacticTypeAppliedExtractor = new SyntacticTypeAppliedExtractor {
        def apply(tree: Tree, targs: List[Tree]): Tree = c.universe.build.SyntacticAppliedType.apply(tree.unwrap, targs.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(Tree, List[Tree])] = c.universe.build.SyntacticAppliedType.unapply(tree.unwrap).map { case (x, y) => (x.wrap, y.map(_.wrap)) }
      }
      val SyntacticAssign: SyntacticAssignExtractor = new SyntacticAssignExtractor {
        def apply(lhs: Tree, rhs: Tree): Tree = c.universe.build.SyntacticAssign.apply(lhs.unwrap, rhs.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, Tree)] = c.universe.build.SyntacticAssign.unapply(tree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
      }
      val SyntacticBlock: SyntacticBlockExtractor = new SyntacticBlockExtractor {
        def apply(stats: List[Tree]): Tree = c.universe.build.SyntacticBlock.apply(stats.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[List[Tree]] = c.universe.build.SyntacticBlock.unapply(tree.unwrap).map { case x => x.map(_.wrap) }
      }
      val SyntacticClassDef: SyntacticClassDefExtractor = new SyntacticClassDefExtractor {
        def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                  constrMods: Modifiers, vparamss: List[List[Tree]],
                  earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef =
          c.universe.build.SyntacticClassDef.apply(mods.unwrap, name.unwrap, tparams.map(_.unwrap),
                                                   constrMods.unwrap, vparamss.map(_.map(_.unwrap)),
                                                   earlyDefs.map(_.unwrap), parents.map(_.unwrap), selfType.unwrap, body.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                         List[Tree], List[Tree], ValDef, List[Tree])] = c.universe.build.SyntacticClassDef.unapply(tree.unwrap).map {
          case (x1, x2, x3, x4, x5, x6, x7, x8, x9) => (x1.wrap, x2.wrap, x3.map(_.wrap), x4.wrap, x5.map(_.map(_.wrap)), x6.map(_.wrap), x7.map(_.wrap), x8.wrap, x9.map(_.wrap))
        }
      }
      val SyntacticDefDef: SyntacticDefDefExtractor = new SyntacticDefDefExtractor {
        def apply(mods: Modifiers, name: TermName, tparams: List[Tree],
                  vparamss: List[List[Tree]], tpt: Tree, rhs: Tree): DefDef = c.universe.build.SyntacticDefDef.apply(mods.unwrap, name.unwrap, tparams.map(_.unwrap), vparamss.map(_.map(_.unwrap)), tpt.unwrap, rhs.unwrap).wrap
        def unapply(tree: Tree): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)] = c.universe.build.SyntacticDefDef.unapply(tree.unwrap).map {
          case (x1, x2, x3, x4, x5, x6) => (x1.wrap, x2.wrap, x3.map(_.wrap), x4.map(_.map(_.wrap)), x5.wrap, x6.wrap)
        }
      }
      val SyntacticEmptyTypeTree: SyntacticEmptyTypeTreeExtractor = new SyntacticEmptyTypeTreeExtractor {
        def apply(): TypeTree = c.universe.build.SyntacticEmptyTypeTree().wrap
        def unapply(tt: TypeTree): Boolean = c.universe.build.SyntacticEmptyTypeTree.unapply(tt.unwrap)
      }
      val SyntacticFilter: SyntacticFilterExtractor = new SyntacticFilterExtractor {
        def apply(test: Tree): Tree = c.universe.build.SyntacticFilter.apply(test.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree)] = c.universe.build.SyntacticFilter.unapply(tree.unwrap).map(_.wrap)
      }
      val SyntacticFor: SyntacticForExtractor = new SyntacticForExtractor {
        def apply(enums: List[Tree], body: Tree): Tree = c.universe.build.SyntacticFor.apply(enums.map(_.unwrap), body.unwrap).wrap
        def unapply(tree: Tree): Option[(List[Tree], Tree)] = c.universe.build.SyntacticFor.unapply(tree.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
      }
      val SyntacticForYield: SyntacticForExtractor = new SyntacticForExtractor {
        def apply(enums: List[Tree], body: Tree): Tree = c.universe.build.SyntacticForYield.apply(enums.map(_.unwrap), body.unwrap).wrap
        def unapply(tree: Tree): Option[(List[Tree], Tree)] = c.universe.build.SyntacticForYield.unapply(tree.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
      }
      val SyntacticFunction: SyntacticFunctionExtractor = new SyntacticFunctionExtractor {
        def apply(params: List[Tree], body: Tree): Function = c.universe.build.SyntacticFunction.apply(params.map(_.unwrap), body.unwrap).wrap
        def unapply(tree: Function): Option[(List[ValDef], Tree)] = c.universe.build.SyntacticFunction.unapply(tree.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
      }
      val SyntacticFunctionType: SyntacticFunctionTypeExtractor = new SyntacticFunctionTypeExtractor {
        def apply(argtpes: List[Tree], restpe: Tree): Tree = c.universe.build.SyntacticFunctionType.apply(argtpes.map(_.unwrap), restpe.unwrap).wrap
        def unapply(tree: Tree): Option[(List[Tree], Tree)] = c.universe.build.SyntacticFunctionType.unapply(tree.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
      }
      val SyntacticTermIdent: SyntacticTermIdentExtractor = new SyntacticTermIdentExtractor {
        def apply(name: TermName, isBackquoted: Boolean = false): Ident = c.universe.build.SyntacticTermIdent.apply(name.unwrap, isBackquoted).wrap
        def unapply(tree: Ident): Option[(TermName, Boolean)] = c.universe.build.SyntacticTermIdent.unapply(tree.unwrap.asInstanceOf[c.universe.Ident]).map { case (x1, x2) => (x1.wrap, x2) }
      }
      val SyntacticTypeIdent: SyntacticTypeIdentExtractor = new SyntacticTypeIdentExtractor {
        def apply(name: TypeName): Ident = c.universe.build.SyntacticTypeIdent.apply(name.unwrap).wrap
        def unapply(tree: Tree): Option[TypeName] = c.universe.build.SyntacticTypeIdent.unapply(tree.unwrap).map(_.wrap)
      }
      val SyntacticImport: SyntacticImportExtractor = new SyntacticImportExtractor {
        def apply(expr: Tree, selectors: List[Tree]): Import = c.universe.build.SyntacticImport.apply(expr.unwrap, selectors.map(_.unwrap)).wrap
        def unapply(imp: Import): Some[(Tree, List[Tree])] = {
          val Some((x1, x2)) = c.universe.build.SyntacticImport.unapply(imp.unwrap)
          Some((x1.wrap, x2.map(_.wrap)))
        }
      }
      val SyntacticMatch: SyntacticMatchExtractor = new SyntacticMatchExtractor {
        def apply(scrutinee: Tree, cases: List[Tree]): Match = c.universe.build.SyntacticMatch.apply(scrutinee.unwrap, cases.map(_.unwrap)).wrap
        def unapply(tree: Match): Option[(Tree, List[CaseDef])] = c.universe.build.SyntacticMatch.unapply(tree.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
      }
      val SyntacticNew: SyntacticNewExtractor = new SyntacticNewExtractor {
        def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): Tree = c.universe.build.SyntacticNew.apply(earlyDefs.map(_.unwrap), parents.map(_.unwrap), selfType.unwrap, body.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])] = c.universe.build.SyntacticNew.unapply(tree.unwrap).map { case (x1, x2, x3, x4) => (x1.map(_.wrap), x2.map(_.wrap), x3.wrap, x4.map(_.wrap)) }
      }
      val SyntacticObjectDef: SyntacticObjectDefExtractor = new SyntacticObjectDefExtractor {
        def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                  parents: List[Tree], selfType: Tree, body: List[Tree]): ModuleDef =
          c.universe.build.SyntacticObjectDef.apply(mods.unwrap, name.unwrap, earlyDefs.map(_.unwrap), parents.map(_.unwrap), selfType.unwrap, body.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])] =
          c.universe.build.SyntacticObjectDef.unapply(tree.unwrap).map { case (x0, x1, x2, x3, x4, x5) => (x0.wrap, x1.wrap, x2.map(_.wrap), x3.map(_.wrap), x4.wrap, x5.map(_.wrap)) }
      }
      val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor = new SyntacticPackageObjectDefExtractor {
        def apply(name: TermName, earlyDefs: List[Tree],
                  parents: List[Tree], selfType: Tree, body: List[Tree]): PackageDef =
          c.universe.build.SyntacticPackageObjectDef.apply(name.unwrap, earlyDefs.map(_.unwrap), parents.map(_.unwrap), selfType.unwrap, body.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(TermName, List[Tree], List[Tree], ValDef, List[Tree])] =
          c.universe.build.SyntacticPackageObjectDef.unapply(tree.unwrap).map { case (x1, x2, x3, x4, x5) => (x1.wrap, x2.map(_.wrap), x3.map(_.wrap), x4.wrap, x5.map(_.wrap)) }
      }
      val SyntacticPartialFunction: SyntacticPartialFunctionExtractor = new SyntacticPartialFunctionExtractor {
        def apply(cases: List[Tree]): Match = c.universe.build.SyntacticPartialFunction.apply(cases.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[List[CaseDef]] = c.universe.build.SyntacticPartialFunction.unapply(tree.unwrap).map(_.map(_.wrap))
      }
      val SyntacticPatDef: SyntacticPatDefExtractor = new SyntacticPatDefExtractor {
        def apply(mods: Modifiers, pat: Tree, tpt: Tree, rhs: Tree): List[ValDef] = c.universe.build.SyntacticPatDef.apply(mods.unwrap, pat.unwrap, tpt.unwrap, rhs.unwrap).map(_.wrap)
      }
      val SyntacticSelectTerm: SyntacticSelectTermExtractor = new SyntacticSelectTermExtractor {
        def apply(qual: Tree, name: TermName): Select = c.universe.build.SyntacticSelectTerm.apply(qual.unwrap, name.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, TermName)] = c.universe.build.SyntacticSelectTerm.unapply(tree.unwrap).map { case (x, y) => (x.wrap, y.wrap) }
      }
      val SyntacticSelectType: SyntacticSelectTypeExtractor = new SyntacticSelectTypeExtractor {
        def apply(qual: Tree, name: TypeName): Select = c.universe.build.SyntacticSelectType.apply(qual.unwrap, name.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, TypeName)] = c.universe.build.SyntacticSelectType.unapply(tree.unwrap).map { case (x, y) => (x.wrap, y.wrap) }
      }
      val SyntacticTraitDef: SyntacticTraitDefExtractor = new SyntacticTraitDefExtractor {
        def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                  earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef =
          c.universe.build.SyntacticTraitDef.apply(mods.unwrap, name.unwrap, tparams.map(_.unwrap), earlyDefs.map(_.unwrap), parents.map(_.unwrap), selfType.unwrap, body.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                         List[Tree], List[Tree], ValDef, List[Tree])] =
          c.universe.build.SyntacticTraitDef.unapply(tree.unwrap).map { case (x1, x2, x3, x4, x5, x6, x7) => (x1.wrap, x2.wrap, x3.map(_.wrap), x4.map(_.wrap), x5.map(_.wrap), x6.wrap, x7.map(_.wrap)) }

      }
      val SyntacticTry: SyntacticTryExtractor = new SyntacticTryExtractor {
        def apply(block: Tree, catches: List[Tree], finalizer: Tree): Try = c.universe.build.SyntacticTry.apply(block.unwrap, catches.map(_.unwrap), finalizer.unwrap).wrap
        def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)] = c.universe.build.SyntacticTry.unapply(tree.unwrap).map { case (x, y, z) => (x.wrap, y.map(_.wrap), z.wrap) }
      }
      val SyntacticTuple: SyntacticTupleExtractor = new SyntacticTupleExtractor {
        def apply(args: List[Tree]): Tree = c.universe.build.SyntacticTuple.apply(args.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[List[Tree]] = c.universe.build.SyntacticTuple.unapply(tree.unwrap).map { case x => x.map(_.wrap) }
      }
      val SyntacticTupleType: SyntacticTupleExtractor = new SyntacticTupleExtractor {
        def apply(args: List[Tree]): Tree = c.universe.build.SyntacticTupleType.apply(args.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[List[Tree]] = c.universe.build.SyntacticTupleType.unapply(tree.unwrap).map { case x => x.map(_.wrap) }
      }
      val SyntacticTypeApplied: SyntacticTypeAppliedExtractor = new SyntacticTypeAppliedExtractor {
        def apply(tree: Tree, targs: List[Tree]): Tree = c.universe.build.SyntacticTypeApplied.apply(tree.unwrap, targs.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(Tree, List[Tree])] = c.universe.build.SyntacticTypeApplied.unapply(tree.unwrap).map { case (x, y) => (x.wrap, y.map(_.wrap)) }
      }
      val SyntacticValDef: SyntacticValDefExtractor = new SyntacticValDefExtractor {
        def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef = c.universe.build.SyntacticValDef(mods.unwrap, name.unwrap, tpt.unwrap, rhs.unwrap).wrap
        def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = c.universe.build.SyntacticValDef.unapply(tree.unwrap).map { case (x1, x2, x3, x4) => (x1.wrap, x2.wrap, x3.wrap, x4.wrap) }
      }
      val SyntacticValEq: SyntacticValEqExtractor = new SyntacticValEqExtractor {
        def apply(pat: Tree, rhs: Tree): Tree = c.universe.build.SyntacticValEq.apply(pat.unwrap, rhs.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, Tree)] = c.universe.build.SyntacticValEq.unapply(tree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
      }
      val SyntacticValFrom: SyntacticValFromExtractor = new SyntacticValFromExtractor {
        def apply(pat: Tree, rhs: Tree): Tree = c.universe.build.SyntacticValFrom.apply(pat.unwrap, rhs.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, Tree)] = c.universe.build.SyntacticValFrom.unapply(tree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
      }
      val SyntacticVarDef: SyntacticValDefExtractor = new SyntacticValDefExtractor {
        def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef = c.universe.build.SyntacticVarDef(mods.unwrap, name.unwrap, tpt.unwrap, rhs.unwrap).wrap
        def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = c.universe.build.SyntacticVarDef.unapply(tree.unwrap).map { case (x1, x2, x3, x4) => (x1.wrap, x2.wrap, x3.wrap, x4.wrap) }
      }
      val SyntacticCompoundType: SyntacticCompoundTypeExtractor = new SyntacticCompoundTypeExtractor {
        def apply(parents: List[Tree], defns: List[Tree]): CompoundTypeTree = c.universe.build.SyntacticCompoundType(parents.map(_.unwrap), defns.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(List[Tree], List[Tree])] = c.universe.build.SyntacticCompoundType.unapply(tree.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.map(_.wrap)) }
      }
      val SyntacticSingletonType: SyntacitcSingletonTypeExtractor = new SyntacitcSingletonTypeExtractor {
        def apply(tree: Tree): SingletonTypeTree = c.universe.build.SyntacticSingletonType(tree.unwrap).wrap
        def unapply(tree: Tree): Option[Tree] = c.universe.build.SyntacticSingletonType.unapply(tree.unwrap).map(_.wrap)
      }
      val SyntacticTypeProjection: SyntacticTypeProjectionExtractor = new SyntacticTypeProjectionExtractor {
        def apply(qual: Tree, name: TypeName): SelectFromTypeTree = c.universe.build.SyntacticTypeProjection(qual.unwrap, name.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, TypeName)] = c.universe.build.SyntacticTypeProjection.unapply(tree.unwrap).map{ case(x1, x2) => (x1.wrap, x2.wrap) }
      }
      val SyntacticAnnotatedType: SyntacticAnnotatedTypeExtractor = new SyntacticAnnotatedTypeExtractor {
        def apply(tpt: Tree, annot: Tree): Annotated = c.universe.build.SyntacticAnnotatedType(tpt.unwrap, annot.unwrap).wrap
        def unapply(tree: Tree): Option[(Tree, Tree)] = c.universe.build.SyntacticAnnotatedType.unapply(tree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
      }
      val SyntacticExistentialType: SyntacticExistentialTypeExtractor = new SyntacticExistentialTypeExtractor {
        def apply(tpt: Tree, where: List[Tree]): ExistentialTypeTree = c.universe.build.SyntacticExistentialType(tpt.unwrap, where.map(_.unwrap)).wrap
        def unapply(tree: Tree): Option[(Tree, List[MemberDef])] = c.universe.build.SyntacticExistentialType.unapply(tree.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
      }
      def ThisType(sym: Symbol): Type = c.universe.build.ThisType(sym.unwrap).wrap
      def TypeBounds(lo: Type,hi: Type): TypeBounds = c.universe.build.TypeBounds(lo.unwrap, hi.unwrap).wrap
      def TypeRef(pre: Type,sym: Symbol,args: List[Type]): Type = c.universe.build.TypeRef(pre.unwrap, sym.unwrap, args.map(_.unwrap)).wrap
      def UnliftListElementwise[T](unliftable: Unliftable[T]): UnliftListElementwise[T] = ??? // TODO: later
      def UnliftListOfListsElementwise[T](unliftable: Unliftable[T]): UnliftListOfListsElementwise[T] = ??? // TODO: later
      def freshTermName(prefix: String): TermName = c.universe.build.freshTermName(prefix).wrap
      def freshTypeName(prefix: String): TypeName = c.universe.build.freshTypeName(prefix).wrap
      def mkAnnotation(trees: List[Tree]): List[Tree] = c.universe.build.mkAnnotation(trees.map(_.unwrap)).map(_.wrap)
      def mkAnnotation(tree: Tree): Tree = c.universe.build.mkAnnotation(tree.unwrap).wrap
      def mkEarlyDef(defns: List[Tree]): List[Tree] = c.universe.build.mkEarlyDef(defns.map(_.unwrap)).map(_.wrap)
      def mkEarlyDef(defn: Tree): Tree = c.universe.build.mkEarlyDef(defn.unwrap).wrap
      def mkIdent(sym: Symbol): Ident = c.universe.build.mkIdent(sym.unwrap).wrap
      def mkPackageStat(stats: List[Tree]): List[Tree] = c.universe.build.mkPackageStat(stats.map(_.unwrap)).map(_.wrap)
      def mkPackageStat(stat: Tree): Tree = c.universe.build.mkPackageStat(stat.unwrap).wrap
      def mkRefTree(qual: Tree,sym: Symbol): Tree = c.universe.build.mkRefTree(qual.unwrap, sym.unwrap).wrap
      def mkRefineStat(stats: List[Tree]): List[Tree] = c.universe.build.mkRefineStat(stats.map(_.unwrap)).map(_.wrap)
      def mkRefineStat(stat: Tree): Tree = c.universe.build.mkRefineStat(stat.unwrap).wrap
      def mkSelect(qualifier: Tree,sym: Symbol): Select = c.universe.build.mkSelect(qualifier.unwrap, sym.unwrap).wrap
      def mkThis(sym: Symbol): Tree = c.universe.build.mkThis(sym.unwrap).wrap
      def mkTypeTree(tp: Type): TypeTree = c.universe.build.mkTypeTree(tp.unwrap).wrap
      def newFreeTerm(name: String,value: => Any,flags: FlagSet,origin: String): FreeTermSymbol = c.universe.build.newFreeTerm(name, value, flags.unwrap, origin).wrap
      def newFreeType(name: String,flags: FlagSet,origin: String): FreeTypeSymbol = c.universe.build.newFreeType(name, flags.unwrap, origin).wrap
      def newNestedSymbol(owner: Symbol,name: Name,pos: c.universe.Position,flags: FlagSet,isClass: Boolean): Symbol = c.universe.build.newNestedSymbol(owner.unwrap, name.unwrap, pos, flags.unwrap, isClass).wrap
      def newScopeWith(elems: Symbol*): Scope = c.universe.build.newScopeWith(elems.map(_.unwrap): _*).wrap
      def selectOverloadedMethod(owner: Symbol,name: String,index: Int): MethodSymbol = c.universe.build.selectOverloadedMethod(owner.unwrap, name, index).wrap
      def selectTerm(owner: Symbol,name: String): TermSymbol = c.universe.build.selectTerm(owner.unwrap, name).wrap
      def selectType(owner: Symbol,name: String): TypeSymbol = c.universe.build.selectType(owner.unwrap, name).wrap
      def setAnnotations[S <: Symbol](sym: S,annots: List[Annotation]): S = c.universe.build.setAnnotations(sym.unwrap, annots.map(_.unwrap)).wrap.asInstanceOf[S]
      def setInfo[S <: Symbol](sym: S,tpe: Type): S = c.universe.build.setInfo(sym.unwrap, tpe.unwrap).wrap.asInstanceOf[S]
      def setSymbol[T <: Tree](tree: T,sym: Symbol): T = c.universe.build.setSymbol(tree.unwrap, sym.unwrap).wrap.asInstanceOf[T]
      def setType[T <: Tree](tree: T,tpe: Type): T = c.universe.build.setType(tree.unwrap, tpe.unwrap).wrap.asInstanceOf[T]
      def thisPrefix(sym: Symbol): Type = c.universe.build.thisPrefix(sym.unwrap).wrap
      def toStats(tree: Tree): List[Tree] = c.universe.build.toStats(tree.unwrap).map(_.wrap)
    }
  }

  import universe._

  lazy val internal: ContextInternalApi = new universe.SymbolTableInternal with ContextInternalApi {
    val enclosingOwner: OurSymbol = c.internal.enclosingOwner.wrap

    type OurTransform = TransformApi
    type CompilerTransform = c.internal.TransformApi
    implicit class RichCompilerTransform(t: CompilerTransform) { def wrap = new Transform(t) }
    class Transform(t: CompilerTransform) extends TransformApi {
      def recur(tree: OurTree): OurTree = t.recur(tree.unwrap).wrap
      def default(tree: OurTree): OurTree = t.default(tree.unwrap).wrap
    }
    def transform(tree: OurTree)(transformer: (OurTree, OurTransform) => Tree): Tree = {
      c.internal.transform(tree.unwrap)((compilerTree, compilerTransform) => transformer(compilerTree.wrap, compilerTransform.wrap).unwrap).wrap
    }

    type OurTypingTransform = TypingTransformApi
    type CompilerTypingTransform = c.internal.TypingTransformApi
    implicit class RichCompilerTypingTransform(t: CompilerTypingTransform) { def wrap = new TypingTransform(t) }
    class TypingTransform(t: CompilerTypingTransform) extends Transform(t) with TypingTransformApi {
      // TODO: atOwner is going to crash here if we don't unwrap stuff
      def atOwner[T](owner: OurSymbol)(op: => T): T = t.atOwner(owner.unwrap)(op)
      def atOwner[T](tree: OurTree, owner: Symbol)(op: => T): T = t.atOwner(tree.unwrap, owner.unwrap)(op)
      def currentOwner: OurSymbol = t.currentOwner.wrap
      def typecheck(tree: OurTree): OurTree = t.typecheck(tree.unwrap).wrap
    }

    def typingTransform(tree: OurTree)(transformer: (OurTree, OurTypingTransform) => OurTree): OurTree = {
      c.internal.typingTransform(tree.unwrap)((compilerTree, compilerTransform) => transformer(compilerTree.wrap, compilerTransform.wrap).unwrap).wrap
    }
    def typingTransform(tree: OurTree, owner: OurSymbol)(transformer: (OurTree, OurTypingTransform) => OurTree): OurTree = {
      c.internal.typingTransform(tree.unwrap, owner.unwrap)((compilerTree, compilerTransform) => transformer(compilerTree.wrap, compilerTransform.wrap).unwrap).wrap
    }
  }
}