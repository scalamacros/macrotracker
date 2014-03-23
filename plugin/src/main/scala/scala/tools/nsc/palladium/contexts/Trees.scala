package scala.tools.nsc.palladium
package contexts

trait Trees {
  self: Context =>

  trait UniverseTrees {
    self: universe.type =>

    type CompilerTree = c.universe.Tree
    type OurTree = Tree
    implicit class RichCompilerTree(t: CompilerTree) {
      def wrap: OurTree = t match {
        case null => null
        case c.universe.EmptyTree => EmptyTree
        case c.universe.emptyValDef => emptyValDef
        case c.universe.noSelfType => noSelfType
        case c.universe.pendingSuperCall => pendingSuperCall
        case t if c.universe.IdentTag.runtimeClass.isInstance(t) => t.asInstanceOf[c.universe.Ident].wrap
        case t if c.universe.SelectTag.runtimeClass.isInstance(t) => t.asInstanceOf[c.universe.Select].wrap
        case t if c.universe.ApplyTag.runtimeClass.isInstance(t) => t.asInstanceOf[c.universe.Apply].wrap
        case t if c.universe.LiteralTag.runtimeClass.isInstance(t) => t.asInstanceOf[c.universe.Literal].wrap
        case _ => throw new Exception(s"don't know how to wrap $t (${c.universe.showRaw(t)}) of ${t.getClass}")
      }
    }
    implicit class RichOurTree(t: OurTree) { def unwrap: CompilerTree = t.t0 }

    implicit class RichCompilerRefTree(t: c.universe.RefTree) { def wrap: RefTree = RefTree(t.qualifier.wrap, t.name.wrap) }
    implicit class RichOurRefTree(t: RefTree) { def unwrap: c.universe.RefTree = t.t.asInstanceOf[c.universe.RefTree] }
    implicit class RichCompilerIdent(t: c.universe.Ident) { def wrap: Ident = new Ident(t) }
    implicit class RichOurIdent(t: Ident) { def unwrap: c.universe.Ident = t.t }
    implicit class RichCompilerSelect(t: c.universe.Select) { def wrap: Select = new Select(t) }
    implicit class RichOurSelect(t: Select) { def unwrap: c.universe.Select = t.t }
    implicit class RichCompilerTry(t: c.universe.Try) { def wrap: Try = new Try(t) }
    implicit class RichOurTry(t: Try) { def unwrap: c.universe.Try = t.t }
    implicit class RichCompilerApply(t: c.universe.Apply) { def wrap: Apply = new Apply(t) }
    implicit class RichOurApply(t: Apply) { def unwrap: c.universe.Apply = t.t }
    implicit class RichCompilerBlock(t: c.universe.Block) { def wrap: Block = new Block(t) }
    implicit class RichOurBlock(t: Block) { def unwrap: c.universe.Block = t.t }
    implicit class RichCompilerCaseDef(t: c.universe.CaseDef) { def wrap: CaseDef = new CaseDef(t) }
    implicit class RichOurCaseDef(t: CaseDef) { def unwrap: c.universe.CaseDef = t.t }
    implicit class RichCompilerBind(t: c.universe.Bind) { def wrap: Bind = new Bind(t) }
    implicit class RichOurBind(t: Bind) { def unwrap: c.universe.Bind = t.t }
    implicit class RichCompilerTypeTree(t: c.universe.TypeTree) { def wrap: TypeTree = new TypeTree(t) }
    implicit class RichOurTypeTree(t: TypeTree) { def unwrap: c.universe.TypeTree = t.t }
    implicit class RichCompilerThrow(t: c.universe.Throw) { def wrap: Throw = new Throw(t) }
    implicit class RichOurThrow(t: Throw) { def unwrap: c.universe.Throw = t.t }
    implicit class RichCompilerValDef(t: c.universe.ValDef) { def wrap: ValDef = new ValDef(t) }
    implicit class RichOurValDef(t: ValDef) { def unwrap: c.universe.ValDef = t.t }
    implicit class RichCompilerThis(t: c.universe.This) { def wrap: This = new This(t) }
    implicit class RichOurThis(t: This) { def unwrap: c.universe.This = t.t }
    implicit class RichCompilerLabelDef(t: c.universe.LabelDef) { def wrap: LabelDef = new LabelDef(t) }
    implicit class RichOurLabelDef(t: LabelDef) { def unwrap: c.universe.LabelDef = t.t }
    implicit class RichCompilerDefDef(t: c.universe.DefDef) { def wrap: DefDef = new DefDef(t) }
    implicit class RichOurDefDef(t: DefDef) { def unwrap: c.universe.DefDef = t.t }
    implicit class RichCompilerTypeDef(t: c.universe.TypeDef) { def wrap: TypeDef = new TypeDef(t) }
    implicit class RichOurTypeDef(t: TypeDef) { def unwrap: c.universe.TypeDef = t.t }
    implicit class RichCompilerTemplate(t: c.universe.Template) { def wrap: Template = new Template(t) }
    implicit class RichOurTemplate(t: Template) { def unwrap: c.universe.Template = t.t }
    implicit class RichCompilerModuleDef(t: c.universe.ModuleDef) { def wrap: ModuleDef = new ModuleDef(t) }
    implicit class RichOurModuleDef(t: ModuleDef) { def unwrap: c.universe.ModuleDef = t.t }
    implicit class RichCompilerClassDef(t: c.universe.ClassDef) { def wrap: ClassDef = new ClassDef(t) }
    implicit class RichOurClassDef(t: ClassDef) { def unwrap: c.universe.ClassDef = t.t }
    implicit class RichCompilerImplDef(t: c.universe.ImplDef) { def wrap: ImplDef = if (t.getClass == c.universe.ClassDefTag.runtimeClass) new ClassDef(t.asInstanceOf[c.universe.ClassDef]) else new ModuleDef(t.asInstanceOf[c.universe.ModuleDef]) }
    implicit class RichOurImplDef(t: ImplDef) { def unwrap: c.universe.ImplDef = t.t }
    implicit class RichCompilerPackageDef(t: c.universe.PackageDef) { def wrap: PackageDef = new PackageDef(t) }
    implicit class RichOurPackageDef(t: PackageDef) { def unwrap: c.universe.PackageDef = t.t }
    implicit class RichCompilerMatch(t: c.universe.Match) { def wrap: Match = new Match(t) }
    implicit class RichOurMatch(t: Match) { def unwrap: c.universe.Match = t.t }
    implicit class RichCompilerFunction(t: c.universe.Function) { def wrap: Function = new Function(t) }
    implicit class RichOurFunction(t: Function) { def unwrap: c.universe.Function = t.t }
    implicit class RichCompilerImport(t: c.universe.Import) { def wrap: Import = new Import(t) }
    implicit class RichOurImport(t: Import) { def unwrap: c.universe.Import = t.t }
    implicit class RichCompilerImportSelector(t: c.universe.ImportSelector) { def wrap: ImportSelector = new ImportSelector(t) }
    implicit class RichOurImportSelector(t: ImportSelector) { def unwrap: c.universe.ImportSelector = t.t }
    implicit class RichCompilerAlternative(t: c.universe.Alternative) { def wrap: Alternative = new Alternative(t) }
    implicit class RichOurAlternative(t: Alternative) { def unwrap: c.universe.Alternative = t.t }
    implicit class RichCompilerStar(t: c.universe.Star) { def wrap: Star = new Star(t) }
    implicit class RichOurStar(t: Star) { def unwrap: c.universe.Star = t.t }
    implicit class RichCompilerUnApply(t: c.universe.UnApply) { def wrap: UnApply = new UnApply(t) }
    implicit class RichOurUnApply(t: UnApply) { def unwrap: c.universe.UnApply = t.t }
    implicit class RichCompilerAssign(t: c.universe.Assign) { def wrap: Assign = new Assign(t) }
    implicit class RichOurAssign(t: Assign) { def unwrap: c.universe.Assign = t.t }
    implicit class RichCompilerAssignOrNamedArg(t: c.universe.AssignOrNamedArg) { def wrap: AssignOrNamedArg = new AssignOrNamedArg(t) }
    implicit class RichOurAssignOrNamedArg(t: AssignOrNamedArg) { def unwrap: c.universe.AssignOrNamedArg = t.t }
    implicit class RichCompilerTypeApply(t: c.universe.TypeApply) { def wrap: TypeApply = new TypeApply(t) }
    implicit class RichOurTypeApply(t: TypeApply) { def unwrap: c.universe.TypeApply = t.t }
    implicit class RichCompilerSuper(t: c.universe.Super) { def wrap: Super = new Super(t) }
    implicit class RichOurSuper(t: Super) { def unwrap: c.universe.Super = t.t }
    implicit class RichCompilerNew(t: c.universe.New) { def wrap: New = new New(t) }
    implicit class RichOurNew(t: New) { def unwrap: c.universe.New = t.t }
    implicit class RichCompilerIf(t: c.universe.If) { def wrap: If = new If(t) }
    implicit class RichOurIf(t: If) { def unwrap: c.universe.If = t.t }
    implicit class RichCompilerReturn(t: c.universe.Return) { def wrap: Return = new Return(t) }
    implicit class RichOurReturn(t: Return) { def unwrap: c.universe.Return = t.t }
    implicit class RichCompilerTyped(t: c.universe.Typed) { def wrap: Typed = new Typed(t) }
    implicit class RichOurTyped(t: Typed) { def unwrap: c.universe.Typed = t.t }
    implicit class RichCompilerLiteral(t: c.universe.Literal) { def wrap: Literal = new Literal(t) }
    implicit class RichOurLiteral(t: Literal) { def unwrap: c.universe.Literal = t.t }
    implicit class RichCompilerReferenceToBoxed(t: c.universe.ReferenceToBoxed) { def wrap: ReferenceToBoxed = new ReferenceToBoxed(t) }
    implicit class RichOurReferenceToBoxed(t: ReferenceToBoxed) { def unwrap: c.universe.ReferenceToBoxed = t.t }
    implicit class RichCompilerAnnotated(t: c.universe.Annotated) { def wrap: Annotated = new Annotated(t) }
    implicit class RichOurAnnotated(t: Annotated) { def unwrap: c.universe.Annotated = t.t }
    implicit class RichCompilerSingletonTypeTree(t: c.universe.SingletonTypeTree) { def wrap: SingletonTypeTree = new SingletonTypeTree(t) }
    implicit class RichOurSingletonTypeTree(t: SingletonTypeTree) { def unwrap: c.universe.SingletonTypeTree = t.t }
    implicit class RichCompilerSelectFromTypeTree(t: c.universe.SelectFromTypeTree) { def wrap: SelectFromTypeTree = new SelectFromTypeTree(t) }
    implicit class RichOurSelectFromTypeTree(t: SelectFromTypeTree) { def unwrap: c.universe.SelectFromTypeTree = t.t }
    implicit class RichCompilerCompoundTypeTree(t: c.universe.CompoundTypeTree) { def wrap: CompoundTypeTree = new CompoundTypeTree(t) }
    implicit class RichOurCompoundTypeTree(t: CompoundTypeTree) { def unwrap: c.universe.CompoundTypeTree = t.t }
    implicit class RichCompilerExistentialTypeTree(t: c.universe.ExistentialTypeTree) { def wrap: ExistentialTypeTree = new ExistentialTypeTree(t) }
    implicit class RichOurExistentialTypeTree(t: ExistentialTypeTree) { def unwrap: c.universe.ExistentialTypeTree = t.t }
    implicit class RichCompilerAppliedTypeTree(t: c.universe.AppliedTypeTree) { def wrap: AppliedTypeTree = new AppliedTypeTree(t) }
    implicit class RichOurAppliedTypeTree(t: AppliedTypeTree) { def unwrap: c.universe.AppliedTypeTree = t.t }
    implicit class RichCompilerTypedBoundsTree(t: c.universe.TypeBoundsTree) { def wrap: TypeBoundsTree = new TypeBoundsTree(t) }
    implicit class RichOurTypedBoundsTree(t: TypeBoundsTree) { def unwrap: c.universe.TypeBoundsTree = t.t }
    implicit class RichCompilerMemberDef(t: c.universe.MemberDef) { def wrap: MemberDef = (t: c.universe.Tree).wrap.asInstanceOf[MemberDef] }
    implicit class RichOurMemberDef(t: MemberDef) { def unwrap: c.universe.MemberDef = t.t }
    // implicit class RichCompilerTyped(t: c.universe.Typed) { def wrap: Typed = new Typed(t) }
    // implicit class RichOurTyped(t: Typed) { def unwrap: c.universe.Typed = t.t }

    abstract class Tree(val t0: CompilerTree) extends TreeApi {
      def canEqual(that: Any): Boolean = that.isInstanceOf[Tree]
      def productArity: Int = t0.productArity
      def productElement(n: Int): Any = unwrapAny(t0.productElement(n))
      def canHaveAttrs: Boolean = t0.canHaveAttrs
      def children: List[Tree] = t0.children.map(_.wrap)
      def collect[T](pf: PartialFunction[Tree,T]): List[T] = t0.collect{ case t => pf(t.wrap) } // TODO: needs unwrapAny as well :(
      def duplicate: this.type = t0.duplicate.wrap.asInstanceOf[this.type]
      def equalsStructure(that: Tree): Boolean = t0.equalsStructure(that.unwrap)
      def exists(p: Tree => Boolean): Boolean = t0.exists(t => p(t.wrap))
      def filter(f: Tree => Boolean): List[Tree] = t0.filter(t => f(t.wrap)).map(_.wrap)
      def find(p: Tree => Boolean): Option[Tree] = t0.find(t => p(t.wrap)).map(_.wrap)
      def forAll(p: Tree => Boolean): Boolean = t0.forAll(t => p(t.wrap))
      def foreach(f: Tree => Unit): Unit = t0.foreach(t => f(t.wrap))
      def isDef: Boolean = t0.isDef
      def isEmpty: Boolean = t0.isEmpty
      def isTerm: Boolean = t0.isTerm
      def isType: Boolean = t0.isType
      def nonEmpty: Boolean = t0.nonEmpty
      def orElse(alt: => Tree): Tree = t0.orElse(alt.unwrap).wrap
      def pos: Position = t0.pos.wrap
      def symbol: Symbol = t0.symbol.wrap
      def tpe: Type = t0.tpe.wrap
      def withFilter(f: Tree => Boolean): List[Tree] = t0.withFilter(t => f(t.wrap)).map(_.wrap)
      override def hashCode(): Int = System.identityHashCode(this)
      override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    }

    trait TermTree extends Tree with TermTreeApi {
      val t: c.universe.TermTree
    }

    trait TypTree extends Tree with TypTreeApi {
      val t: c.universe.TypTree
    }

    trait SymTree extends Tree with SymTreeApi {
      val t: c.universe.SymTree
      override def symbol: Symbol = t.symbol.wrap
    }

    trait NameTree extends Tree with NameTreeApi {
      val t: c.universe.NameTree
      def name: Name = t.name.wrap
    }

    trait RefTree extends SymTree with NameTree with RefTreeApi {
      override val t: c.universe.RefTree
      def qualifier: Tree = t.qualifier.wrap
      override def name: Name = t.name.wrap
    }
    object RefTree extends RefTreeExtractor {
      def apply(qualifier: Tree, name: Name): RefTree = c.universe.RefTree(qualifier.unwrap, name.unwrap).wrap
      def unapply(refTree: RefTree): Option[(Tree, Name)] = c.universe.RefTree.unapply(refTree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    trait DefTree extends SymTree with NameTree with DefTreeApi {
      override val t: c.universe.DefTree
    }

    abstract class MemberDef(override val t: c.universe.MemberDef) extends Tree(t) with DefTree with MemberDefApi {
      def mods: Modifiers = t.mods.wrap
    }

    class PackageDef(override val t: c.universe.PackageDef) extends MemberDef(t) with PackageDefApi {
      def pid: RefTree = t.pid.wrap
      def stats: List[Tree] = t.stats.map(_.wrap)
    }
    object PackageDef extends PackageDefExtractor {
      def apply(pid: RefTree,stats: List[Tree]): PackageDef = c.universe.PackageDef.apply(pid.unwrap, stats.map(_.unwrap)).wrap
      def unapply(packageDef: PackageDef): Option[(RefTree, List[Tree])] = c.universe.PackageDef.unapply(packageDef.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    abstract class ImplDef(override val t: c.universe.ImplDef) extends MemberDef(t) with ImplDefApi {
      def impl: Template = t.impl.wrap
    }

    class ClassDef(override val t: c.universe.ClassDef) extends ImplDef(t) with ClassDefApi {
      override def name: TypeName = t.name.wrap
      def tparams: List[TypeDef] = t.tparams.map(_.wrap)
    }
    object ClassDef extends ClassDefExtractor {
      def apply(mods: Modifiers,name: TypeName,tparams: List[TypeDef],impl: Template): ClassDef =
        c.universe.ClassDef.apply(mods.unwrap, name.unwrap, tparams.map(_.unwrap), impl.unwrap).wrap
      def unapply(classDef: ClassDef): Option[(Modifiers, TypeName, List[TypeDef], Template)] =
        c.universe.ClassDef.unapply(classDef.unwrap).map { case (x1, x2, x3, x4) => (x1.wrap, x2.wrap, x3.map(_.wrap), x4.wrap) }
    }

    class ModuleDef(override val t: c.universe.ModuleDef) extends ImplDef(t) with ModuleDefApi {
      override def name: TermName = t.name.wrap
    }
    object ModuleDef extends ModuleDefExtractor {
      def apply(mods: Modifiers,name: TermName,impl: Template): ModuleDef = c.universe.ModuleDef.apply(mods.unwrap, name.unwrap, impl.unwrap).wrap
      def unapply(moduleDef: ModuleDef): Option[(Modifiers, TermName, Template)] = c.universe.ModuleDef.unapply(moduleDef.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.wrap, x3.wrap) }
    }

    abstract class ValOrDefDef(override val t: c.universe.ValOrDefDef) extends MemberDef(t) with ValOrDefDefApi {
      override def name: TermName = t.name.wrap
      def tpt: Tree = t.tpt.wrap
      def rhs: Tree = t.rhs.wrap
    }

    class ValDef(override val t: c.universe.ValDef) extends ValOrDefDef(t) with ValDefApi {
    }
    object ValDef extends ValDefExtractor {
      def apply(mods: Modifiers,name: TermName,tpt: Tree,rhs: Tree): ValDef = c.universe.ValDef(mods.unwrap, name.unwrap, tpt.unwrap, rhs.unwrap).wrap
      def unapply(valDef: ValDef): Option[(Modifiers, TermName, Tree, Tree)] = c.universe.ValDef.unapply(valDef.unwrap).map { case (x1, x2, x3, x4) => (x1.wrap, x2.wrap, x3.wrap, x4.wrap) }
    }

    class DefDef(override val t: c.universe.DefDef) extends ValOrDefDef(t) with DefDefApi {
      def tparams: List[TypeDef] = t.tparams.map(_.wrap)
      def vparamss: List[List[ValDef]] = t.vparamss.map(_.map(_.wrap))
    }
    object DefDef extends DefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef =
        c.universe.DefDef.apply(mods.unwrap, name.unwrap, tparams.map(_.unwrap), vparamss.map(_.map(_.unwrap)), tpt.unwrap, rhs.unwrap).wrap
      def unapply(defDef: DefDef): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)] =
        c.universe.DefDef.unapply(defDef.unwrap).map { case (x1, x2, x3, x4, x5, x6) => (x1.wrap, x2.wrap, x3.map(_.wrap), x4.map(_.map(_.wrap)), x5.wrap, x6.wrap) }
    }

    class TypeDef(override val t: c.universe.TypeDef) extends MemberDef(t) with TypeDefApi {
      override def name: TypeName = t.name.wrap
      def tparams: List[TypeDef] = t.tparams.map(_.wrap)
      def rhs: Tree = t.rhs.wrap
    }
    object TypeDef extends TypeDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree): TypeDef =
        c.universe.TypeDef.apply(mods.unwrap, name.unwrap, tparams.map(_.unwrap), rhs.unwrap).wrap
      def unapply(typeDef: TypeDef): Option[(Modifiers, TypeName, List[TypeDef], Tree)] =
        c.universe.TypeDef.unapply(typeDef.unwrap).map { case (x1, x2, x3, x4) => (x1.wrap, x2.wrap, x3.map(_.wrap), x4.wrap) }
    }

    class LabelDef(override val t: c.universe.LabelDef) extends Tree(t) with DefTree with TermTree with LabelDefApi {
      override def name: TermName = t.name.wrap
      def params: List[Ident] = t.params.map(_.wrap)
      def rhs: Tree = t.rhs.wrap
    }
    object LabelDef extends LabelDefExtractor {
      def apply(name: TermName, params: List[Ident], rhs: Tree): LabelDef = c.universe.LabelDef.apply(name.unwrap, params.map(_.unwrap), rhs.unwrap).wrap
      def unapply(labelDef: LabelDef): Option[(TermName, List[Ident], Tree)] = c.universe.LabelDef.unapply(labelDef.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.map(_.wrap), x3.wrap) }
    }

    class ImportSelector(val t: c.universe.ImportSelector) extends ImportSelectorApi {
      def name: Name = t.name.wrap
      def namePos: Int = t.namePos
      def rename: Name = t.rename.wrap
      def renamePos: Int = t.renamePos
    }
    object ImportSelector extends ImportSelectorExtractor {
      def apply(name: Name, namePos: Int, rename: Name, renamePos: Int): ImportSelector =
        c.universe.ImportSelector.apply(name.unwrap, namePos, rename.unwrap, renamePos).wrap
      def unapply(importSelector: ImportSelector): Option[(Name, Int, Name, Int)] =
        c.universe.ImportSelector.unapply(importSelector.unwrap).map { case (x1, x2, x3, x4) => (x1.wrap, x2, x3.wrap, x4) }
    }

    class Import(override val t: c.universe.Import) extends Tree(t) with SymTree with ImportApi {
      def expr: Tree = t.expr.wrap
      def selectors: List[ImportSelector] = t.selectors.map(_.wrap)
    }
    object Import extends ImportExtractor {
      def apply(expr: Tree, selectors: List[ImportSelector]): Import =
        c.universe.Import.apply(expr.unwrap, selectors.map(_.unwrap)).wrap
      def unapply(import_ : Import): Option[(Tree, List[ImportSelector])] =
        c.universe.Import.unapply(import_.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class Template(override val t: c.universe.Template) extends Tree(t) with SymTree with TemplateApi {
      def parents: List[Tree] = t.parents.map(_.wrap)
      def self: ValDef = t.self.wrap
      def body: List[Tree] = t.body.map(_.wrap)
    }
    object Template extends TemplateExtractor {
      def apply(parents: List[Tree], self: ValDef, body: List[Tree]): Template =
        c.universe.Template(parents.map(_.unwrap), self.unwrap, body.map(_.unwrap)).wrap
      def unapply(template: Template): Option[(List[Tree], ValDef, List[Tree])] =
        c.universe.Template.unapply(template.unwrap).map { case (x1, x2, x3) => (x1.map(_.wrap), x2.wrap, x3.map(_.wrap)) }
    }

    class Block(override val t: c.universe.Block) extends Tree(t) with TermTree with BlockApi {
      def stats: List[Tree] = t.stats.map(_.wrap)
      def expr: Tree = t.expr.wrap
    }
    object Block extends BlockExtractor {
      def apply(stats: List[Tree], expr: Tree): Block =
        c.universe.Block(stats.map(_.unwrap), expr.unwrap).wrap
      def unapply(block: Block): Option[(List[Tree], Tree)] =
        c.universe.Block.unapply(block.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class CaseDef(val t: c.universe.CaseDef) extends Tree(t) with CaseDefApi {
      def pat: Tree = t.pat.wrap
      def guard: Tree = t.guard.wrap
      def body: Tree = t.body.wrap
    }
    object CaseDef extends CaseDefExtractor {
      def apply(pat: Tree, guard: Tree, body: Tree): CaseDef =
        c.universe.CaseDef(pat.unwrap, guard.unwrap, body.unwrap).wrap
      def unapply(caseDef: CaseDef): Option[(Tree, Tree, Tree)] =
        c.universe.CaseDef.unapply(caseDef.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.wrap, x3.wrap) }
    }

    class Alternative(override val t: c.universe.Alternative) extends Tree(t) with TermTree with AlternativeApi {
      def trees: List[Tree] = t.trees.map(_.wrap)
    }
    object Alternative extends AlternativeExtractor {
      def apply(trees: List[Tree]): Alternative = c.universe.Alternative.apply(trees.map(_.unwrap)).wrap
      def unapply(alternative: Alternative): Option[List[Tree]] = c.universe.Alternative.unapply(alternative.unwrap).map(_.map(_.wrap))
    }

    class Star(override val t: c.universe.Star) extends Tree(t) with TermTree with StarApi {
      def elem: Tree = t.elem.wrap
    }
    object Star extends StarExtractor {
      def apply(elem: Tree): Star = c.universe.Star.apply(elem.unwrap).wrap
      def unapply(star: Star): Option[Tree] = c.universe.Star.unapply(star.unwrap).map(_.wrap)
    }

    class Bind(override val t: c.universe.Bind) extends Tree(t) with DefTree with BindApi {
      override def name: Name = t.name.wrap
      def body: Tree = t.body.wrap
    }
    object Bind extends BindExtractor {
      def apply(name: Name, body: Tree): Bind = c.universe.Bind(name.unwrap, body.unwrap).wrap
      def unapply(bind: Bind): Option[(Name, Tree)] = c.universe.Bind.unapply(bind.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class UnApply(override val t: c.universe.UnApply) extends Tree(t) with TermTree with UnApplyApi {
      def fun: Tree = t.fun.wrap
      def args: List[Tree] = t.args.map(_.wrap)
    }
    object UnApply extends UnApplyExtractor {
      def apply(fun: Tree, args: List[Tree]): UnApply = c.universe.UnApply(fun.unwrap, args.map(_.unwrap)).wrap
      def unapply(unApply: UnApply): Option[(Tree, List[Tree])] = c.universe.UnApply.unapply(unApply.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class Function(override val t: c.universe.Function) extends Tree(t) with TermTree with SymTree with FunctionApi {
      def vparams: List[ValDef] = t.vparams.map(_.wrap)
      def body: Tree = t.body.wrap
    }
    object Function extends FunctionExtractor {
      def apply(vparams: List[ValDef], body: Tree): Function =
        c.universe.Function.apply(vparams.map(_.unwrap), body.unwrap).wrap
      def unapply(function: Function): Option[(List[ValDef], Tree)] =
        c.universe.Function.unapply(function.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class Assign(override val t: c.universe.Assign) extends Tree(t) with TermTree with AssignApi {
      def lhs: Tree = t.lhs.wrap
      def rhs: Tree = t.rhs.wrap
    }
    object Assign extends AssignExtractor {
      def apply(lhs: Tree, rhs: Tree): Assign = c.universe.Assign.apply(lhs.unwrap, rhs.unwrap).wrap
      def unapply(assign: Assign): Option[(Tree, Tree)] = c.universe.Assign.unapply(assign.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class AssignOrNamedArg(override val t: c.universe.AssignOrNamedArg) extends Tree(t) with TermTree with AssignOrNamedArgApi {
      def lhs: Tree = t.lhs.wrap
      def rhs: Tree = t.rhs.wrap
    }
    object AssignOrNamedArg extends AssignOrNamedArgExtractor {
      def apply(lhs: Tree, rhs: Tree): AssignOrNamedArg = c.universe.AssignOrNamedArg.apply(lhs.unwrap, rhs.unwrap).wrap
      def unapply(assignOrNamedArg: AssignOrNamedArg): Option[(Tree, Tree)] = c.universe.AssignOrNamedArg.unapply(assignOrNamedArg.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class If(override val t: c.universe.If) extends Tree(t) with TermTree with IfApi {
      def cond: Tree = t.cond.wrap
      def thenp: Tree = t.thenp.wrap
      def elsep: Tree = t.elsep.wrap
    }
    object If extends IfExtractor {
    def apply(cond: Tree, thenp: Tree, elsep: Tree): If = c.universe.If(cond.unwrap, thenp.unwrap, elsep.unwrap).wrap
    def unapply(if_ : If): Option[(Tree, Tree, Tree)] = c.universe.If.unapply(if_.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.wrap, x3.wrap) }
    }

    class Match(override val t: c.universe.Match) extends Tree(t) with TermTree with MatchApi {
      def selector: Tree = t.selector.wrap
      def cases: List[CaseDef] = t.cases.map(_.wrap)
    }
    object Match extends MatchExtractor {
      def apply(selector: Tree, cases: List[CaseDef]): Match = c.universe.Match(selector.unwrap, cases.map(_.unwrap)).wrap
      def unapply(match_ : Match): Option[(Tree, List[CaseDef])] = c.universe.Match.unapply(match_.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class Return(override val t: c.universe.Return) extends Tree(t) with SymTree with TermTree with ReturnApi {
      def expr: Tree = t.expr.wrap
    }
    object Return extends ReturnExtractor {
      def apply(expr: Tree): Return = c.universe.Return(expr.unwrap).wrap
      def unapply(return_ : Return): Option[Tree] = c.universe.Return.unapply(return_.unwrap).map(_.wrap)
    }

    class Try(override val t: c.universe.Try) extends Tree(t) with TermTree with TryApi {
      def block: Tree = t.block.wrap
      def catches: List[CaseDef] = t.catches.map(_.wrap)
      def finalizer: Tree = t.finalizer.wrap
    }
    object Try extends TryExtractor {
      def apply(block: Tree, catches: List[CaseDef], finalizer: Tree): Try =
        c.universe.Try(block.unwrap, catches.map(_.unwrap), finalizer.unwrap).wrap
      def unapply(try_ : Try): Option[(Tree, List[CaseDef], Tree)] =
        c.universe.Try.unapply(try_.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.map(_.wrap), x3.wrap) }
    }

    class Throw(override val t: c.universe.Throw) extends Tree(t) with TermTree with ThrowApi {
      def expr: Tree = t.expr.wrap
    }
    object Throw extends ThrowExtractor {
      def apply(expr: Tree): Throw = c.universe.Throw(expr.unwrap).wrap
      def unapply(throw_ : Throw): Option[Tree] = c.universe.Throw.unapply(throw_.unwrap).map(_.wrap)
    }

    class New(override val t: c.universe.New) extends Tree(t) with TermTree with NewApi {
      def tpt: Tree = t.tpt.wrap
    }
    object New extends NewExtractor {
      def apply(tpt: Tree): New = c.universe.New(tpt.unwrap).wrap
      def unapply(new_ : New): Option[Tree] = c.universe.New.unapply(new_.unwrap).map(_.wrap)
    }

    class Typed(override val t: c.universe.Typed) extends Tree(t) with TermTree with TypedApi {
      def expr: Tree = t.expr.wrap
      def tpt: Tree = t.tpt.wrap
    }
    object Typed extends TypedExtractor {
      def apply(expr: Tree, tpt: Tree): Typed = c.universe.Typed.apply(expr.unwrap, tpt.unwrap).wrap
      def unapply(typed: Typed): Option[(Tree, Tree)] = c.universe.Typed.unapply(typed.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    abstract class GenericApply(override val t: c.universe.GenericApply) extends Tree(t) with TermTree with GenericApplyApi {
      def fun: Tree = t.fun.wrap
      def args: List[Tree] = t.args.map(_.wrap)
    }

    class TypeApply(override val t: c.universe.TypeApply) extends GenericApply(t) with TypeApplyApi {
    }
    object TypeApply extends TypeApplyExtractor {
      def apply(fun: Tree, args: List[Tree]): TypeApply = c.universe.TypeApply(fun.unwrap, args.map(_.unwrap)).wrap
      def unapply(typeApply: TypeApply): Option[(Tree, List[Tree])] = c.universe.TypeApply.unapply(typeApply.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class Apply(override val t: c.universe.Apply) extends GenericApply(t) with ApplyApi {
    }
    object Apply extends ApplyExtractor {
      def apply(fun: Tree, args: List[Tree]): Apply = c.universe.Apply(fun.unwrap, args.map(_.unwrap)).wrap
      def unapply(apply: Apply): Option[(Tree, List[Tree])] = c.universe.Apply.unapply(apply.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class Super(override val t: c.universe.Super) extends Tree(t) with TermTree with SuperApi {
      def qual: Tree = t.qual.wrap
      def mix: TypeName = t.mix.wrap
    }
    object Super extends SuperExtractor {
      def apply(qual: Tree, mix: TypeName): Super = c.universe.Super(qual.unwrap, mix.unwrap).wrap
      def unapply(super_ : Super): Option[(Tree, TypeName)] = c.universe.Super.unapply(super_.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class This(override val t: c.universe.This) extends Tree(t) with TermTree with SymTree with ThisApi {
      def qual: TypeName = t.qual.wrap
    }
    object This extends ThisExtractor {
      def apply(qual: TypeName): This = c.universe.This(qual.unwrap).wrap
      def unapply(this_ : This): Option[TypeName] = c.universe.This.unapply(this_.unwrap).map(_.wrap)
    }

    class Select(override val t: c.universe.Select) extends Tree(t) with RefTree with SelectApi {
    }
    object Select extends SelectExtractor {
      def apply(qualifier: Tree, name: Name): Select = c.universe.Select(qualifier.unwrap, name.unwrap).wrap
      def unapply(select: Select): Option[(Tree, Name)] = c.universe.Select.unapply(select.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class Ident(override val t: c.universe.Ident) extends Tree(t) with RefTree with IdentApi {
      def isBackquoted: Boolean = t.isBackquoted
    }
    object Ident extends IdentExtractor {
      def apply(name: Name): Ident = c.universe.Ident(name.unwrap).wrap
      def unapply(ident: Ident): Option[Name] = c.universe.Ident.unapply(ident.unwrap).map(_.wrap)
    }

    class Literal(override val t: c.universe.Literal) extends Tree(t) with TermTree with LiteralApi {
      def value: Constant = t.value.wrap
    }
    object Literal extends LiteralExtractor {
      def apply(value: Constant): Literal = c.universe.Literal(value.unwrap).wrap
      def unapply(literal: Literal): Option[Constant] = c.universe.Literal.unapply(literal.unwrap).map(_.wrap)
    }

    class ReferenceToBoxed(override val t: c.universe.ReferenceToBoxed) extends Tree(t) with TermTree with ReferenceToBoxedApi {
      def ident: Tree = t.ident.wrap
    }
    object ReferenceToBoxed extends ReferenceToBoxedExtractor {
      def apply(ident: Ident): ReferenceToBoxed = c.universe.ReferenceToBoxed(ident.unwrap).wrap
      def unapply(referenceToBoxed: ReferenceToBoxed): Option[Ident] = c.universe.ReferenceToBoxed.unapply(referenceToBoxed.unwrap).map(_.wrap)
    }

    class Annotated(val t: c.universe.Annotated) extends Tree(t) with AnnotatedApi {
      def annot: Tree = t.annot.wrap
      def arg: Tree = t.arg.wrap
    }
    object Annotated extends AnnotatedExtractor {
      def apply(annot: Tree, arg: Tree): Annotated = c.universe.Annotated(annot.unwrap, arg.unwrap).wrap
      def unapply(annotated: Annotated): Option[(Tree, Tree)] = c.universe.Annotated.unapply(annotated.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class SingletonTypeTree(override val t: c.universe.SingletonTypeTree) extends Tree(t) with TypTree with SingletonTypeTreeApi {
      def ref: Tree = t.ref.wrap
    }
    object SingletonTypeTree extends SingletonTypeTreeExtractor {
      def apply(ref: Tree): SingletonTypeTree = c.universe.SingletonTypeTree(ref.unwrap).wrap
      def unapply(singletonTypeTree: SingletonTypeTree): Option[Tree] = c.universe.SingletonTypeTree.unapply(singletonTypeTree.unwrap).map(_.wrap)
    }

    class SelectFromTypeTree(override val t: c.universe.SelectFromTypeTree) extends Tree(t) with RefTree with TypTree with SelectFromTypeTreeApi {
      override def name: TypeName = t.name.wrap
    }
    object SelectFromTypeTree extends SelectFromTypeTreeExtractor {
      def apply(qualifier: Tree, name: TypeName): SelectFromTypeTree = c.universe.SelectFromTypeTree(qualifier.unwrap, name.unwrap).wrap
      def unapply(selectFromTypeTree: SelectFromTypeTree): Option[(Tree, TypeName)] = c.universe.SelectFromTypeTree.unapply(selectFromTypeTree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class CompoundTypeTree(override val t: c.universe.CompoundTypeTree) extends Tree(t) with TypTree with CompoundTypeTreeApi {
      def templ: Template = t.templ.wrap
    }
    object CompoundTypeTree extends CompoundTypeTreeExtractor {
      def apply(templ: Template): CompoundTypeTree = c.universe.CompoundTypeTree(templ.unwrap).wrap
      def unapply(compoundTypeTree: CompoundTypeTree): Option[Template] = c.universe.CompoundTypeTree.unapply(compoundTypeTree.unwrap).map(_.wrap)
    }

    class AppliedTypeTree(override val t: c.universe.AppliedTypeTree) extends Tree(t) with TypTree with AppliedTypeTreeApi {
      def tpt: Tree = t.tpt.wrap
      def args: List[Tree] = t.args.map(_.wrap)
    }
    object AppliedTypeTree extends AppliedTypeTreeExtractor {
      def apply(tpt: Tree, args: List[Tree]): AppliedTypeTree = c.universe.AppliedTypeTree(tpt.unwrap, args.map(_.unwrap)).wrap
      def unapply(appliedTypeTree: AppliedTypeTree): Option[(Tree, List[Tree])] = c.universe.AppliedTypeTree.unapply(appliedTypeTree.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class TypeBoundsTree(override val t: c.universe.TypeBoundsTree) extends Tree(t) with TypTree with TypeBoundsTreeApi {
      def lo: Tree = t.lo.wrap
      def hi: Tree = t.hi.wrap
    }
    object TypeBoundsTree extends TypeBoundsTreeExtractor {
      def apply(lo: Tree, hi: Tree): TypeBoundsTree = c.universe.TypeBoundsTree(lo.unwrap, hi.unwrap).wrap
      def unapply(typeBoundsTree: TypeBoundsTree): Option[(Tree, Tree)] = c.universe.TypeBoundsTree.unapply(typeBoundsTree.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class ExistentialTypeTree(override val t: c.universe.ExistentialTypeTree) extends Tree(t) with TypTree with ExistentialTypeTreeApi {
      def tpt: Tree = t.tpt.wrap
      def whereClauses: List[MemberDef] = t.whereClauses.map(_.wrap)
    }
    object ExistentialTypeTree extends ExistentialTypeTreeExtractor {
      def apply(tpt: Tree, whereClauses: List[MemberDef]): ExistentialTypeTree = c.universe.ExistentialTypeTree(tpt.unwrap, whereClauses.map(_.unwrap)).wrap
      def unapply(existentialTypeTree: ExistentialTypeTree): Option[(Tree, List[MemberDef])] = c.universe.ExistentialTypeTree.unapply(existentialTypeTree.unwrap).map { case (x1, x2) => (x1.wrap, x2.map(_.wrap)) }
    }

    class TypeTree(override val t: c.universe.TypeTree) extends Tree(t) with TypTree with TypeTreeApi {
      def original: Tree = t.original.wrap
    }
    object TypeTree extends TypeTreeExtractor {
      def apply(): TypeTree = c.universe.TypeTree().wrap
      def unapply(typeTree: TypeTree): Boolean = c.universe.TypeTree.unapply(typeTree.unwrap)
    }

    case object EmptyTree extends Tree(c.universe.EmptyTree)
    lazy val emptyValDef = noSelfType
    case object noSelfType extends ValDef(c.universe.noSelfType)
    case object pendingSuperCall extends Apply(c.universe.pendingSuperCall)

    def Block(stats: Tree*): Block = c.universe.Block(stats.map(_.unwrap): _*).wrap
    def CaseDef(pat: Tree, body: Tree): CaseDef = c.universe.CaseDef(pat.unwrap, body.unwrap).wrap
    def Bind(sym: Symbol, body: Tree): Bind = c.universe.Bind(sym.unwrap, body.unwrap).wrap
    def Try(body: Tree, cases: (Tree, Tree)*): Try = c.universe.Try(body.unwrap, cases.map{case (k, v) => (k.unwrap, v.unwrap)}: _*).wrap
    def Throw(tpe: Type, args: Tree*): Throw = c.universe.Throw(tpe.unwrap, args.map(_.unwrap): _*).wrap
    def New(tpt: Tree, argss: List[List[Tree]]): Tree = c.universe.New(tpt.unwrap, argss.map(_.map(_.unwrap))).wrap
    def New(tpe: Type, args: Tree*): Tree = c.universe.New(tpe.unwrap, args.map(_.unwrap): _*).wrap
    def New(sym: Symbol, args: Tree*): Tree = c.universe.New(sym.unwrap, args.map(_.unwrap): _*).wrap
    def Apply(sym: Symbol, args: Tree*): Tree = c.universe.Apply(sym.unwrap, args.map(_.unwrap): _*).wrap
    def ApplyConstructor(tpt: Tree, args: List[Tree]): Tree = c.universe.ApplyConstructor(tpt.unwrap, args.map(_.unwrap)).wrap
    def Super(sym: Symbol, mix: TypeName): Tree = c.universe.Super(sym.unwrap, mix.unwrap).wrap
    def This(sym: Symbol): Tree = c.universe.This(sym.unwrap).wrap
    def Select(qualifier: Tree, name: String): Select = c.universe.Select(qualifier.unwrap, TermName(name).unwrap).wrap
    def Select(qualifier: Tree, sym: Symbol): Select = c.universe.Select(qualifier.unwrap, sym.unwrap).wrap
    def Ident(name: String): Ident = c.universe.Ident(TermName(name).unwrap).wrap
    def Ident(sym: Symbol): Ident = c.universe.Ident(sym.unwrap).wrap
    def TypeTree(tp: Type): TypeTree = c.universe.TypeTree(tp.unwrap).wrap

    type TreeCopier = AdaptingTreeCopier
    class AdaptingTreeCopier(tc: c.universe.TreeCopier) extends TreeCopierOps {
      def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) =
        tc.ClassDef(tree.unwrap, mods.unwrap, name.unwrap, tparams.map(_.unwrap), impl.unwrap).wrap
      def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) =
        tc.PackageDef(tree.unwrap, pid.unwrap, stats.map(_.unwrap)).wrap
      def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) =
        tc.ModuleDef(tree.unwrap, mods.unwrap, name.toTermName.unwrap, impl.unwrap).wrap
      def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) =
        tc.ValDef(tree.unwrap, mods.unwrap, name.toTermName.unwrap, tpt.unwrap, rhs.unwrap).wrap
      def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
        tc.DefDef(tree.unwrap, mods.unwrap, name.toTermName.unwrap, tparams.map(_.unwrap), vparamss.map(_.map(_.unwrap)), tpt.unwrap, rhs.unwrap).wrap
      def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =
        tc.TypeDef(tree.unwrap, mods.unwrap, name.toTypeName.unwrap, tparams.map(_.unwrap), rhs.unwrap).wrap
      def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
        tc.LabelDef(tree.unwrap, name.toTermName.unwrap, params.map(_.unwrap), rhs.unwrap).wrap
      def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) =
        tc.Import(tree.unwrap, expr.unwrap, selectors.map(_.unwrap)).wrap
      def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) =
        tc.Template(tree.unwrap, parents.map(_.unwrap), self.unwrap, body.map(_.unwrap)).wrap
      def Block(tree: Tree, stats: List[Tree], expr: Tree) =
        tc.Block(tree.unwrap, stats.map(_.unwrap), expr.unwrap).wrap
      def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
        tc.CaseDef(tree.unwrap, pat.unwrap, guard.unwrap, body.unwrap).wrap
      def Alternative(tree: Tree, trees: List[Tree]) =
        tc.Alternative(tree.unwrap, trees.map(_.unwrap)).wrap
      def Star(tree: Tree, elem: Tree) =
        tc.Star(tree.unwrap, elem.unwrap).wrap
      def Bind(tree: Tree, name: Name, body: Tree) =
        tc.Bind(tree.unwrap, name.unwrap, body.unwrap).wrap
      def UnApply(tree: Tree, fun: Tree, args: List[Tree]) =
        tc.UnApply(tree.unwrap, fun.unwrap, args.map(_.unwrap)).wrap
      def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
        tc.Function(tree.unwrap, vparams.map(_.unwrap), body.unwrap).wrap
      def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
        tc.Assign(tree.unwrap, lhs.unwrap, rhs.unwrap).wrap
      def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) =
        tc.AssignOrNamedArg(tree.unwrap, lhs.unwrap, rhs.unwrap).wrap
      def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
        tc.If(tree.unwrap, cond.unwrap, thenp.unwrap, elsep.unwrap).wrap
      def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =
        tc.Match(tree.unwrap, selector.unwrap, cases.map(_.unwrap)).wrap
      def Return(tree: Tree, expr: Tree) =
        tc.Return(tree.unwrap, expr.unwrap).wrap
      def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) =
        tc.Try(tree.unwrap, block.unwrap, catches.map(_.unwrap), finalizer.unwrap).wrap
      def Throw(tree: Tree, expr: Tree) =
        tc.Throw(tree.unwrap, expr.unwrap).wrap
      def New(tree: Tree, tpt: Tree) =
        tc.New(tree.unwrap, tpt.unwrap).wrap
      def Typed(tree: Tree, expr: Tree, tpt: Tree) =
        tc.Typed(tree.unwrap, expr.unwrap, tpt.unwrap).wrap
      def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) =
        tc.TypeApply(tree.unwrap, fun.unwrap, args.map(_.unwrap)).wrap
      def Apply(tree: Tree, fun: Tree, args: List[Tree]) =
        tc.Apply(tree.unwrap, fun.unwrap, args.map(_.unwrap)).wrap
      def Super(tree: Tree, qual: Tree, mix: TypeName) =
        tc.Super(tree.unwrap, qual.unwrap, mix.unwrap).wrap
      def This(tree: Tree, qual: Name) =
        tc.This(tree.unwrap, qual.toTypeName.unwrap).wrap
      def Select(tree: Tree, qualifier: Tree, selector: Name) =
        tc.Select(tree.unwrap, qualifier.unwrap, selector.unwrap).wrap
      def Ident(tree: Tree, name: Name) =
        tc.Ident(tree.unwrap, name.unwrap).wrap
      def RefTree(tree: Tree, qualifier: Tree, selector: Name) =
        tc.RefTree(tree.unwrap, qualifier.unwrap, selector.unwrap).wrap
      def ReferenceToBoxed(tree: Tree, idt: Ident) =
        tc.ReferenceToBoxed(tree.unwrap, idt.unwrap).wrap
      def Literal(tree: Tree, value: Constant) =
        tc.Literal(tree.unwrap, value.unwrap).wrap
      def TypeTree(tree: Tree) =
        tc.TypeTree(tree.unwrap).wrap
      def Annotated(tree: Tree, annot: Tree, arg: Tree) =
        tc.Annotated(tree.unwrap, annot.unwrap, arg.unwrap).wrap
      def SingletonTypeTree(tree: Tree, ref: Tree) =
        tc.SingletonTypeTree(tree.unwrap, ref.unwrap).wrap
      def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
        tc.SelectFromTypeTree(tree.unwrap, qualifier.unwrap, selector.toTypeName.unwrap).wrap
      def CompoundTypeTree(tree: Tree, templ: Template) =
        tc.CompoundTypeTree(tree.unwrap, templ.unwrap).wrap
      def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
        tc.AppliedTypeTree(tree.unwrap, tpt.unwrap, args.map(_.unwrap)).wrap
      def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) =
        tc.TypeBoundsTree(tree.unwrap, lo.unwrap, hi.unwrap).wrap
      def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[MemberDef]) =
        tc.ExistentialTypeTree(tree.unwrap, tpt.unwrap, whereClauses.map(_.unwrap)).wrap
    }
    def newLazyTreeCopier: TreeCopier = new AdaptingTreeCopier(c.universe.newLazyTreeCopier)
    def newStrictTreeCopier: TreeCopier = new AdaptingTreeCopier(c.universe.newStrictTreeCopier)

    type CompilerModifiers = c.universe.Modifiers
    type OurModifiers = Modifiers
    implicit class RichCompilerModifiers(m: CompilerModifiers) { def wrap: OurModifiers = new OurModifiers(m) }
    implicit class RichOurModifiers(m: OurModifiers) { def unwrap: CompilerModifiers = m.m }
    class Modifiers(val m: CompilerModifiers) extends ModifiersApi {
      def annotations: List[Tree] = m.annotations.map(_.wrap)
      def flags: FlagSet = m.flags.wrap
      def hasFlag(flag: FlagSet): Boolean = m.hasFlag(flag.unwrap)
      def privateWithin: Name = m.privateWithin.wrap
    }
    object Modifiers extends ModifiersExtractor {
      def apply(flags: FlagSet,privateWithin: Name,annotations: List[Tree]): Modifiers = c.universe.Modifiers.apply(flags.unwrap, privateWithin.unwrap, annotations.map(_.unwrap)).wrap
      def unapply(mods: Modifiers): Option[(FlagSet, Name, List[Tree])] = c.universe.Modifiers.unapply(mods.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.wrap, x3.map(_.wrap)) }
    }
  }
}