package scala.tools.nsc.palladium
package contexts

import scala.reflect.api.{TreeCreator, TypeCreator}
import scala.reflect.api.{Universe => ApiUniverse, Mirror => ApiMirror}
import scala.reflect.ClassTag
import scala.language.implicitConversions
import scala.language.existentials
import scala.collection.immutable.ListMap

trait Universes {
  self: Context =>

  object universe extends scala.reflect.macros.Universe with UniverseTrees with UniverseTypes with UniverseSymbols with UniverseInternals {
    // Members declared in scala.reflect.api.Annotations
    type CompilerAnnotation = c.universe.Annotation
    type OurAnnotation = Annotation
    implicit class RichCompilerAnnotation(ann: CompilerAnnotation) { def wrap = new Annotation(ann) }
    implicit class RichOurAnnotation(ann: OurAnnotation) { def unwrap = ann.ann }
    class Annotation(val ann: CompilerAnnotation) extends AnnotationApi {
      def tpe: OurType = ann.tpe.wrap
      def scalaArgs: List[OurTree] = ann.scalaArgs.map(_.wrap)
      def javaArgs: ListMap[OurName, OurJavaArgument] = ann.javaArgs.map{ case (k, v) => (k.wrap, v.wrap) }
    }
    object Annotation extends AnnotationExtractor {
      def apply(tpe: OurType, scalaArgs: List[OurTree], javaArgs: ListMap[OurName, OurJavaArgument]): OurAnnotation =
        c.universe.Annotation(tpe.unwrap, scalaArgs.map(_.unwrap), javaArgs.map { case (k, v) => (k.unwrap, v.unwrap) }).wrap
      def unapply(ann: OurAnnotation): Option[(OurType, List[OurTree], ListMap[OurName, OurJavaArgument])] =
        Some((ann.tpe, ann.scalaArgs, ann.javaArgs))
    }
    type OurJavaArgument = JavaArgument
    type CompilerJavaArgument = c.universe.JavaArgument
    implicit class RichCompilerJavaArgument(ja: CompilerJavaArgument) {
      def wrap: OurJavaArgument = {
        ???
      }
    }
    implicit class RichOurJavaArgument(ja: OurJavaArgument) { def unwrap = ja.ja }
    abstract class JavaArgument(val ja: CompilerJavaArgument) extends JavaArgumentApi
    type OurArrayArgument = ArrayArgument
    type CompilerArrayArgument = c.universe.ArrayArgument
    implicit class RichCompilerArrayArgument(aa: CompilerArrayArgument) { def wrap = new ArrayArgument(aa) }
    implicit class RichOurArrayArgument(aa: OurArrayArgument) { def unwrap = aa.aa }
    class ArrayArgument(val aa: c.universe.ArrayArgument) extends JavaArgument(aa) with ArrayArgumentApi {
      def args: Array[OurJavaArgument] = aa.args.map(_.wrap)
    }
    object ArrayArgument extends ArrayArgumentExtractor {
      def apply(args: Array[OurJavaArgument]): ArrayArgument = c.universe.ArrayArgument(args.map(_.unwrap).toArray(c.universe.JavaArgumentTag)).wrap
      def unapply(arg: ArrayArgument): Option[Array[OurJavaArgument]] = Some(arg.args)
    }
    type OurLiteralArgument = LiteralArgument
    type CompilerLiteralArgument = c.universe.LiteralArgument
    implicit class RichCompilerLiteralArgument(la: CompilerLiteralArgument) { def wrap = new LiteralArgument(la) }
    implicit class RichOurLiteralArgument(la: OurLiteralArgument) { def unwrap = la.la }
    class LiteralArgument(val la: c.universe.LiteralArgument) extends JavaArgument(la) with LiteralArgumentApi {
      def value: OurConstant = la.value.wrap
    }
    object LiteralArgument extends LiteralArgumentExtractor {
      def apply(value: OurConstant): LiteralArgument = c.universe.LiteralArgument(value.unwrap).wrap
      def unapply(arg: LiteralArgument): Option[OurConstant] = Some(arg.value)
    }
    type OurNestedArgument = NestedArgument
    type CompilerNestedArgument = c.universe.NestedArgument
    implicit class RichCompilerNestedArgument(na: CompilerNestedArgument) { def wrap = new NestedArgument(na) }
    implicit class RichOurNestedArgument(na: OurNestedArgument) { def unwrap = na.na }
    class NestedArgument(val na: c.universe.NestedArgument) extends JavaArgument(na) with NestedArgumentApi {
      def annotation: OurAnnotation = na.annotation.wrap
    }
    object NestedArgument extends NestedArgumentExtractor {
      def apply(annotation: OurAnnotation): NestedArgument = c.universe.NestedArgument(annotation.unwrap).wrap
      def unapply(arg: NestedArgument): Option[OurAnnotation] = Some(arg.annotation)
    }
    protected[scala] def annotationToTree(ann: Annotation): Tree = c.universe.annotationToTree(ann.unwrap).wrap
    protected[scala] def treeToAnnotation(tree: Tree): Annotation = c.universe.treeToAnnotation(tree.unwrap).wrap

    // Members declared in scala.reflect.api.Constants
    type CompilerConstant = c.universe.Constant
    type OurConstant = Constant
    implicit class RichCompilerConstant(const: CompilerConstant) { def wrap = new Constant(const) }
    implicit class RichOurConstant(const: OurConstant) { def unwrap = const.const }
    class Constant(val const: CompilerConstant) extends ConstantApi {
      val value: Any = const.value
      def tpe: OurType = const.tpe.wrap
    }
    object Constant extends ConstantExtractor {
      def apply(value: Any): Constant = c.universe.Constant(value).wrap
      def unapply(arg: OurConstant): Option[Any] = Some(arg.value)
    }

    // Members declared in scala.reflect.api.Exprs
    type CompilerExpr[T] = c.universe.Expr[T]
    type OurExpr[T] = Expr[T]
    implicit class RichCompilerExpr[T](expr: CompilerExpr[T]) { def wrap = createExpr[T](expr.tree.wrap)(createTypeTag[T](expr.staticType.wrap)) }
    implicit class RichOurExpr[T](expr: OurExpr[T]) { def unwrap = c.universe.Expr[T](c.mirror, FixedMirrorTreeCreator(c.mirror, expr.tree.unwrap)) }
    def createExpr[T: OurWeakTypeTag](tree: OurTree): OurExpr[T] = Expr[T](mirror, FixedMirrorTreeCreator(mirror, tree))
    case class FixedMirrorTreeCreator(mirror: ApiMirror[_ <: ApiUniverse with Singleton], tree: Any) extends TreeCreator {
      def apply[U <: ApiUniverse with Singleton](m: ApiMirror[U]): U # Tree =
        if (m eq mirror) tree.asInstanceOf[U # Tree]
        else throw new IllegalArgumentException(s"Expr defined in $mirror cannot be migrated to other mirrors.")
    }

    // Members declared in scala.reflect.api.FlagSets
    type CompilerFlagSet = c.universe.FlagSet
    type OurFlagSet = FlagSet
    implicit class RichCompilerFlagSet(fs: CompilerFlagSet) { def wrap = fs }
    implicit class RichOurFlagSet(fs: OurFlagSet) { def unwrap = fs }
    type FlagSet = c.universe.FlagSet
    val NoFlags: FlagSet = c.universe.NoFlags
    implicit def addFlagOps(left: OurFlagSet): FlagOps = new FlagOps {
      def | (right: OurFlagSet): OurFlagSet = (left.unwrap | right.unwrap).wrap
    }
    object Flag extends FlagValues {
      val TRAIT = c.universe.Flag.TRAIT.wrap
      val INTERFACE = c.universe.Flag.INTERFACE.wrap
      val MUTABLE = c.universe.Flag.MUTABLE.wrap
      val MACRO = c.universe.Flag.MACRO.wrap
      val DEFERRED = c.universe.Flag.DEFERRED.wrap
      val ABSTRACT = c.universe.Flag.ABSTRACT.wrap
      val FINAL = c.universe.Flag.FINAL.wrap
      val SEALED = c.universe.Flag.SEALED.wrap
      val IMPLICIT = c.universe.Flag.IMPLICIT.wrap
      val LAZY = c.universe.Flag.LAZY.wrap
      val OVERRIDE = c.universe.Flag.OVERRIDE.wrap
      val PRIVATE = c.universe.Flag.PRIVATE.wrap
      val PROTECTED = c.universe.Flag.PROTECTED.wrap
      val LOCAL = c.universe.Flag.LOCAL.wrap
      val CASE = c.universe.Flag.CASE.wrap
      val ABSOVERRIDE = c.universe.Flag.ABSOVERRIDE.wrap
      val BYNAMEPARAM = c.universe.Flag.BYNAMEPARAM.wrap
      val PARAM = c.universe.Flag.PARAM.wrap
      val COVARIANT = c.universe.Flag.COVARIANT.wrap
      val CONTRAVARIANT = c.universe.Flag.CONTRAVARIANT.wrap
      val DEFAULTPARAM = c.universe.Flag.DEFAULTPARAM.wrap
      val PRESUPER = c.universe.Flag.PRESUPER.wrap
      val DEFAULTINIT = c.universe.Flag.DEFAULTINIT.wrap
      val ENUM = c.universe.Flag.ENUM.wrap
      val PARAMACCESSOR = c.universe.Flag.PARAMACCESSOR.wrap
      val CASEACCESSOR = c.universe.Flag.CASEACCESSOR.wrap
      val SYNTHETIC = c.universe.Flag.SYNTHETIC.wrap
      val ARTIFACT = c.universe.Flag.ARTIFACT.wrap
      val STABLE = c.universe.Flag.STABLE.wrap
    }

    // Members declared in scala.reflect.api.ImplicitTags
    implicit val AnnotatedTypeTag: ClassTag[AnnotatedType] = ClassTag[AnnotatedType](classOf[AnnotatedType])
    implicit val BoundedWildcardTypeTag: ClassTag[BoundedWildcardType] = ClassTag[BoundedWildcardType](classOf[BoundedWildcardType])
    implicit val ClassInfoTypeTag: ClassTag[ClassInfoType] = ClassTag[ClassInfoType](classOf[ClassInfoType])
    implicit val CompoundTypeTag: ClassTag[CompoundType] = ClassTag[CompoundType](classOf[CompoundType])
    implicit val ConstantTypeTag: ClassTag[ConstantType] = ClassTag[ConstantType](classOf[ConstantType])
    implicit val ExistentialTypeTag: ClassTag[ExistentialType] = ClassTag[ExistentialType](classOf[ExistentialType])
    implicit val MethodTypeTag: ClassTag[MethodType] = ClassTag[MethodType](classOf[MethodType])
    implicit val NullaryMethodTypeTag: ClassTag[NullaryMethodType] = ClassTag[NullaryMethodType](classOf[NullaryMethodType])
    implicit val PolyTypeTag: ClassTag[PolyType] = ClassTag[PolyType](classOf[PolyType])
    implicit val RefinedTypeTag: ClassTag[RefinedType] = ClassTag[RefinedType](classOf[RefinedType])
    implicit val SingleTypeTag: ClassTag[SingleType] = ClassTag[SingleType](classOf[SingleType])
    implicit val SingletonTypeTag: ClassTag[SingletonType] = ClassTag[SingletonType](classOf[SingletonType])
    implicit val SuperTypeTag: ClassTag[SuperType] = ClassTag[SuperType](classOf[SuperType])
    implicit val ThisTypeTag: ClassTag[ThisType] = ClassTag[ThisType](classOf[ThisType])
    implicit val TypeBoundsTag: ClassTag[TypeBounds] = ClassTag[TypeBounds](classOf[TypeBounds])
    implicit val TypeRefTag: ClassTag[TypeRef] = ClassTag[TypeRef](classOf[TypeRef])
    implicit val TypeTagg: ClassTag[Type] = ClassTag[Type](classOf[Type])
    implicit val NameTag: ClassTag[Name] = ClassTag[Name](classOf[Name])
    implicit val TermNameTag: ClassTag[TermName] = ClassTag[TermName](classOf[TermName])
    implicit val TypeNameTag: ClassTag[TypeName] = ClassTag[TypeName](classOf[TypeName])
    implicit val ScopeTag: ClassTag[Scope] = ClassTag[Scope](classOf[Scope])
    implicit val MemberScopeTag: ClassTag[MemberScope] = ClassTag[MemberScope](classOf[MemberScope])
    implicit val AnnotationTag: ClassTag[Annotation] = ClassTag[Annotation](classOf[Annotation])
    implicit val JavaArgumentTag: ClassTag[JavaArgument] = ClassTag[JavaArgument](classOf[JavaArgument])
    implicit val LiteralArgumentTag: ClassTag[LiteralArgument] = ClassTag[LiteralArgument](classOf[LiteralArgument])
    implicit val ArrayArgumentTag: ClassTag[ArrayArgument] = ClassTag[ArrayArgument](classOf[ArrayArgument])
    implicit val NestedArgumentTag: ClassTag[NestedArgument] = ClassTag[NestedArgument](classOf[NestedArgument])
    implicit val TermSymbolTag: ClassTag[TermSymbol] = ClassTag[TermSymbol](classOf[TermSymbol])
    implicit val MethodSymbolTag: ClassTag[MethodSymbol] = ClassTag[MethodSymbol](classOf[MethodSymbol])
    implicit val SymbolTag: ClassTag[Symbol] = ClassTag[Symbol](classOf[Symbol])
    implicit val TypeSymbolTag: ClassTag[TypeSymbol] = ClassTag[TypeSymbol](classOf[TypeSymbol])
    implicit val ModuleSymbolTag: ClassTag[ModuleSymbol] = ClassTag[ModuleSymbol](classOf[ModuleSymbol])
    implicit val ClassSymbolTag: ClassTag[ClassSymbol] = ClassTag[ClassSymbol](classOf[ClassSymbol])
    implicit val PositionTag: ClassTag[Position] = c.universe.PositionTag
    implicit val ConstantTag: ClassTag[Constant] = ClassTag[Constant](classOf[Constant])
    implicit val FlagSetTag: ClassTag[FlagSet] = c.universe.FlagSetTag
    implicit val ModifiersTag: ClassTag[Modifiers] = ClassTag[Modifiers](classOf[Modifiers])
    implicit val AlternativeTag: ClassTag[Alternative] = ClassTag[Alternative](classOf[Alternative])
    implicit val AnnotatedTag: ClassTag[Annotated] = ClassTag[Annotated](classOf[Annotated])
    implicit val AppliedTypeTreeTag: ClassTag[AppliedTypeTree] = ClassTag[AppliedTypeTree](classOf[AppliedTypeTree])
    implicit val ApplyTag: ClassTag[Apply] = ClassTag[Apply](classOf[Apply])
    implicit val AssignOrNamedArgTag: ClassTag[AssignOrNamedArg] = ClassTag[AssignOrNamedArg](classOf[AssignOrNamedArg])
    implicit val AssignTag: ClassTag[Assign] = ClassTag[Assign](classOf[Assign])
    implicit val BindTag: ClassTag[Bind] = ClassTag[Bind](classOf[Bind])
    implicit val BlockTag: ClassTag[Block] = ClassTag[Block](classOf[Block])
    implicit val CaseDefTag: ClassTag[CaseDef] = ClassTag[CaseDef](classOf[CaseDef])
    implicit val ClassDefTag: ClassTag[ClassDef] = ClassTag[ClassDef](classOf[ClassDef])
    implicit val CompoundTypeTreeTag: ClassTag[CompoundTypeTree] = ClassTag[CompoundTypeTree](classOf[CompoundTypeTree])
    implicit val DefDefTag: ClassTag[DefDef] = ClassTag[DefDef](classOf[DefDef])
    implicit val DefTreeTag: ClassTag[DefTree] = ClassTag[DefTree](classOf[DefTree])
    implicit val ExistentialTypeTreeTag: ClassTag[ExistentialTypeTree] = ClassTag[ExistentialTypeTree](classOf[ExistentialTypeTree])
    implicit val FunctionTag: ClassTag[Function] = ClassTag[Function](classOf[Function])
    implicit val GenericApplyTag: ClassTag[GenericApply] = ClassTag[GenericApply](classOf[GenericApply])
    implicit val IdentTag: ClassTag[Ident] = ClassTag[Ident](classOf[Ident])
    implicit val IfTag: ClassTag[If] = ClassTag[If](classOf[If])
    implicit val ImplDefTag: ClassTag[ImplDef] = ClassTag[ImplDef](classOf[ImplDef])
    implicit val ImportSelectorTag: ClassTag[ImportSelector] = ClassTag[ImportSelector](classOf[ImportSelector])
    implicit val ImportTag: ClassTag[Import] = ClassTag[Import](classOf[Import])
    implicit val LabelDefTag: ClassTag[LabelDef] = ClassTag[LabelDef](classOf[LabelDef])
    implicit val LiteralTag: ClassTag[Literal] = ClassTag[Literal](classOf[Literal])
    implicit val MatchTag: ClassTag[Match] = ClassTag[Match](classOf[Match])
    implicit val MemberDefTag: ClassTag[MemberDef] = ClassTag[MemberDef](classOf[MemberDef])
    implicit val ModuleDefTag: ClassTag[ModuleDef] = ClassTag[ModuleDef](classOf[ModuleDef])
    implicit val NameTreeTag: ClassTag[NameTree] = ClassTag[NameTree](classOf[NameTree])
    implicit val NewTag: ClassTag[New] = ClassTag[New](classOf[New])
    implicit val PackageDefTag: ClassTag[PackageDef] = ClassTag[PackageDef](classOf[PackageDef])
    implicit val RefTreeTag: ClassTag[RefTree] = ClassTag[RefTree](classOf[RefTree])
    implicit val ReturnTag: ClassTag[Return] = ClassTag[Return](classOf[Return])
    implicit val SelectFromTypeTreeTag: ClassTag[SelectFromTypeTree] = ClassTag[SelectFromTypeTree](classOf[SelectFromTypeTree])
    implicit val SelectTag: ClassTag[Select] = ClassTag[Select](classOf[Select])
    implicit val SingletonTypeTreeTag: ClassTag[SingletonTypeTree] = ClassTag[SingletonTypeTree](classOf[SingletonTypeTree])
    implicit val StarTag: ClassTag[Star] = ClassTag[Star](classOf[Star])
    implicit val SuperTag: ClassTag[Super] = ClassTag[Super](classOf[Super])
    implicit val SymTreeTag: ClassTag[SymTree] = ClassTag[SymTree](classOf[SymTree])
    implicit val TemplateTag: ClassTag[Template] = ClassTag[Template](classOf[Template])
    implicit val TermTreeTag: ClassTag[TermTree] = ClassTag[TermTree](classOf[TermTree])
    implicit val ThisTag: ClassTag[This] = ClassTag[This](classOf[This])
    implicit val ThrowTag: ClassTag[Throw] = ClassTag[Throw](classOf[Throw])
    implicit val TreeTag: ClassTag[Tree] = ClassTag[Tree](classOf[Tree])
    implicit val TryTag: ClassTag[Try] = ClassTag[Try](classOf[Try])
    implicit val TypTreeTag: ClassTag[TypTree] = ClassTag[TypTree](classOf[TypTree])
    implicit val TypeApplyTag: ClassTag[TypeApply] = ClassTag[TypeApply](classOf[TypeApply])
    implicit val TypeBoundsTreeTag: ClassTag[TypeBoundsTree] = ClassTag[TypeBoundsTree](classOf[TypeBoundsTree])
    implicit val TypeDefTag: ClassTag[TypeDef] = ClassTag[TypeDef](classOf[TypeDef])
    implicit val TypeTreeTag: ClassTag[TypeTree] = ClassTag[TypeTree](classOf[TypeTree])
    implicit val TypedTag: ClassTag[Typed] = ClassTag[Typed](classOf[Typed])
    implicit val UnApplyTag: ClassTag[UnApply] = ClassTag[UnApply](classOf[UnApply])
    implicit val ValDefTag: ClassTag[ValDef] = ClassTag[ValDef](classOf[ValDef])
    implicit val ValOrDefDefTag: ClassTag[ValOrDefDef] = ClassTag[ValOrDefDef](classOf[ValOrDefDef])
    implicit val TreeCopierTag: ClassTag[TreeCopier] = ClassTag[TreeCopier](classOf[TreeCopier])
    implicit val RuntimeClassTag: ClassTag[RuntimeClass] = c.universe.RuntimeClassTag
    implicit val MirrorTag: ClassTag[Mirror] = ClassTag[Mirror](classOf[Mirror])
    implicit val FreeTermSymbolTag: scala.reflect.ClassTag[FreeTermSymbol] = ClassTag[FreeTermSymbol](classOf[FreeTermSymbol])
    implicit val FreeTypeSymbolTag: scala.reflect.ClassTag[FreeTypeSymbol] = ClassTag[FreeTypeSymbol](classOf[FreeTypeSymbol])
    implicit val ReferenceToBoxedTag: scala.reflect.ClassTag[ReferenceToBoxed] = ClassTag[ReferenceToBoxed](classOf[ReferenceToBoxed])

    // Members declared in scala.reflect.api.Internals
    // see Internals.scala

    // Members declared in scala.reflect.api.Mirrors
    type CompilerMirror = c.universe.Mirror
    type OurMirror = Mirror
    implicit class RichCompilerMirror(m: CompilerMirror) { def wrap = new Mirror(m) }
    implicit class RichOurMirror(m: OurMirror) { def unwrap = m.m }
    class Mirror(val m: CompilerMirror) extends scala.reflect.api.Mirror[self.universe.type] {
      lazy val universe: self.universe.type = self.universe
      def RootClass: OurClassSymbol = m.RootClass.wrap
      def RootPackage: OurModuleSymbol = m.RootPackage.wrap
      def EmptyPackageClass: OurClassSymbol = m.EmptyPackageClass.wrap
      def EmptyPackage: OurModuleSymbol = m.EmptyPackage.wrap
      def staticClass(fullName: String): OurClassSymbol = m.staticClass(fullName).wrap
      def staticModule(fullName: String): OurModuleSymbol = m.staticModule(fullName).wrap
      def staticPackage(fullName: String): OurModuleSymbol = m.staticPackage(fullName).wrap
    }
    val rootMirror: Mirror = c.universe.rootMirror.wrap
    type RuntimeClass = c.universe.RuntimeClass

    // Members declared in scala.reflect.api.Names
    type CompilerName = c.universe.Name
    type OurName = Name
    implicit class RichCompilerName(n: CompilerName) { def wrap = if (n.isTermName) TermName(n.toString) else TypeName(n.toString) }
    implicit class RichOurName(n: OurName) { def unwrap = if (n.isTermName) c.universe.TermName(n.toString) else c.universe.TypeName(n.toString) }
    abstract class Name(n: CompilerName) extends NameApi {
      def isTermName: Boolean = n.isTermName
      def isTypeName: Boolean = n.isTypeName
      def toTermName: OurTermName = n.toTermName.wrap.asInstanceOf[OurTermName]
      def toTypeName: OurTypeName = n.toTypeName.wrap.asInstanceOf[OurTypeName]
      def decoded: String = n.decoded
      def encoded: String = n.encoded
      def decodedName: OurName = n.decodedName.wrap
      def encodedName: OurName = n.encodedName.wrap
      override def toString = n.toString
    }
    type CompilerTermName = c.universe.TermName
    type OurTermName = TermName
    implicit class RichCompilerTermName(n: CompilerTermName) { def wrap = TermName(n.toString) }
    implicit class RichOurTermName(n: OurTermName) { def unwrap = c.universe.TermName(n.toString) }
    class TermName(n: CompilerTermName) extends Name(n) with TermNameApi
    object TermName extends TermNameExtractor {
      def apply(s: String): OurTermName = new TermName(c.universe.TermName(s))
      def unapply(name: OurTermName): Option[String] = Some(name.toString)
    }
    type CompilerTypeName = c.universe.TypeName
    type OurTypeName = TypeName
    implicit class RichCompilerTypeName(n: CompilerTypeName) { def wrap = TypeName(n.toString) }
    implicit class RichOurTypeName(n: OurTypeName) { def unwrap = c.universe.TypeName(n.toString) }
    class TypeName(n: CompilerTypeName) extends Name(n) with TypeNameApi
    object TypeName extends TypeNameExtractor {
      def apply(s: String): OurTypeName = new TypeName(c.universe.TypeName(s))
      def unapply(name: OurTypeName): Option[String] = Some(name.toString)
    }
    def newTermName(s: String): OurTermName = c.universe.newTermName(s).wrap.asInstanceOf[OurTermName]
    def newTypeName(s: String): OurTypeName = c.universe.newTypeName(s).wrap.asInstanceOf[OurTypeName]

    // Members declared in scala.reflect.api.Positions
    type CompilerPosition = c.universe.Position
    type OurPosition = Position
    implicit class RichCompilerPosition(p: CompilerPosition) { def wrap = p }
    implicit class RichOurPosition(p: OurPosition) { def unwrap = p }
    type Position = c.Position
    val NoPosition: OurPosition = c.universe.NoPosition.wrap
    def atPos[T <: OurTree](pos: OurPosition)(tree: T): T = c.universe.atPos(pos.unwrap)(tree.unwrap).wrap.asInstanceOf[T]
    def wrappingPos(trees: List[OurTree]): OurPosition = c.universe.wrappingPos(trees.map(_.unwrap)).wrap
    def wrappingPos(default: OurPosition, trees: List[OurTree]): OurPosition = c.universe.wrappingPos(default.unwrap, trees.map(_.unwrap)).wrap

    // Members declared in scala.reflect.api.Printers
    protected def newCodePrinter(out: java.io.PrintWriter,tree: Tree,printRootPkg: Boolean): TreePrinter = sys.error("this couldn't have happened") // NOTE: never called when showCode is overridden
    override def showCode(tree: Tree, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printOwners: BooleanFlag = None, printPositions: BooleanFlag = None, printRootPkg: Boolean = false) =
      c.universe.showCode(tree.unwrap, c.universe.BooleanFlag(printTypes.value), c.universe.BooleanFlag(printIds.value), c.universe.BooleanFlag(printOwners.value), c.universe.BooleanFlag(printPositions.value), printRootPkg)
    protected def newRawTreePrinter(out: java.io.PrintWriter): TreePrinter = sys.error("this couldn't have happened") // NOTE: never called when showRaw is overridden
    override def showRaw(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printOwners: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None, printPositions: BooleanFlag = None): String =
      c.universe.showRaw(unwrapAny(any), c.universe.BooleanFlag(printTypes.value), c.universe.BooleanFlag(printIds.value), c.universe.BooleanFlag(printOwners.value), c.universe.BooleanFlag(printKinds.value), c.universe.BooleanFlag(printMirrors.value), c.universe.BooleanFlag(printPositions.value))
    protected def newTreePrinter(out: java.io.PrintWriter): TreePrinter = sys.error("this couldn't have happened") // NOTE: never called when show is overridden
    override def show(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printOwners: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None, printPositions: BooleanFlag = None): String =
      c.universe.show(unwrapAny(any), c.universe.BooleanFlag(printTypes.value), c.universe.BooleanFlag(printIds.value), c.universe.BooleanFlag(printOwners.value), c.universe.BooleanFlag(printKinds.value), c.universe.BooleanFlag(printMirrors.value), c.universe.BooleanFlag(printPositions.value))
    def show(position: Position): String = c.universe.show(position.unwrap)
    def show(flags: FlagSet): String = c.universe.show(flags)
    def show(name: Name): String = c.universe.show(name.unwrap)
    def showDecl(sym: Symbol): String = c.universe.showDecl(sym.unwrap)

    // Members declared in scala.reflect.api.Scopes
    type CompilerScope = c.universe.Scope
    type OurScope = Scope
    implicit class RichCompilerScope(s: CompilerScope) { def wrap = new OurScope(s) }
    implicit class RichOurScope(s: OurScope) { def unwrap = s.s0 }
    class Scope(val s0: CompilerScope) extends ScopeApi {
      def iterator: Iterator[OurSymbol] = s0.iterator.map(_.wrap)
    }
    type CompilerMemberScope = c.universe.MemberScope
    type OurMemberScope = MemberScope
    implicit class RichCompilerMemberScope(s: CompilerMemberScope) { def wrap = new OurMemberScope(s) }
    implicit class RichOurMemberScope(s: OurMemberScope) { def unwrap = s.s1 }
    class MemberScope(val s1: CompilerMemberScope) extends Scope(s1) with MemberScopeApi {
      def sorted: List[OurSymbol] = s1.sorted.map(_.wrap)
    }

    // Members declared in scala.reflect.api.StandardDefinitions
    object definitions extends DefinitionsApi {
      def ScalaPackageClass: OurClassSymbol = c.universe.definitions.ScalaPackageClass.wrap
      def ScalaPackage: OurModuleSymbol = c.universe.definitions.ScalaPackage.wrap
      def AnyClass   : OurClassSymbol = c.universe.definitions.AnyClass.wrap
      def AnyValClass: OurClassSymbol = c.universe.definitions.AnyValClass.wrap
      def ObjectClass: OurClassSymbol = c.universe.definitions.ObjectClass.wrap
      def AnyRefClass: OurTypeSymbol = c.universe.definitions.AnyRefClass.wrap
      def NullClass   : OurClassSymbol = c.universe.definitions.NullClass.wrap
      def NothingClass: OurClassSymbol = c.universe.definitions.NothingClass.wrap
      def UnitClass   : OurClassSymbol = c.universe.definitions.UnitClass.wrap
      def ByteClass   : OurClassSymbol = c.universe.definitions.ByteClass.wrap
      def ShortClass  : OurClassSymbol = c.universe.definitions.ShortClass.wrap
      def CharClass   : OurClassSymbol = c.universe.definitions.CharClass.wrap
      def IntClass    : OurClassSymbol = c.universe.definitions.IntClass.wrap
      def LongClass   : OurClassSymbol = c.universe.definitions.LongClass.wrap
      def FloatClass  : OurClassSymbol = c.universe.definitions.FloatClass.wrap
      def DoubleClass : OurClassSymbol = c.universe.definitions.DoubleClass.wrap
      def BooleanClass: OurClassSymbol = c.universe.definitions.BooleanClass.wrap
      def StringClass : OurClassSymbol = c.universe.definitions.StringClass.wrap
      def ClassClass  : OurClassSymbol = c.universe.definitions.ClassClass.wrap
      def ArrayClass  : OurClassSymbol = c.universe.definitions.ArrayClass.wrap
      def ListClass   : OurClassSymbol = c.universe.definitions.ListClass.wrap
      def PredefModule: OurModuleSymbol = c.universe.definitions.PredefModule.wrap
      def JavaLangPackageClass: OurClassSymbol = c.universe.definitions.JavaLangPackageClass.wrap
      def JavaLangPackage: OurModuleSymbol = c.universe.definitions.JavaLangPackage.wrap
      def ArrayModule: OurModuleSymbol = c.universe.definitions.ArrayModule.wrap
      def ArrayModule_overloadedApply: OurTermSymbol = c.universe.definitions.ArrayModule_overloadedApply.wrap
      def Array_apply: OurTermSymbol = c.universe.definitions.Array_apply.wrap
      def Array_clone: OurTermSymbol = c.universe.definitions.Array_clone.wrap
      def Array_length: OurTermSymbol = c.universe.definitions.Array_length.wrap
      def Array_update: OurTermSymbol = c.universe.definitions.Array_update.wrap
      def ByNameParamClass: OurClassSymbol = c.universe.definitions.ByNameParamClass.wrap
      def JavaRepeatedParamClass: OurClassSymbol = c.universe.definitions.JavaRepeatedParamClass.wrap
      def RepeatedParamClass: OurClassSymbol = c.universe.definitions.RepeatedParamClass.wrap
      def ListModule: OurModuleSymbol = c.universe.definitions.ListModule.wrap
      def List_apply: OurTermSymbol = c.universe.definitions.List_apply.wrap
      def NilModule: OurModuleSymbol = c.universe.definitions.NilModule.wrap
      def OptionClass: OurClassSymbol = c.universe.definitions.OptionClass.wrap
      def NoneModule: OurModuleSymbol = c.universe.definitions.NoneModule.wrap
      def SomeModule: OurModuleSymbol = c.universe.definitions.SomeModule.wrap
      def ProductClass: VarArityClassApi = new VarArityClassApi {
        def seq: Seq[OurClassSymbol] = c.universe.definitions.ProductClass.seq.map(_.wrap)
        def apply(x: Int): Symbol = c.universe.definitions.ProductClass(x).wrap
      }
      def FunctionClass: VarArityClassApi = new VarArityClassApi {
        def seq: Seq[OurClassSymbol] = c.universe.definitions.FunctionClass.seq.map(_.wrap)
        def apply(x: Int): Symbol = c.universe.definitions.FunctionClass(x).wrap
      }
      def TupleClass: VarArityClassApi = new VarArityClassApi {
        def seq: Seq[OurClassSymbol] = c.universe.definitions.TupleClass.seq.map(_.wrap)
        def apply(x: Int): Symbol = c.universe.definitions.TupleClass(x).wrap
      }
      def ScalaPrimitiveValueClasses: List[ClassSymbol] = c.universe.definitions.ScalaPrimitiveValueClasses.map(_.wrap)
      def ScalaNumericValueClasses: List[ClassSymbol] = c.universe.definitions.ScalaNumericValueClasses.map(_.wrap)
      val UnitTpe: OurType = c.universe.definitions.UnitTpe.wrap
      val ByteTpe: OurType = c.universe.definitions.ByteTpe.wrap
      val ShortTpe: OurType = c.universe.definitions.ShortTpe.wrap
      val CharTpe: OurType = c.universe.definitions.CharTpe.wrap
      val IntTpe: OurType = c.universe.definitions.IntTpe.wrap
      val LongTpe: OurType = c.universe.definitions.LongTpe.wrap
      val FloatTpe: OurType = c.universe.definitions.FloatTpe.wrap
      val DoubleTpe: OurType = c.universe.definitions.DoubleTpe.wrap
      val BooleanTpe: OurType = c.universe.definitions.BooleanTpe.wrap
      val AnyTpe: OurType = c.universe.definitions.AnyTpe.wrap
      val AnyValTpe: OurType = c.universe.definitions.AnyValTpe.wrap
      val AnyRefTpe: OurType = c.universe.definitions.AnyRefTpe.wrap
      val ObjectTpe: OurType = c.universe.definitions.ObjectTpe.wrap
      val NothingTpe: OurType = c.universe.definitions.NothingTpe.wrap
      val NullTpe: OurType = c.universe.definitions.NullTpe.wrap
    }

    // Members declared in scala.reflect.api.StandardNames
    val nme: TermNamesApi = termNames
    object termNames extends TermNamesApi {
      val WILDCARD: OurTermName = c.universe.termNames.WILDCARD.wrap
      val EMPTY: OurTermName = c.universe.termNames.EMPTY.wrap
      val ERROR: OurTermName = c.universe.termNames.ERROR.wrap
      val PACKAGE: OurTermName = c.universe.termNames.PACKAGE.wrap
      val CONSTRUCTOR: OurTermName = c.universe.termNames.CONSTRUCTOR.wrap
      val ROOTPKG: OurTermName = c.universe.termNames.ROOTPKG.wrap
      val EMPTY_PACKAGE_NAME: OurTermName = c.universe.termNames.EMPTY_PACKAGE_NAME.wrap
      val LOCAL_SUFFIX_STRING: String = c.universe.termNames.LOCAL_SUFFIX_STRING
    }
    val tpnme: TypeNamesApi = typeNames
    object typeNames extends TypeNamesApi {
      val WILDCARD: OurTypeName = c.universe.typeNames.WILDCARD.wrap
      val EMPTY: OurTypeName = c.universe.typeNames.EMPTY.wrap
      val ERROR: OurTypeName = c.universe.typeNames.ERROR.wrap
      val PACKAGE: OurTypeName = c.universe.typeNames.PACKAGE.wrap
      val WILDCARD_STAR: OurTypeName = c.universe.typeNames.WILDCARD_STAR.wrap
    }

    // Members declared in scala.reflect.api.Symbols
    // see Symbols.scala

    // Members declared in scala.reflect.api.Trees
    // see Trees.scala

    // Members declared in scala.reflect.api.Types
    // see Types.scala

    // Members declared in scala.reflect.api.TypeTags
    type CompilerWeakTypeTag[T] = c.universe.WeakTypeTag[T]
    type OurWeakTypeTag[T] = WeakTypeTag[T]
    implicit class RichCompilerWeakTypeTag[T](tt: CompilerWeakTypeTag[T]) { def wrap = createWeakTypeTag[T](tt.tpe.wrap) }
    implicit class RichOurWeakTypeTag[T](tt: OurWeakTypeTag[T]) { def unwrap = c.universe.WeakTypeTag[T](c.mirror, FixedMirrorTypeCreator(c.mirror, tt.tpe.unwrap)) }
    def createWeakTypeTag[T](tpe: OurType): OurWeakTypeTag[T] = WeakTypeTag[T](mirror, FixedMirrorTypeCreator(mirror, tpe))
    type CompilerTypeTag[T] = c.universe.TypeTag[T]
    type OurTypeTag[T] = TypeTag[T]
    implicit class RichCompilerTypeTag[T](tt: CompilerTypeTag[T]) { def wrap = createTypeTag[T](tt.tpe.wrap) }
    implicit class RichOurTypeTag[T](tt: OurTypeTag[T]) { def unwrap = c.universe.TypeTag[T](c.mirror, FixedMirrorTypeCreator(c.mirror, tt.tpe.unwrap)) }
    def createTypeTag[T](tpe: OurType): OurTypeTag[T] = TypeTag[T](mirror, FixedMirrorTypeCreator(mirror, tpe))
    def symbolOf[T: WeakTypeTag]: TypeSymbol = c.universe.symbolOf[T](implicitly[WeakTypeTag[T]].unwrap).wrap
    case class FixedMirrorTypeCreator(mirror: ApiMirror[_ <: ApiUniverse with Singleton], tpe: Any) extends TypeCreator {
      def apply[U <: ApiUniverse with Singleton](m: ApiMirror[U]): U # Type =
        if (m eq mirror) tpe.asInstanceOf[U # Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    }

    // Members declared in scala.reflect.macros.Universe
    type CompilerRun = c.universe.Run
    type OurRun = Run
    implicit class RichCompilerRun(r: CompilerRun) { def wrap = new Run(r) }
    class Run(r: CompilerRun) extends RunContextApi {
      def currentUnit: OurCompilationUnit = r.currentUnit.wrap
      def units: Iterator[OurCompilationUnit] = r.units.map(_.wrap)
    }
    type CompilerCompilationUnit = c.universe.CompilationUnit
    type OurCompilationUnit = CompilationUnit
    implicit class RichCompilerCompilationUnit(cu: CompilerCompilationUnit) { def wrap = new CompilationUnit(cu) }
    class CompilationUnit(cu: CompilerCompilationUnit) extends CompilationUnitContextApi {
      def source = cu.source
      def body = cu.body.wrap
    }
    val treeBuild: TreeGen = new TreeGen {
      def mkAttributedIdent(sym: OurSymbol): RefTree = c.universe.treeBuild.mkAttributedIdent(sym.unwrap).wrap
      def mkAttributedQualifier(tpe: OurType, termSym: OurSymbol): OurTree = c.universe.treeBuild.mkAttributedQualifier(tpe.unwrap, termSym.unwrap).wrap
      def mkAttributedQualifier(tpe: OurType): Tree = c.universe.treeBuild.mkAttributedQualifier(tpe.unwrap).wrap
      def mkAttributedRef(sym: OurSymbol): RefTree = c.universe.treeBuild.mkAttributedRef(sym.unwrap).wrap
      def mkAttributedRef(pre: OurType, sym: OurSymbol): RefTree = c.universe.treeBuild.mkAttributedRef(pre.unwrap, sym.unwrap).wrap
      def mkAttributedSelect(qual: OurTree, sym: OurSymbol): RefTree = c.universe.treeBuild.mkAttributedSelect(qual.unwrap, sym.unwrap).wrap
      def mkAttributedStableRef(sym: OurSymbol): OurTree = c.universe.treeBuild.mkAttributedStableRef(sym.unwrap).wrap
      def mkAttributedStableRef(pre: OurType, sym: OurSymbol): Tree = c.universe.treeBuild.mkAttributedStableRef(pre.unwrap, sym.unwrap).wrap
      def mkAttributedThis(sym: OurSymbol): This = c.universe.treeBuild.mkAttributedThis(sym.unwrap).wrap
      def mkCast(tree: OurTree, pt: OurType): Tree = c.universe.treeBuild.mkCast(tree.unwrap, pt.unwrap).wrap
      def mkMethodCall(target: OurTree, targs: List[OurType], args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(target.unwrap, targs.map(_.unwrap), args.map(_.unwrap)).wrap
      def mkMethodCall(receiver: OurTree, method: OurSymbol, targs: List[OurType], args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(receiver.unwrap, method.unwrap, targs.map(_.unwrap), args.map(_.unwrap)).wrap
      def mkMethodCall(receiver: OurSymbol, methodName: OurName, args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(receiver.unwrap, methodName.unwrap, args.map(_.unwrap)).wrap
      def mkMethodCall(target: OurTree, args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(target.unwrap, args.map(_.unwrap)).wrap
      def mkMethodCall(method: OurSymbol, args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(method.unwrap, args.map(_.unwrap)).wrap
      def mkMethodCall(method: OurSymbol, targs: List[OurType], args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(method.unwrap, targs.map(_.unwrap), args.map(_.unwrap)).wrap
      def mkMethodCall(receiver: OurSymbol, methodName: OurName, targs: List[OurType], args: List[OurTree]): OurTree = c.universe.treeBuild.mkMethodCall(receiver.unwrap, methodName.unwrap, targs.map(_.unwrap), args.map(_.unwrap)).wrap
      def mkNullaryCall(method: OurSymbol, targs: List[Type]): OurTree = c.universe.treeBuild.mkNullaryCall(method.unwrap, targs.map(_.unwrap)).wrap
      def mkRuntimeUniverseRef: OurTree = c.universe.treeBuild.mkRuntimeUniverseRef.wrap
      def mkUnattributedRef(fullName: OurName): RefTree = c.universe.treeBuild.mkUnattributedRef(fullName.unwrap).wrap
      def mkUnattributedRef(sym: OurSymbol): RefTree = c.universe.treeBuild.mkUnattributedRef(sym.unwrap).wrap
      def mkZero(tp: OurType): OurTree = c.universe.treeBuild.mkZero(tp.unwrap).wrap
      def stabilize(tree: OurTree): OurTree = c.universe.treeBuild.stabilize(tree.unwrap).wrap
    }

    // TODO: so hack much amaze
    def unwrapAny(any: Any): Any = any match {
      case null => null
      case t: Tree => t.unwrap
      case e: Expr[_] => e.unwrap
      case t: Type => t.unwrap
      case tt: TypeTag[_] => tt.unwrap
      case tt: WeakTypeTag[_] => tt.unwrap
      case s: Symbol => s.unwrap
      case xs: List[_] => xs.map(unwrapAny)
      case _ => throw new Exception(s"don't know how to unwrap $any (${c.universe.showRaw(any)}) of class ${any.getClass}")
    }
  }
}