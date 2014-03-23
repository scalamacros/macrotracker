package scala.tools.nsc.palladium
package contexts

trait Types {
  self: Context =>

  trait UniverseTypes {
    self: universe.type =>

    type CompilerType = c.universe.Type
    type OurType = Type
    implicit class RichCompilerType(tpe: CompilerType) {
      def wrap: OurType = tpe match {
        case null => null
        case c.universe.NoType => NoType
        case c.universe.NoPrefix => NoPrefix
        case c.universe.WildcardType => WildcardType
        case tpe if c.universe.AnnotatedTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.AnnotatedType].wrap
        case tpe if c.universe.BoundedWildcardTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.BoundedWildcardType].wrap
        case tpe if c.universe.ClassInfoTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.ClassInfoType].wrap
        case tpe if c.universe.ConstantTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.ConstantType].wrap
        case tpe if c.universe.ExistentialTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.ExistentialType].wrap
        case tpe if c.universe.MethodTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.MethodType].wrap
        case tpe if c.universe.NullaryMethodTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.NullaryMethodType].wrap
        case tpe if c.universe.PolyTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.PolyType].wrap
        case tpe if c.universe.RefinedTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.RefinedType].wrap
        case tpe if c.universe.SingleTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.SingleType].wrap
        case tpe if c.universe.SuperTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.SuperType].wrap
        case tpe if c.universe.ThisTypeTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.ThisType].wrap
        case tpe if c.universe.TypeBoundsTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.TypeBounds].wrap
        case tpe if c.universe.TypeRefTag.runtimeClass.isInstance(tpe) => tpe.asInstanceOf[c.universe.TypeRef].wrap
        case _ => throw new Exception(s"don't know how to wrap $tpe (${c.universe.showRaw(tpe)}) of ${tpe.getClass}")
      }
    }
    implicit class RichOurType(val tpe: OurType) { def unwrap: CompilerType = tpe.tpe0 }

    implicit class RichCompilerAnnotatedType(tpe: c.universe.AnnotatedType) { def wrap: AnnotatedType = new AnnotatedType(tpe) }
    implicit class RichOurAnnotatedType(tpe: AnnotatedType) { def unwrap: c.universe.AnnotatedType = tpe.tpe }
    implicit class RichCompilerExistentialType(tpe: c.universe.ExistentialType) { def wrap: ExistentialType = new ExistentialType(tpe) }
    implicit class RichOurExistentialType(tpe: ExistentialType) { def unwrap: c.universe.ExistentialType = tpe.tpe }
    implicit class RichCompilerTypeBounds(tpe: c.universe.TypeBounds) { def wrap: TypeBounds = new TypeBounds(tpe) }
    implicit class RichOurTypeBounds(tpe: TypeBounds) { def unwrap: c.universe.TypeBounds = tpe.tpe }
    implicit class RichCompilerPolyType(tpe: c.universe.PolyType) { def wrap: PolyType = new PolyType(tpe) }
    implicit class RichOurPolyType(tpe: PolyType) { def unwrap: c.universe.PolyType = tpe.tpe }
    implicit class RichCompilerNullaryMethodType(tpe: c.universe.NullaryMethodType) { def wrap: NullaryMethodType = new NullaryMethodType(tpe) }
    implicit class RichOurNullaryMethodType(tpe: NullaryMethodType) { def unwrap: c.universe.NullaryMethodType = tpe.tpe }
    implicit class RichCompilerMethodType(tpe: c.universe.MethodType) { def wrap: MethodType = new MethodType(tpe) }
    implicit class RichOurMethodType(tpe: MethodType) { def unwrap: c.universe.MethodType = tpe.tpe }
    implicit class RichCompilerRefinedType(tpe: c.universe.RefinedType) { def wrap: RefinedType = new RefinedType(tpe) }
    implicit class RichOurRefinedType(tpe: RefinedType) { def unwrap: c.universe.RefinedType = tpe.tpe }
    implicit class RichCompilerConstantType(tpe: c.universe.ConstantType) { def wrap: ConstantType = new ConstantType(tpe) }
    implicit class RichOurConstantType(tpe: ConstantType) { def unwrap: c.universe.ConstantType = tpe.tpe }
    implicit class RichCompilerBoundedWildcardType(tpe: c.universe.BoundedWildcardType) { def wrap: BoundedWildcardType = new BoundedWildcardType(tpe) }
    implicit class RichOurBoundedWildcardType(tpe: BoundedWildcardType) { def unwrap: c.universe.BoundedWildcardType = tpe.tpe }
    implicit class RichCompilerClassInfoType(tpe: c.universe.ClassInfoType) { def wrap: ClassInfoType = new ClassInfoType(tpe) }
    implicit class RichOurClassInfoType(tpe: ClassInfoType) { def unwrap: c.universe.ClassInfoType = tpe.tpe }
    implicit class RichCompilerSingleType(tpe: c.universe.SingleType) { def wrap: SingleType = new SingleType(tpe) }
    implicit class RichOurSingleType(tpe: SingleType) { def unwrap: c.universe.SingleType = tpe.tpe }
    implicit class RichCompilerSuperType(tpe: c.universe.SuperType) { def wrap: SuperType = new SuperType(tpe) }
    implicit class RichOurSuperType(tpe: SuperType) { def unwrap: c.universe.SuperType = tpe.tpe }
    implicit class RichCompilerThisType(tpe: c.universe.ThisType) { def wrap: ThisType = new ThisType(tpe) }
    implicit class RichOurThisType(tpe: ThisType) { def unwrap: c.universe.ThisType = tpe.tpe }
    implicit class RichCompilerTypeRef(tpe: c.universe.TypeRef) { def wrap: TypeRef = new TypeRef(tpe) }
    implicit class RichOurTypeRef(tpe: TypeRef) { def unwrap: c.universe.TypeRef = tpe.tpe }

    abstract class Type(val tpe0: CompilerType) extends TypeApi {
      def =:=(that: Type): Boolean = tpe0 =:= that.unwrap
      def <:<(that: Type): Boolean = tpe0 <:< that.unwrap
      def asSeenFrom(pre: Type,clazz: Symbol): Type = tpe0.asSeenFrom(pre.unwrap, clazz.unwrap).wrap
      def baseClasses: List[Symbol] = tpe0.baseClasses.map(_.wrap)
      def baseType(clazz: Symbol): Type = tpe0.baseType(clazz.unwrap).wrap
      def companion: Type = tpe0.companion.wrap
      def contains(sym: Symbol): Boolean = tpe0.contains(sym.unwrap)
      def dealias: Type = tpe0.dealias.wrap
      def decl(name: Name): Symbol = tpe0.decl(name.unwrap).wrap
      def declaration(name: Name): Symbol = tpe0.declaration(name.unwrap).wrap
      def declarations: MemberScope = tpe0.declarations.wrap
      def decls: MemberScope = tpe0.decls.wrap
      def erasure: Type = tpe0.erasure.wrap
      def etaExpand: Type = tpe0.etaExpand.wrap
      def exists(p: Type => Boolean): Boolean = tpe0.exists(t => p(t.wrap))
      def finalResultType: Type = tpe0.finalResultType.wrap
      def find(p: Type => Boolean): Option[Type] = tpe0.find(t => p(t.wrap)).map(_.wrap)
      def foreach(f: Type => Unit): Unit = tpe0.foreach(t => f(t.wrap))
      def map(f: Type => Type): Type = tpe0.map(t => f(t.wrap).unwrap).wrap
      def member(name: Name): Symbol = tpe0.member(name.unwrap).wrap
      def members: MemberScope = tpe0.members.wrap
      def normalize: Type = tpe0.normalize.wrap
      def orElse(alt: => Type): Type = tpe0.orElse(alt.unwrap).wrap
      def paramLists: List[List[Symbol]] = tpe0.paramLists.map(_.map(_.wrap))
      def paramss: List[List[Symbol]] = tpe0.paramss.map(_.map(_.wrap))
      def resultType: Type = tpe0.resultType.wrap
      def substituteSymbols(from: List[Symbol],to: List[Symbol]): Type = tpe0.substituteSymbols(from.map(_.unwrap), to.map(_.unwrap)).wrap
      def substituteTypes(from: List[Symbol],to: List[Type]): Type = tpe0.substituteTypes(from.map(_.unwrap), to.map(_.unwrap)).wrap
      def takesTypeArgs: Boolean = tpe0.takesTypeArgs
      def termSymbol: Symbol = tpe0.termSymbol.wrap
      def typeArgs: List[Type] = tpe0.typeArgs.map(_.wrap)
      def typeConstructor: Type = tpe0.typeConstructor.wrap
      def typeParams: List[Symbol] = tpe0.typeParams.map(_.wrap)
      def typeSymbol: Symbol = tpe0.typeSymbol.wrap
      def weak_<:<(that: Type): Boolean = tpe0 weak_<:< that.unwrap
      def widen: Type = tpe0.widen.wrap
      override def toString = tpe0.toString
    }

    class AnnotatedType(val tpe: c.universe.AnnotatedType) extends Type(tpe) with AnnotatedTypeApi {
      def annotations: List[Annotation] = tpe.annotations.map(_.wrap)
      def underlying: Type = tpe.underlying.wrap
    }
    object AnnotatedType extends AnnotatedTypeExtractor {
      def unapply(tpe: AnnotatedType): Option[(List[Annotation], Type)] = c.universe.AnnotatedType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class BoundedWildcardType(val tpe: c.universe.BoundedWildcardType) extends Type(tpe) with BoundedWildcardTypeApi {
      def bounds: TypeBounds = tpe.bounds.wrap
    }
    object BoundedWildcardType extends BoundedWildcardTypeExtractor {
      def unapply(tpe: BoundedWildcardType): Option[TypeBounds] = c.universe.BoundedWildcardType.unapply(tpe.unwrap).map(_.wrap)
    }

    class ClassInfoType(override val tpe: c.universe.ClassInfoType) extends CompoundType(tpe) with ClassInfoTypeApi {
      def parents: List[Type] = tpe.parents.map(_.wrap)
    }
    object ClassInfoType extends ClassInfoTypeExtractor {
      def unapply(tpe: ClassInfoType): Option[(List[Type], Scope, Symbol)] = c.universe.ClassInfoType.unapply(tpe.unwrap).map { case (x1, x2, x3) => (x1.map(_.wrap), x2.wrap, x3.wrap) }
    }

    class CompoundType(val tpe: c.universe.CompoundType) extends Type(tpe) with CompoundTypeApi {
    }

    class ConstantType(override val tpe: c.universe.ConstantType) extends SingletonType(tpe) with ConstantTypeApi {
      def value: Constant = tpe.value.wrap
    }
    object ConstantType extends ConstantTypeExtractor {
      def unapply(tpe: ConstantType): Option[Constant] = c.universe.ConstantType.unapply(tpe.unwrap).map(_.wrap)
    }

    class ExistentialType(val tpe: c.universe.ExistentialType) extends Type(tpe) with ExistentialTypeApi {
      def quantified: List[Symbol] = tpe.quantified.map(_.wrap)
      def underlying: Type = tpe.underlying.wrap
    }
    object ExistentialType extends ExistentialTypeExtractor {
      def unapply(tpe: ExistentialType): Option[(List[Symbol], Type)] = c.universe.ExistentialType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class MethodType(val tpe: c.universe.MethodType) extends Type(tpe) with MethodTypeApi {
      def params: List[Symbol] = tpe.params.map(_.wrap)
    }
    object MethodType extends MethodTypeExtractor {
      def unapply(tpe: MethodType): Option[(List[Symbol], Type)] = c.universe.MethodType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class NullaryMethodType(val tpe: c.universe.NullaryMethodType) extends Type(tpe) with NullaryMethodTypeApi {
    }
    object NullaryMethodType extends NullaryMethodTypeExtractor {
      def unapply(tpe: NullaryMethodType): Option[(Type)] = c.universe.NullaryMethodType.unapply(tpe.unwrap).map(_.wrap)
    }

    class PolyType(val tpe: c.universe.PolyType) extends Type(tpe) with PolyTypeApi {
    }
    object PolyType extends PolyTypeExtractor {
      def unapply(tpe: PolyType): Option[(List[Symbol], Type)] = c.universe.PolyType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class RefinedType(override val tpe: c.universe.RefinedType) extends CompoundType(tpe) with RefinedTypeApi {
      def parents: List[Type] = tpe.parents.map(_.wrap)
    }
    object RefinedType extends RefinedTypeExtractor {
      def unapply(tpe: RefinedType): Option[(List[Type], Scope)] = c.universe.RefinedType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.map(_.wrap), x2.wrap) }
    }

    class SingletonType(val tpe: c.universe.SingletonType) extends Type(tpe) with SingletonTypeApi {
    }

    class SingleType(override val tpe: c.universe.SingleType) extends SingletonType(tpe) with SingleTypeApi {
      def pre: Type = tpe.pre.wrap
      def sym: Symbol = tpe.sym.wrap
    }
    object SingleType extends SingleTypeExtractor {
      def unapply(tpe: SingleType): Option[(Type, Symbol)] = c.universe.SingleType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class SuperType(override val tpe: c.universe.SuperType) extends SingletonType(tpe) with SuperTypeApi {
      def thistpe: Type = tpe.thistpe.wrap
      def supertpe: Type = tpe.supertpe.wrap
    }
    object SuperType extends SuperTypeExtractor {
      def unapply(tpe: SuperType): Option[(Type, Type)] = c.universe.SuperType.unapply(tpe.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class ThisType(override val tpe: c.universe.ThisType) extends SingletonType(tpe) with ThisTypeApi {
      def sym: Symbol = tpe.sym.wrap
    }
    object ThisType extends ThisTypeExtractor {
      def unapply(tpe: ThisType): Option[Symbol] = c.universe.ThisType.unapply(tpe.unwrap).map(_.wrap)
    }

    class TypeBounds(val tpe: c.universe.TypeBounds) extends Type(tpe) with TypeBoundsApi {
      def lo: Type = tpe.lo.wrap
      def hi: Type = tpe.hi.wrap
    }
    object TypeBounds extends TypeBoundsExtractor {
      def unapply(tpe: TypeBounds): Option[(Type, Type)] = c.universe.TypeBounds.unapply(tpe.unwrap).map { case (x1, x2) => (x1.wrap, x2.wrap) }
    }

    class TypeRef(val tpe: c.universe.TypeRef) extends Type(tpe) with TypeRefApi {
      def pre: Type = tpe.pre.wrap
      def sym: Symbol = tpe.sym.wrap
      def args: List[Type] = tpe.args.map(_.wrap)
    }
    object TypeRef extends TypeRefExtractor {
      def unapply(tpe: TypeRef): Option[(Type, Symbol, List[Type])] = c.universe.TypeRef.unapply(tpe.unwrap).map { case (x1, x2, x3) => (x1.wrap, x2.wrap, x3.map(_.wrap)) }
    }

    case object NoPrefix extends Type(c.universe.NoPrefix)
    case object NoType extends Type(c.universe.NoType)
    case object WildcardType extends Type(c.universe.WildcardType)

    def appliedType(sym: OurSymbol, args: OurType*): Type = c.universe.appliedType(sym.unwrap, args.map(_.unwrap): _*).wrap
    def appliedType(sym: OurSymbol, args: List[OurType]): Type = appliedType(sym, args: _*)
    def appliedType(tycon: OurType, args: OurType*): OurType = c.universe.appliedType(tycon.unwrap, args.map(_.unwrap): _*).wrap
    def appliedType(tycon: OurType, args: List[OurType]): Type = appliedType(tycon, args: _*)
    def glb(ts: List[OurType]): Type = c.universe.glb(ts.map(_.unwrap)).wrap
    def lub(xs: List[OurType]): Type = c.universe.lub(xs.map(_.unwrap)).wrap
  }
}