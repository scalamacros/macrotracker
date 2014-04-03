package scala.tools.nsc.palladium
package contexts

trait Symbols {
  self: Context =>

  trait UniverseSymbols {
    self: universe.type =>

    type CompilerSymbol = c.universe.Symbol
    type OurSymbol = universe.Symbol
    implicit class RichCompilerSymbol(sym: CompilerSymbol) {
      def wrap: OurSymbol = sym match {
        case null => null
        case c.universe.NoSymbol => NoSymbol
        case sym if c.universe.FreeTypeSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.FreeTypeSymbol].wrap
        case sym if c.universe.FreeTermSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.FreeTermSymbol].wrap
        case sym if c.universe.ClassSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.ClassSymbol].wrap
        case sym if c.universe.ModuleSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.ModuleSymbol].wrap
        case sym if c.universe.MethodSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.MethodSymbol].wrap
        case sym if c.universe.TypeSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.TypeSymbol].wrap
        case sym if c.universe.TermSymbolTag.runtimeClass.isInstance(sym) => sym.asInstanceOf[c.universe.TermSymbol].wrap
        case _ => throw new Exception(s"don't know how to wrap $sym (${c.universe.showDecl(sym)}) of class ${sym.getClass}")
      }
    }
    implicit class RichOurSymbol(sym: OurSymbol) { def unwrap: CompilerSymbol = sym.sym0 }

    type CompilerTermSymbol = c.universe.TermSymbol
    type OurTermSymbol = universe.TermSymbol
    implicit class RichCompilerTermSymbol(sym: CompilerTermSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurTermSymbol(sym)
      }
    }
    implicit class RichOurTermSymbol(sym: OurTermSymbol) { def wrap = sym.sym }

    type CompilerTypeSymbol = c.universe.TypeSymbol
    type OurTypeSymbol = universe.TypeSymbol
    implicit class RichCompilerTypeSymbol(sym: CompilerTypeSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurTypeSymbol(sym)
      }
    }
    implicit class RichOurTypeSymbol(sym: OurTypeSymbol) { def wrap = sym.sym }

    type CompilerMethodSymbol = c.universe.MethodSymbol
    type OurMethodSymbol = universe.MethodSymbol
    implicit class RichCompilerMethodSymbol(sym: CompilerMethodSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurMethodSymbol(sym)
      }
    }
    implicit class RichOurMethodSymbol(sym: OurMethodSymbol) { def wrap = sym.sym }

    type CompilerModuleSymbol = c.universe.ModuleSymbol
    type OurModuleSymbol = universe.ModuleSymbol
    implicit class RichCompilerModuleSymbol(sym: CompilerModuleSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurModuleSymbol(sym)
      }
    }
    implicit class RichOurModuleSymbol(sym: OurModuleSymbol) { def wrap = sym.sym }

    type CompilerClassSymbol = c.universe.ClassSymbol
    type OurClassSymbol = universe.ClassSymbol
    implicit class RichCompilerClassSymbol(sym: CompilerClassSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurClassSymbol(sym)
      }
    }
    implicit class RichOurClassSymbol(sym: OurClassSymbol) { def wrap = sym.sym }

    type CompilerFreeTermSymbol = c.universe.FreeTermSymbol
    type OurFreeTermSymbol = universe.FreeTermSymbol
    implicit class RichCompilerFreeTermSymbol(sym: CompilerFreeTermSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurFreeTermSymbol(sym)
      }
    }
    implicit class RichOurFreeTermSymbol(sym: OurFreeTermSymbol) { def wrap = sym.sym }

    type CompilerFreeTypeSymbol = c.universe.FreeTypeSymbol
    type OurFreeTypeSymbol = universe.FreeTypeSymbol
    implicit class RichCompilerFreeTypeSymbol(sym: CompilerFreeTypeSymbol) {
      def wrap = {
        touchedSymbols += sym
        new OurFreeTypeSymbol(sym)
      }
    }
    implicit class RichOurFreeTypeSymbol(sym: OurFreeTypeSymbol) { def wrap = sym.sym }

    abstract class Symbol(val sym0: CompilerSymbol) extends SymbolApi {
      def allOverriddenSymbols: List[Symbol] = sym0.allOverriddenSymbols.map(_.wrap)
      def alternatives: List[Symbol] = sym0.alternatives.map(_.wrap)
      def annotations: List[Annotation] = sym0.annotations.map(_.wrap)
      def associatedFile: scala.reflect.io.AbstractFile = sym0.associatedFile
      def companion: Symbol = sym0.companion.wrap
      def companionSymbol: Symbol = sym0.companionSymbol.wrap
      def filter(cond: Symbol => Boolean): Symbol = sym0.filter(sym => cond(sym.wrap)).wrap
      def fullName: String = sym0.fullName
      def info: Type = sym0.info.wrap
      def infoIn(site: Type): Type = sym0.infoIn(site.unwrap).wrap
      def isAbstract: Boolean = sym0.isAbstract
      def isAbstractOverride: Boolean = sym0.isAbstractOverride
      def isConstructor: Boolean = sym0.isConstructor
      def isFinal: Boolean = sym0.isFinal
      def isImplementationArtifact: Boolean = sym0.isImplementationArtifact
      def isImplicit: Boolean = sym0.isImplicit
      def isJava: Boolean = sym0.isJava
      def isMacro: Boolean = sym0.isMacro
      def isPackage: Boolean = sym0.isPackage
      def isPackageClass: Boolean = sym0.isPackageClass
      def isParameter: Boolean = sym0.isParameter
      def isPrivate: Boolean = sym0.isPrivate
      def isPrivateThis: Boolean = sym0.isPrivateThis
      def isProtected: Boolean = sym0.isProtected
      def isProtectedThis: Boolean = sym0.isProtectedThis
      def isPublic: Boolean = sym0.isPublic
      def isSpecialized: Boolean = sym0.isSpecialized
      def isStatic: Boolean = sym0.isStatic
      def isSynthetic: Boolean = sym0.isSynthetic
      def map(f: Symbol => Symbol): Symbol = sym0.map(sym => f(sym.wrap).unwrap).wrap
      def name: NameType = sym0.name.wrap.asInstanceOf[NameType]
      def orElse(alt: => Symbol): Symbol = sym0.orElse(alt.unwrap).wrap
      def overrides: List[Symbol] = sym0.overrides.map(_.wrap)
      def owner: Symbol = sym0.owner.wrap
      def pos: Position = sym0.pos.wrap
      def privateWithin: Symbol = sym0.privateWithin.wrap
      def suchThat(cond: Symbol => Boolean): Symbol = sym0.suchThat(sym => cond(sym.wrap)).wrap
      def typeSignature: Type = sym0.typeSignature.wrap
      def typeSignatureIn(site: Type): Type = sym0.typeSignatureIn(site.unwrap).wrap
      override def toString = sym0.toString
    }

    class TermSymbol(val sym: CompilerTermSymbol) extends Symbol(sym) with TermSymbolApi {
      override def name: TermName = sym.name.wrap
      def accessed: Symbol = sym.accessed.wrap
      def getter: Symbol = sym.getter.wrap
      def isAccessor: Boolean = sym.isAccessor
      def isByNameParam: Boolean = sym.isByNameParam
      def isCaseAccessor: Boolean = sym.isCaseAccessor
      def isGetter: Boolean = sym.isGetter
      def isLazy: Boolean = sym.isLazy
      def isOverloaded: Boolean = sym.isOverloaded
      def isParamAccessor: Boolean = sym.isParamAccessor
      def isParamWithDefault: Boolean = sym.isParamWithDefault
      def isSetter: Boolean = sym.isSetter
      def isStable: Boolean = sym.isStable
      def isVal: Boolean = sym.isVal
      def isVar: Boolean = sym.isVar
      def setter: Symbol = sym.setter.wrap
    }

    class TypeSymbol(val sym: CompilerTypeSymbol) extends Symbol(sym) with TypeSymbolApi {
      override def name: TypeName = sym.name.wrap
      def isAbstractType: Boolean = sym.isAbstractType
      def isAliasType: Boolean = sym.isAliasType
      def isContravariant: Boolean = sym.isContravariant
      def isCovariant: Boolean = sym.isCovariant
      def isExistential: Boolean = sym.isExistential
      def toType: Type = sym.toType.wrap
      def toTypeConstructor: Type = sym.toTypeConstructor.wrap
      def toTypeIn(site: Type): Type = sym.toTypeIn(site.unwrap).wrap
      def typeParams: List[Symbol] = sym.typeParams.map(_.wrap)
    }

    class MethodSymbol(override val sym: CompilerMethodSymbol) extends TermSymbol(sym) with MethodSymbolApi {
      def exceptions: List[Symbol] = sym.exceptions.map(_.wrap)
      def isPrimaryConstructor: Boolean = sym.isPrimaryConstructor
      def isVarargs: Boolean = sym.isVarargs
      def paramLists: List[List[Symbol]] = sym.paramLists.map(_.map(_.wrap))
      def paramss: List[List[Symbol]] = sym.paramss.map(_.map(_.wrap))
      def returnType: Type = sym.returnType.wrap
      def typeParams: List[Symbol] = sym.typeParams.map(_.wrap)
    }

    class ModuleSymbol(override val sym: CompilerModuleSymbol) extends TermSymbol(sym) with ModuleSymbolApi {
      def moduleClass: Symbol = sym.moduleClass.wrap
    }

    class ClassSymbol(override val sym: CompilerClassSymbol) extends TypeSymbol(sym) with ClassSymbolApi {
      def baseClasses: List[Symbol] = sym.baseClasses.map(_.wrap)
      def isAbstractClass: Boolean = sym.isAbstractClass
      def isCaseClass: Boolean = sym.isCaseClass
      def isDerivedValueClass: Boolean = sym.isDerivedValueClass
      def isNumeric: Boolean = sym.isNumeric
      def isPrimitive: Boolean = sym.isPrimitive
      def isSealed: Boolean = sym.isSealed
      def isTrait: Boolean = sym.isTrait
      def knownDirectSubclasses: Set[Symbol] = sym.knownDirectSubclasses.map(_.wrap)
      def module: Symbol = sym.module.wrap
      def primaryConstructor: Symbol = sym.primaryConstructor.wrap
      def selfType: Type = sym.selfType.wrap
      def superPrefix(supertpe: Type): Type = sym.superPrefix(supertpe.unwrap).wrap
      def thisPrefix: Type = sym.thisPrefix.wrap
    }

    class FreeTermSymbol(override val sym: CompilerFreeTermSymbol) extends TermSymbol(sym) with FreeTermSymbolApi {
      def origin: String = sym.origin
      def value: Any = sym.value
    }

    class FreeTypeSymbol(override val sym: CompilerFreeTypeSymbol) extends TypeSymbol(sym) with FreeTypeSymbolApi {
      def origin: String = sym.origin
    }

    case object NoSymbol extends Symbol(c.universe.NoSymbol)
  }
}