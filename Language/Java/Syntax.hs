{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, StandaloneDeriving #-}
module Language.Java.Syntax where

import Data.Data
import Text.Parsec.Pos

#define DERIVE deriving (Eq,Ord,Show,Typeable,Data)

-----------------------------------------------------------------------
-- Packages


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit a = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl a]
  DERIVE


-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl = PackageDecl Name
  DERIVE

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
    = ImportDecl Bool {- static? -} Name Bool {- .*? -}
  DERIVE


-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl a
    = ClassTypeDecl (ClassDecl a)
    | InterfaceTypeDecl (InterfaceDecl a)
  DERIVE

-- | A class declaration specifies a new named reference type.
data ClassDecl a
    = ClassDecl [Modifier a] Ident [TypeParam] (Maybe RefType) [RefType] (ClassBody a)
    | EnumDecl  [Modifier a] Ident                             [RefType] (EnumBody a)
  DERIVE

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody a = ClassBody [Decl a]
  DERIVE

-- | The body of an enum type may contain enum constants.
data EnumBody a = EnumBody [EnumConstant a] [Decl a]
  DERIVE

-- | An enum constant defines an instance of the enum type.
data EnumConstant a = EnumConstant Ident [a] (Maybe (ClassBody a))
  DERIVE

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl a
    = InterfaceDecl [Modifier a] Ident [TypeParam] [RefType] (InterfaceBody a)
  DERIVE

-- | The body of an interface may declare members of the interface.
data InterfaceBody a
    = InterfaceBody [MemberDecl a]
  DERIVE

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl a
    = MemberDecl (MemberDecl a)
    | InitDecl Bool (Block a)
  DERIVE


-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl a
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl [Modifier a] Type [VarDecl a]
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl      [Modifier a] [TypeParam] (Maybe Type) Ident [FormalParam a] [ExceptionType] (MethodBody a)
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl [Modifier a] [TypeParam]              Ident [FormalParam a] [ExceptionType] (ConstructorBody a)
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl (ClassDecl a)
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl (InterfaceDecl a)
  DERIVE


-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl a
    = VarDecl VarDeclId (Maybe (VarInit a))
  DERIVE

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
    = VarId Ident
    | VarDeclArray VarDeclId
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  DERIVE

-- | Explicit initializer for a variable declaration.
data VarInit a
    = InitExp (a)
    | InitArray (ArrayInit a)
  DERIVE

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam a = FormalParam [Modifier a] Type Bool VarDeclId
  DERIVE

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody a = MethodBody (Maybe (Block a))
  DERIVE

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody a = ConstructorBody (Maybe (ExplConstrInv a)) [BlockStmt a]
  DERIVE

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv a
    = ThisInvoke                 [RefType] [(a)]
    | SuperInvoke                [RefType] [(a)]
    | PrimarySuperInvoke (a) [RefType] [(a)]
  DERIVE


-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier a
    = Public
    | Private
    | Protected
    | Abstract
    | Final
    | Static
    | StrictFP
    | Transient
    | Volatile
    | Native
    | Annotation (Annotation a)
    | Synchronised
  DERIVE

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation a = NormalAnnotation        { annName :: Name -- Not type because not type generics not allowed
                                          , annKV   :: [(Ident, (ElementValue a))] }
                | SingleElementAnnotation { annName :: Name
                                          , annValue:: ElementValue a }
                | MarkerAnnotation        { annName :: Name }
  DERIVE

desugarAnnotation (MarkerAnnotation n)          = (n, [])
desugarAnnotation (SingleElementAnnotation n e) = (n, [(Ident Nothing "value", e)])
desugarAnnotation (NormalAnnotation n kv)       = (n, kv)
desugarAnnotation' = uncurry NormalAnnotation . desugarAnnotation

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue a = EVVal (VarInit a)
                  | EVAnn (Annotation a)
  DERIVE

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block a = Block [BlockStmt a]
  DERIVE



-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt a
    = BlockStmt (Stmt a)
    | LocalClass (ClassDecl a)
    | LocalVars [Modifier a] Type [VarDecl a]
  DERIVE


-- | A Java statement.
data Stmt a
    -- | A statement can be a nested block.
    = StmtBlock (Block a)
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen (a) (Stmt a)
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse (a) (Stmt a) (Stmt a)
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While (a) (Stmt a)
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor (Maybe (ForInit a)) (Maybe (a)) (Maybe [a]) (Stmt a)
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor [Modifier a] Type Ident (a) (Stmt a)
    -- | An empty statement does nothing.
    | Empty
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt (a)
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert (a) (Maybe (a))
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch (a) [SwitchBlock a]
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do (Stmt a) (a)
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break (Maybe Ident)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue (Maybe Ident)
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return (Maybe (a))
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized (a) (Block a)
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw (a)
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try (Block a) [Catch a] (Maybe {- finally -} (Block a))
    -- | Statements may have label prefixes.
    | Labeled Ident (Stmt a)
  DERIVE

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch a = Catch (FormalParam a) (Block a)
  DERIVE

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock a
    = SwitchBlock (SwitchLabel a) [BlockStmt a]
  DERIVE

-- | A label within a @switch@ statement.
data SwitchLabel a
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase (a)
    | Default
  DERIVE

-- | Initialization code for a basic @for@ statement.
data ForInit a
    = ForLocalVars [Modifier a] Type [VarDecl a]
    | ForInitExps [a]
  DERIVE

-- | An exception type has to be a class type or a type variable.
type ExceptionType = RefType -- restricted to ClassType or TypeVariable


-----------------------------------------------------------------------
-- Expressions


-- | A Java expression.
data Exp a
    -- | A literal denotes a fixed, unchanging value.
    = Lit Literal
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit (Maybe Type)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClass Name
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation [TypeArgument] ClassType [a] (Maybe (ClassBody a))
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation a [TypeArgument] Ident [a] (Maybe (ClassBody a))
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate Type [a] Int
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit Type Int (ArrayInit a)
    -- | A field access expression.
    | FieldAccess (FieldAccess a)
    -- | A method invocation expression.
    | MethodInv (MethodInvocation a)
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess (ArrayIndex a)
{-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
    | ExpName Name
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement a
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement a
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement  a
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement  a
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  a
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus a
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl a
    -- | Logical complementation of boolean values.
    | PreNot  a
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast  Type a
    -- | The application of a binary operator to two operand expressions.
    | BinOp a Op a
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf a RefType
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond a a a
    -- | Assignment of the result of an expression to a variable.
    | Assign (Lhs a) AssignOp a
    | SuperHackCompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl a]
  DERIVE

deriving instance Functor Exp

-- | A literal denotes a fixed, unchanging value.
data Literal
    = Int Integer
    | Word Integer
    | Float Double
    | Double Double
    | Boolean Bool
    | Char Char
    | String String
    | Null
  DERIVE

-- | A binary infix operator.
data Op = Mult | Div | Rem | Add | Sub | LShift | RShift | RRShift
        | LThan | GThan | LThanE | GThanE | Equal | NotEq
        | And | Or | Xor | CAnd | COr
  DERIVE

-- | An assignment operator.
data AssignOp = EqualA | MultA | DivA | RemA | AddA | SubA
              | LShiftA | RShiftA | RRShiftA | AndA | XorA | OrA
  DERIVE

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs a
    = NameLhs Name          -- ^ Assign to a variable
    | FieldLhs (FieldAccess a)  -- ^ Assign through a field access
    | ArrayLhs (ArrayIndex a)   -- ^ Assign to an array
  DERIVE

-- | Array access
data ArrayIndex a= ArrayIndex (a) [a]    -- ^ Index into an array
  DERIVE

deriving instance Functor ArrayIndex
deriving instance Functor ArrayInit
deriving instance Functor VarInit
deriving instance Functor ClassBody
deriving instance Functor FieldAccess
deriving instance Functor Decl
deriving instance Functor Lhs
deriving instance Functor Block
deriving instance Functor MemberDecl
deriving instance Functor BlockStmt
deriving instance Functor MethodInvocation
deriving instance Functor ClassDecl
deriving instance Functor TypeDecl
deriving instance Functor ConstructorBody
deriving instance Functor EnumBody
deriving instance Functor InterfaceDecl
deriving instance Functor Modifier
deriving instance Functor Annotation
deriving instance Functor InterfaceBody
deriving instance Functor EnumConstant
deriving instance Functor ExplConstrInv
deriving instance Functor Stmt
deriving instance Functor FormalParam
deriving instance Functor ElementValue
deriving instance Functor Catch
deriving instance Functor VarDecl
deriving instance Functor MethodBody
deriving instance Functor ForInit
deriving instance Functor SwitchBlock
deriving instance Functor SwitchLabel


-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess a
    = PrimaryFieldAccess (a) Ident      -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess Ident            -- ^ Accessing a field of the superclass.
    | ClassFieldAccess Name Ident       -- ^ Accessing a (static) field of a named class.
  DERIVE


-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation a
    -- | Invoking a specific named method.
    = MethodCall Name [a]
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall (a) [RefType] Ident [a]
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall [RefType] Ident [a]
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall Name [RefType] Ident [a]
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall  Name [RefType] Ident [a]
  DERIVE

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit a
    = ArrayInit [VarInit a]
  DERIVE


-----------------------------------------------------------------------
-- Types


-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
    = PrimType PrimType
    | RefType RefType
  DERIVE

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
    = ClassRefType ClassType
    {- | TypeVariable Ident -}
    | ArrayType Type
  DERIVE

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType
    = ClassType [(Ident, [TypeArgument])]
  DERIVE

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
    = Wildcard (Maybe WildcardBound)
    | ActualType RefType
  DERIVE

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
    = ExtendsBound RefType
    | SuperBound RefType
  DERIVE

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
    = BooleanT
    | ByteT
    | ShortT
    | IntT
    | LongT
    | CharT
    | FloatT
    | DoubleT
  DERIVE


-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam = TypeParam Ident [RefType]
  DERIVE


-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident (Maybe SourcePos) String
  DERIVE

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name [Ident]
  DERIVE
