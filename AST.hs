module AST where

data Source = Source { srcFilename :: String }

data ModuleAPI
    = ModuleAPI
    { moduleName :: String
    , moduleTypeDecls :: [TypeDecl]
    , moduleTypeDefs :: [TypeDefinition]
    , moduleContracts :: [ContractDecl]
    , moduleValues :: [ValueDecl]
    }

data NamedType
    = NamedType { typeId :: String
                , typeParams :: [NamedType]
                }
    | NamedTypeVar TypeVar

data Field a
    = Field
    { fieldName :: String
    , fieldType :: NamedType
    }

data FValue
data FType
data FConstructor

type ValueDecl = Field FValue

data ContractDecl
    = ContractDecl
    { cdContract :: Contract
    , cdName :: String
    }

data QualTypeVar
    = QualTypeVar
    { qtvVar :: TypeVar
    , qtvContracts :: [Contract]
    }

data Contract
    = Contract
    { contractParams :: [QualTypeVar]
    , contractMembers :: [ValueDecl]
    }

data TypeVar = TypeVar { tvName :: String }

data TypeDecl
    = TypeDecl
    { typeDeclName :: String
    , typeDeclParams :: [TypeVar]
    }

data TypeDef
    = TypeDef
    { typeDefName :: String
    , typeDefinition :: TypeDefinition
    }

data TypeDefinition
    = TDUnion Union
    | TDStruct Struct
--    | Func FuncDecl

data Union
    = Union
    { unionParams :: [TypeVar]
    , unionConstructors :: [Constructor]
    }

data Constructor
    = Constructor
    { conName :: String
    , conFields :: [Field FConstructor]
    }

data Struct
    = Struct
    { structParams :: [TypeVar]
    , structFields :: [ValueDecl]
    }
