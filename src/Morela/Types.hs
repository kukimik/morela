module Morela.Types
  ( AttributeName,
    ConstraintName,
    StyleName,
    TableName,
    TypeName,
    Comment,
    SQLCondition,
    Attribute (..),
    PKConstraint (..),
    NNConstraint (..),
    FKConstraint (..),
    CKConstraint (..),
    UQConstraint (..),
    Index (..),
    Table (..),
    Style (..),
    Diagram (..),
    emptyDiagram,
    emptyTable,
  )
where

import Control.Applicative (empty)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text.Lazy hiding (empty)

type AttributeName = Text

type ConstraintName = Text

type StyleName = Text

type TableName = Text

type TypeName = Text

type Comment = Text

type SQLCondition = Text

data Attribute = Attribute
  { attributeName :: AttributeName,
    attributeType :: Maybe TypeName,
    attributeComment :: Maybe Comment,
    attributeStyleName :: Maybe StyleName
  }
  deriving (Eq, Show)

instance Ord Attribute where
  a1 `compare` a2 = attributeName a1 `compare` attributeName a2

newtype PKConstraint = PKConstraint
  { pkAttributeNames ::
      [AttributeName]
  }
  deriving (Eq, Show)

newtype NNConstraint = NNConstraint
  { nnAttributeName ::
      AttributeName
  }
  deriving (Eq, Show)

data FKConstraint = FKConstraint
  { fkReferencedTableName :: TableName,
    fkAttributeMapping :: [(AttributeName, AttributeName)], -- switch to NonEmpty
    fkStyleName :: Maybe StyleName,
    fkComment :: Maybe Comment
  }
  deriving (Eq, Show)

data CKConstraint = CKConstraint
  { ckSQLCondition :: SQLCondition,
    ckStyleName :: Maybe StyleName,
    ckComment :: Maybe Comment
  }
  deriving (Eq, Show)

data UQConstraint = UQConstraint
  { uqAttributeNames :: [AttributeName], -- switch to NonEmpty
    uqStyleName :: Maybe StyleName,
    uqComment :: Maybe Comment
  }
  deriving (Eq, Show)

data Index = Index
  { ixAttributeNames :: [AttributeName],
    ixIsUnique :: Bool, -- TODO: maybe encode this in type?
    ixStyleName :: Maybe StyleName,
    ixComment :: Maybe Comment
  }
  deriving (Eq, Show)

data Table = Table
  { tableName :: TableName,
    tableAttributes :: [Attribute], -- TODO: change this and other lists to sequences (or non-empty sequences)
    tableComment :: Maybe Comment,
    tablePK :: PKConstraint,
    tableNNs :: [NNConstraint],
    tableCKs :: [CKConstraint],
    tableUQs :: [UQConstraint],
    tableFKs :: [FKConstraint],
    tableIndexes :: [Index]
  }
  deriving (Eq, Show)

instance Ord Table where
  t1 `compare` t2 = tableName t1 `compare` tableName t2

data Style = Style -- TODO!
--{ --???
-- someName :: Maybe (GraphvizAttribute SomeType)
--}
  deriving (Eq, Show)

data Diagram = Diagram
  { diagramTables :: S.Set Table,
    diagramStyles :: M.Map StyleName Style
  }
  deriving (Eq, Show)

emptyDiagram :: Diagram
emptyDiagram = Diagram {diagramTables = S.empty, diagramStyles = M.empty}

emptyTable :: TableName -> Table
emptyTable tn =
  Table
    { tableName = tn,
      tableAttributes = empty,
      tableComment = empty,
      tablePK = PKConstraint {pkAttributeNames = empty},
      tableNNs = empty,
      tableCKs = empty,
      tableUQs = empty,
      tableFKs = empty,
      tableIndexes = empty
    }
