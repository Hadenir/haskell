-- | Module containing data structure for representing JSON paths and helper functions.
module JsonPath
    ( JsonPath (..)
    , rootPath
    , enterObject
    , enterArray
    , exitPath
    , getLeaf
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Data type representing JSON version of XPath, e.g. `$.array[0].member`.
data JsonPath = NilPath
    | RootPath JsonPath
    | ObjectPath Text JsonPath
    | ArrayPath Int JsonPath
    deriving Eq

-- | Implementation of `Show` for `JsonPath` that converts path back to string.
instance Show JsonPath where
    show NilPath = ""
    show (RootPath next) = "$" ++ show next
    show (ObjectPath member next) = "." ++ T.unpack member ++ show next
    show (ArrayPath index next) = "[" ++ show index ++ "]" ++ show next

-- | Comparison operator that allows to check whether one path is a subpath of another one.
--   `path1 <= path2` is True only if `path1 == path2` or when `path2` is subpath of `path1`.
instance Ord JsonPath where
    NilPath <= _ = True
    (RootPath path1) <= (RootPath path2) = path1 <= path2
    (ObjectPath member1 path1) <= (ObjectPath member2 path2) = member1 == member2 && path1 <= path2
    (ArrayPath (-1) path1) <= (ArrayPath _ path2) = path1 <= path2
    (ArrayPath index1 path1) <= (ArrayPath index2 path2) = index1 == index2 && path1 <= path2
    path1 <= path2 = path1 == path2

-- | Return path consisting of only root path, i.e. `$`.
rootPath :: JsonPath
rootPath = RootPath NilPath

-- | Helper function that given path to an JSON object and a name
--   returns path to member of that object with provided name.
--   For example, given `$.object` and `member`, returns `$.object.member`.
enterObject :: JsonPath -> Text -> JsonPath
enterObject NilPath member = ObjectPath member NilPath
enterObject (RootPath next) member = RootPath $ enterObject next member
enterObject (ObjectPath parent next) member = ObjectPath parent $ enterObject next member
enterObject (ArrayPath index next) member = ArrayPath index $ enterObject next member

-- | Helper function that given path to an JSON object and an index
--   returns path to elemenf of that array at provided index.
--   For example, given `$.array` and `0`, returns `$.array[0]`.
enterArray :: JsonPath -> Int -> JsonPath
enterArray NilPath index = ArrayPath index NilPath
enterArray (RootPath next) index = RootPath $ enterArray next index
enterArray (ObjectPath parent next) index = ObjectPath parent $ enterArray next index
enterArray (ArrayPath index next) index' = ArrayPath index $ enterArray next index'

-- | Returns parent path.
--   Passing NilPath results in an error because it has no parent.
exitPath :: JsonPath -> JsonPath
exitPath NilPath = error "Cannot exit from NilPath!"
exitPath (RootPath NilPath) = NilPath
exitPath (RootPath next) = RootPath $ exitPath next
exitPath (ObjectPath _ NilPath) = NilPath
exitPath (ObjectPath member next) = ObjectPath member $ exitPath next
exitPath (ArrayPath _ NilPath) = NilPath
exitPath (ArrayPath index next) = ArrayPath index $ exitPath next

-- | Returns the deepest child of the path, e.g. for `$.a.b[0].c` returns `c`.
getLeaf :: JsonPath -> JsonPath
getLeaf NilPath = NilPath
getLeaf p@(RootPath NilPath) = p
getLeaf (RootPath next) = getLeaf next
getLeaf p@(ObjectPath _ NilPath) = p
getLeaf (ObjectPath member next) = getLeaf next
getLeaf p@(ArrayPath _ NilPath) = p
getLeaf (ArrayPath index next) = getLeaf next
