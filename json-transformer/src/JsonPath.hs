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

data JsonPath = NilPath
    | RootPath JsonPath
    | ObjectPath Text JsonPath
    | ArrayPath Int JsonPath
    deriving Eq

instance Show JsonPath where
    show NilPath = ""
    show (RootPath next) = "$" ++ show next
    show (ObjectPath member next) = "." ++ T.unpack member ++ show next
    show (ArrayPath index next) = "[" ++ show index ++ "]" ++ show next

rootPath :: JsonPath
rootPath = RootPath NilPath

enterObject :: JsonPath -> Text -> JsonPath
enterObject NilPath member = ObjectPath member NilPath
enterObject (RootPath next) member = RootPath $ enterObject next member
enterObject (ObjectPath parent next) member = ObjectPath parent $ enterObject next member
enterObject (ArrayPath index next) member = ArrayPath index $ enterObject next member

enterArray :: JsonPath -> Int -> JsonPath
enterArray NilPath index = ArrayPath index NilPath
enterArray (RootPath next) index = RootPath $ enterArray next index
enterArray (ObjectPath parent next) index = ObjectPath parent $ enterArray next index
enterArray (ArrayPath index next) index' = ArrayPath index $ enterArray next index'

exitPath :: JsonPath -> JsonPath
exitPath NilPath = error "Cannot exit from NilPath!"
exitPath (RootPath NilPath) = NilPath
exitPath (RootPath next) = RootPath $ exitPath next
exitPath (ObjectPath _ NilPath) = NilPath
exitPath (ObjectPath member next) = ObjectPath member $ exitPath next
exitPath (ArrayPath _ NilPath) = NilPath
exitPath (ArrayPath index next) = ArrayPath index $ exitPath next

getLeaf :: JsonPath -> JsonPath
getLeaf NilPath = NilPath
getLeaf p@(RootPath NilPath) = p
getLeaf (RootPath next) = getLeaf next
getLeaf p@(ObjectPath _ NilPath) = p
getLeaf (ObjectPath member next) = getLeaf next
getLeaf p@(ArrayPath _ NilPath) = p
getLeaf (ArrayPath index next) = getLeaf next
