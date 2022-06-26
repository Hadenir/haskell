-- | Module containing data structure for representing JSON objects.
module Json
    ( Token (..)
    , Path (..)
    , rootPath
    , enterObject
    , enterArray
    , exitPath
    , getLeaf
    ) where

import Data.Text (Text)
import qualified Data.Text as T

data Token = Null
    | Boolean Bool
    | String Text
    | Number Double
    | ArrayBegin
    | ArrayEnd
    | ObjectBegin
    | ObjectField Text
    | ObjectEnd
    deriving Show

data Path = NilPath
    | RootPath Path
    | ObjectPath Text Path
    | ArrayPath Int Path
    deriving Eq

instance Show Path where
    show NilPath = ""
    show (RootPath next) = "$" ++ show next
    show (ObjectPath member next) = "." ++ T.unpack member ++ show next
    show (ArrayPath index next) = "[" ++ show index ++ "]" ++ show next

rootPath :: Path
rootPath = RootPath NilPath

enterObject :: Path -> Text -> Path
enterObject NilPath member = ObjectPath member NilPath
enterObject (RootPath next) member = RootPath $ enterObject next member
enterObject (ObjectPath parent next) member = ObjectPath parent $ enterObject next member
enterObject (ArrayPath index next) member = ArrayPath index $ enterObject next member

enterArray :: Path -> Int -> Path
enterArray NilPath index = ArrayPath index NilPath
enterArray (RootPath next) index = RootPath $ enterArray next index
enterArray (ObjectPath parent next) index = ObjectPath parent $ enterArray next index
enterArray (ArrayPath index next) index' = ArrayPath index $ enterArray next index'

exitPath :: Path -> Path
exitPath NilPath = error "Cannot exit NilPath path!"
exitPath (RootPath NilPath) = NilPath
exitPath (RootPath next) = RootPath $ exitPath next
exitPath (ObjectPath _ NilPath) = NilPath
exitPath (ObjectPath member next) = ObjectPath member $ exitPath next
exitPath (ArrayPath _ NilPath) = NilPath
exitPath (ArrayPath index next) = ArrayPath index $ exitPath next

getLeaf :: Path -> Path
getLeaf NilPath = NilPath
getLeaf p@(RootPath NilPath) = p
getLeaf (RootPath next) = getLeaf next
getLeaf p@(ObjectPath _ NilPath) = p
getLeaf (ObjectPath member next) = getLeaf next
getLeaf p@(ArrayPath _ NilPath) = p
getLeaf (ArrayPath index next) = getLeaf next
