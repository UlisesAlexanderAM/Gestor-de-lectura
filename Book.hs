module Book where

import Data.Char (isAlpha)
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day)
import Numeric.Natural (Natural)
import Prelude hiding (Word)
import Data.Coerce (coerce)

data Category
  = GeneralLiterature
  | Manga
  | LightNovel
  | Webtoon
  | Webcomic
  | Comic

newtype Letter = Letter Char deriving (Show)

letter :: Char -> Maybe Letter
letter c =
  if isAlpha c
    then Just $ Letter c
    else Nothing

letterString :: String -> [Letter]
letterString = mapMaybe letter

newtype Word = Word [Letter] deriving (Show)

toWord :: String -> Word
toWord = Word . letterString

fromWord :: Word -> String
fromWord = show

newtype FirstName = FirstName Word

fromFirstName :: FirstName -> Word
fromFirstName =  coerce Word

firstNameString :: FirstName -> String
firstNameString = fromWord . fromFirstName

toFirstName :: String -> FirstName
toFirstName = FirstName . toWord

newtype LastName = LastName Word

fromLastName :: LastName -> Word
fromLastName = coerce Word

lastNameString :: LastName -> String
lastNameString = fromWord . fromLastName

toLastName :: String -> LastName
toLastName = LastName . toWord

data FullName
  = OnlyName Word
  | FullName (LastName, FirstName)
  | FullNameM ([LastName], [FirstName])

concatMapFN :: [FirstName] -> String
concatMapFN =  concatMap firstNameString

concatMapLN :: [LastName] -> String
concatMapLN = concatMap lastNameString

instance Show FullName where
    show (OnlyName w) = fromWord w
    show (FullName (lN,fN)) = lastNameString lN ++ ", " ++ firstNameString fN
    show (FullNameM (lNS,fNS)) = concatMapLN lNS ++ ", " ++ concatMapFN fNS

data Person
  = Person FullName
  | Alias String
  | Anonymous

newtype Title = Title String

newtype Language = Language [Letter]

newtype NoPages = NoPages Natural

newtype NoWords = NoWords Natural

newtype Editorial = Editorial String

newtype PublishedDate = PublishedDate Day

newtype Author = Author Person

newtype Translator = Translator Person

data BookFormat
  = PhysicalBook
  | DigitalBook

instance Show BookFormat where
  show PhysicalBook = "Physical Book"
  show DigitalBook = "Digital Book"

newtype Editor = Editor Person

newtype Ilustrator = Ilustrator Person


