module Book where

import Data.Char ( isAlpha )
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)
data Category =
    GeneralLiterature
    | Manga
    | LightNovel
    | Webtoon
    | Webcomic
    | Comic

newtype Letter = Letter Char

letter :: Char -> Maybe Letter
letter c = if isAlpha c
    then Just $ Letter c
    else Nothing

letterString :: String -> [Letter]
letterString = mapMaybe letter

newtype Name = Name [Letter]

newtype FullName = FullName Name

newtype Title = Title String

newtype Language = Language [Letter]

newtype NoPages = NoPages Natural

newtype NoWords = NoWords Natural

newtype Editorial = Editorial String

