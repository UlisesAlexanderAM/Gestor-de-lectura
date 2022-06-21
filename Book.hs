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

newtype Name = Name [Letter]

newtype Title = Title String

newtype Language = Language [Letter]

newtype FullName = FullName [Letter]

-- data Author = 
--     FullName
--     | Alias
--     | Anonymous

newtype NoPages = NoPages Natural

newtype NoWords = NoWords Natural

newtype Editorial = Editorial String

newtype Letter = Letter Char

letter :: Char -> Maybe Letter
letter x = if isAlpha x
    then Just $ Letter x
    else Nothing

letterString :: String -> [Letter]
letterString = mapMaybe letter

