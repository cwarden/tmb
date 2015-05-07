import Data.List (sort, sortBy, (\\))
import Data.Function (on)
import Data.DateTime
import Data.Maybe

data KeepSpec = KeepSpec {
	howMany :: Integer,
	minDiff :: Integer
} deriving (Show)

validDates :: [Maybe DateTime] -> [DateTime]
validDates [] = []
validDates (Nothing : xs) = validDates xs
validDates (Just x : xs) = x : validDates xs

keepers :: [DateTime] -> [KeepSpec] -> [DateTime]
keepers x = keepers' $ reverse . sort $ (epoch:x)
	where
		epoch = fromGregorian' 1900 1 1
		keepers' :: [DateTime] -> [KeepSpec] -> [DateTime]
		keepers' (_:[]) _ = []
		keepers' _ [] = []
		keepers' xs (KeepSpec { howMany = 0, minDiff = _ } : moreSpecs) = keepers' xs moreSpecs
		keepers' (first : second : rest) allSpecs@(spec:moreSpecs) | diffSeconds first second < minDiff spec = keepers' (first:rest) allSpecs
		keepers' (first : second : rest) (spec:moreSpecs) = first : keepers' (second:rest) (newSpec:moreSpecs)
			where newSpec = KeepSpec { howMany = howMany spec - 1, minDiff = minDiff spec }

toDelete :: [DateTime] -> [KeepSpec] -> [DateTime]
toDelete dates specs = sorted \\ keepDates
	where
		sorted = sort dates
		keepDates = keepers sorted specs

extraBackups :: String -> String
extraBackups input =
	let
		valid = validDates . map fromSqlString $ lines input
		keepSpecs = sortBy (compare `on` minDiff) [ KeepSpec { howMany = 12, minDiff = 30 * 3600 * 24 }, KeepSpec { howMany = 4, minDiff = 7 * 3600 * 24 }, KeepSpec { howMany = 7, minDiff = 1 * 3600 * 18 } ]
		deleteDateStrings = map toSqlString $ toDelete valid keepSpecs
	in unlines deleteDateStrings

main = interact extraBackups
