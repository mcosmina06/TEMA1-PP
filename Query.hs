module Query where

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

splitBy :: Char -> String -> [String]
splitBy c = foldr op [[]] 
              where op x (y:ys)
                      | x /= c = (x:y):ys
                      | otherwise = []:(y:ys)

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col ln str = Table (head str_aux) (init (tail str_aux))
    where
        str_aux = map (splitBy col) (splitBy ln str)

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

{--functia de transpunere, in realizarea functiilor am folosit transpusa matricei
realizata din concatenarea header:entries--}
transpose :: [Entry] -> [Entry]
transpose ([]:_) = []
transpose entries = (map head entries) : transpose(map tail entries) 

{--functia transforma un vector de string-uri intr-un vector 
de int-uri in functie de lungimea cuvantului--}
stringTolength :: [String] -> [Int]
stringTolength [] = []
stringTolength (x:xs) = (length x) : stringTolength(xs)

{--maximul dintr-un vector--}
max' :: Ord a => [a] -> a
max' = foldr1 (\x y ->if x >= y then x else y)

add_space :: Int -> String
add_space 0 = []
add_space cnt = " " ++ (add_space (cnt - 1))

add_lines :: Int -> String
add_lines 0 = []
add_lines cnt = "-" ++ (add_lines (cnt - 1))

{--functie de afisarea a unei singure linii din tabel--}
toStringLine :: Entry -> Int -> [Int] -> String 
toStringLine [] _ _ = []
toStringLine (x:xs) i v = "|" ++ x ++ add_space ((v !! i) - (length x)) 
						++ (toStringLine xs (i + 1) v)

{--functie de afisare a tuturor liniilor ce o apeleaza pe cea anterioara--}
toStringAll :: [Entry] -> Int -> [Int] -> String
toStringAll [] _  _ = []
toStringAll (x:xs) i v = (toStringLine x i v) ++ "|\n" ++ (toStringAll xs i v)

{--max_length reprezinta un vector, de lungime egala cu numarul de elemenente
din header, in care sunt retinute lungimea maxima pentru fiecare coloana--}
show' :: [Entry] -> String
show' [] = []
show' entries = (add_lines (x + (length max_length) + 1))  ++ "\n" 
            ++ toStringLine (head entries) 0 max_length ++ "|\n" 
            ++ (add_lines (x + (length max_length) + 1)) ++ "\n" 
            ++ (toStringAll (tail entries) 0 max_length) 
            ++ (add_lines (x + (length max_length) + 1)) ++ "\n" 
    where
        x = (foldr (+) 0 max_length)
        max_length = map max' (map stringTolength (transpose entries))

instance Show Table where
    show (Table header entries) = show'(header:entries) 

selectAnEntry :: Field -> [Entry] -> Entry
selectAnEntry _ [] = []
selectAnEntry field entries
    | field == head (head entries) = head entries
    | otherwise = selectAnEntry field (tail entries)

selectAllEntries :: [Field] -> [Entry] -> [Entry]
selectAllEntries [] _ = []
selectAllEntries (x:xs) entries
    |foundEntry == [] = selectAllEntries xs entries
    |otherwise = foundEntry : selectAllEntries xs entries
    where
        foundEntry = selectAnEntry x entries

{--functia de selectare foloseste cele doua functii anterioare unde selectAllEntries
realizeaza un [Entry] de field - urile cerute cu ajutorul functie selectAnEntry--}
select :: [Field] -> Query -> Table
select field (Atom (Table header entries)) = Table (head foundEntries) (tail foundEntries)
	where
		foundEntries = transpose (selectAllEntries field (transpose (header:entries)))
select field q = select field (Atom (eval q))

selectLimitaux :: [Entry] -> Integer -> Integer ->[Entry]
selectLimitaux [] _ _ = []
selectLimitaux entries cnt limit
	| cnt <= limit = (entries !! (fromIntegral cnt)):(selectLimitaux entries (cnt + 1) limit)
	| otherwise = []

{--functia ce selecteaza un anumit numar de linii cu header-ul cerut, asemanator
ca cea de select, avand in plus si un contor--}
selectLimit :: [Field] -> Integer -> Query -> Table
selectLimit field limit (Atom (Table header entries)) = Table (head limitEntrie) (tail limitEntrie)
	where
		limitEntrie = selectLimitaux foundEntries 0 limit
		foundEntries = transpose (selectAllEntries field (transpose (header:entries)))
selectLimit field limit q = selectLimit field limit (Atom (eval q))


data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

getFilterLt :: Field -> Integer -> Query -> Table
getFilterLt field limit (Atom (Table header entries)) = Table header (filter (getFilter 
													(Lt field limit) header) entries)
getFilterLt field limit q = getFilterLt field limit (Atom (eval q))

getFilterEq :: Field -> String -> Query -> Table
getFilterEq field str (Atom (Table header entries)) = Table header (filter (getFilter 
													(Eq field str) header) entries)
getFilterEq field limit q = getFilterEq field limit (Atom (eval q))

getFilterIn :: Field -> [String] -> Query -> Table
getFilterIn field str (Atom (Table header entries)) = Table header (filter (getFilter 
													(In field str) header) entries)
getFilterIn field str q = getFilterIn field str (Atom (eval q))

getFilterNot :: FilterCondition -> Query -> Table
getFilterNot condition (Atom (Table header entries)) = Table header (filter 
													(getFilter condition header) entries)
getFilterNot condition q = getFilterNot condition (Atom (eval q))

{--functia returneaza indexul unui field din header--}
getIndexField :: Field -> Int -> TableSchema -> Int
getIndexField field pos (x:xs)
	| field == x = pos
	| otherwise = getIndexField field (pos + 1) xs

{--functia trateaza fiecare caz si intoarce o functie care testeaza conditia impusa si returneaza un bool--}
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field limit) header = 
	(\x -> if (read (x !! (getIndexField field 0 header)) :: Integer) < limit then True else False)
getFilter (Eq field str) header = 
	(\x -> if ((x !! (getIndexField field 0 header))) == str then True else False)
getFilter (In field str) header = 
	(\x -> if (elem (x !! (getIndexField field 0 header)) str) then True else False)
getFilter (Not (Lt field limit)) header = 
	(\x -> if (read (x !! (getIndexField field 0 header)) :: Integer) >= limit then True else False)
getFilter (Not (Eq field str)) header = 
	(\x -> if ((x !! (getIndexField field 0 header))) /= str then True else False)
getFilter (Not (In field str)) header = 
	(\x -> if (elem (x !! (getIndexField field 0 header)) str) then False else True)

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

{--functia de concatenare: ia cele doua yabele si le alipeste--}
concatQuery :: Query -> Query -> Table
concatQuery (Atom (Table header1 entries1)) (Atom (Table header2 entries2)) = 
	Table header1 (entries1 ++ entries2)
concatQuery q1 q2 = concatQuery (Atom (eval q1)) (Atom (eval q2))

eval :: Query -> Table
eval (Atom table) = table
eval (Select fields q) = select fields q
eval (SelectLimit fields limit q) = selectLimit fields limit q
eval (Filter (Lt field limit) q) = getFilterLt field limit q
eval (Filter (Eq field str) q) = getFilterEq field str q
eval (Filter (In field str) q) = getFilterIn field str q
eval (Filter (Not condition) q) = getFilterNot (Not condition) q
eval (q1 :|| q2) = concatQuery q1 q2

getZone :: String -> Table -> String
getZone id (Table header entries)
    | id == head (head entries) = ((head entries) !! 4)
    | otherwise = getZone id  (Table header (tail entries))

same_zone :: String -> Query
same_zone id = Atom (getFilterNot (Not (Eq "user_id" id)) 
            $ Select ["user_id", "occupation"] 
            $ Atom (getFilterEq "zone" zoneToFilter 
            $ Atom user_info))
    where
        zoneToFilter = getZone id user_info

male_within_age :: Integer -> Integer -> Query
male_within_age x y =  Select ["occupation", "zone"] 
                    $ Atom (getFilterNot (Not (Eq "age" (show x))) 
                    $ Atom (getFilterNot (Not (Lt "age" x)) 
                    $ Atom (getFilterLt "age" y 
                    $ Atom (getFilterEq "sex" "M"
                    $ Atom user_info))))

mixed :: [String] -> [String] -> Integer -> Query
mixed listZ listO x = Select ["user_id"] 
                    $ Atom (getFilterIn "zone" listZ
                    $ Atom (getFilterIn "occupation" listO 
                    $ Atom (getFilterLt "age" (toInteger x)
                    $ Atom user_info)))


