-- Ceasar cipher
ceasar :: String -> IO()
ceasar string = ceasarTail string 0

--parameters: ciphertext -> character offset -> console output
ceasarTail :: String -> Int -> IO()
ceasarTail string 26  = outputLine $ shiftString string 26
ceasarTail string x   = do
    outputLine $ shiftString string x
    outputLine " "
    ceasarTail string (x + 1)

shiftString :: String -> Int -> String
shiftString string 0  = string
shiftString string n  = shiftStringTail string [] n

shiftStringTail :: String -> String -> Int -> String 
shiftStringTail [] acc _      = acc
shiftStringTail (x:xs) acc n  = shiftStringTail xs (acc ++ [shiftChar x n]) n

--Vigenere cipher

--parameters: ciphertext -> key -> console output
vigenere :: String -> String -> IO()
vigenere str key = outputLine $ vigenereTail str key 0 (length key) []  

--parameters:  ciphertext -> key -> current key index -> key length -> plaintext accumulator -> plaintext
vigenereTail:: String -> String -> Int -> Int -> String -> String
vigenereTail [] _ _ _ acc                       = acc
vigenereTail ('|':xs) key keyPos keyLength acc  = vigenereTail xs key keyPos keyLength (acc ++ [' '])
vigenereTail (x:xs) key keyPos keyLength acc    = 
    vigenereTail xs key ((keyPos + 1 )`mod` keyLength) keyLength (acc ++ [shiftChar x (0 - charToInt(key !! keyPos))])

--parameters: Ciphertext -> Plaintext -> Key
vigenereGetKey :: String -> String -> String
vigenereGetKey [] []         = []
vigenereGetKey (x:xs) (y:ys) = charset !! (((charToInt x) - (charToInt y)) `mod` 26) : vigenereGetKey xs ys

--Split string based on a known keylength to look for duplicate strings
splitX :: String -> Int -> IO()
splitX string 0 = outputLine string
splitX string x = outputLine $ splitXTail string [] x 1

splitXTail :: String -> String -> Int -> Int -> String
splitXTail [] acc _ _                       = acc 
splitXTail (x:xs) acc len pos | len == pos  = splitXTail xs (acc ++ [x] ++ "\n") len 1
splitXTail (x:xs) acc len pos               = splitXTail xs (acc ++ [x]) len (pos + 1)

-- column transposition
gridBuild :: String -> Int -> IO()
gridBuild string columnLength = outputLine $ gridBuildTail string columnLength 1 []

gridBuildTail :: String -> Int -> Int -> String -> String
gridBuildTail [] _ _ acc = acc 
gridBuildTail (x:xs) columnLength pos acc | (pos `mod` columnLength) == 0 = gridBuildTail xs columnLength (pos + 1) (acc ++ [x] ++ "\n\n---\n\n")
gridBuildTail (x:xs) columnLength pos acc                                 = gridBuildTail xs columnLength (pos + 1) (acc ++ [x] ++ "\n")

-- Utility functions
outputLine :: String -> IO()
outputLine s = putStrLn s

charset = ['A'..'Z']

shiftChar :: Char -> Int -> Char
shiftChar '|' _ = ' '
shiftChar c n   =  charset !! (((charToInt c) + n) `mod` 26)


charToInt :: Char -> Int
charToInt 'A' = 0
charToInt 'B' = 1
charToInt 'C' = 2
charToInt 'D' = 3
charToInt 'E' = 4
charToInt 'F' = 5
charToInt 'G' = 6
charToInt 'H' = 7
charToInt 'I' = 8
charToInt 'J' = 9
charToInt 'K' = 10
charToInt 'L' = 11
charToInt 'M' = 12
charToInt 'N' = 13
charToInt 'O' = 14
charToInt 'P' = 15
charToInt 'Q' = 16
charToInt 'R' = 17
charToInt 'S' = 18
charToInt 'T' = 19
charToInt 'U' = 20
charToInt 'V' = 21
charToInt 'W' = 22
charToInt 'X' = 23
charToInt 'Y' = 24
charToInt 'Z' = 25
charToInt  _  = undefined
