{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import qualified Data.List as L
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B.Base64
import qualified Data.ByteString.Base16 as B.Base16
import qualified Data.ByteString.Char8 as B.Char8
import qualified Data.Word as W
import qualified Codec.Crypto.AES as AES
import qualified Data.Char as C

--QUESTION 1 HEX TO BASE 64
bytestring :: B.ByteString
bytestring = "I'm a ByteString"

testInput :: B.ByteString
testInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

fromBase64 :: B.ByteString -> Either String B.Char8.ByteString
fromBase64 input = B.Base64.decode input

fromHex :: B.ByteString -> B.ByteString
fromHex input = fst (B.Base16.decode input)

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 hexString = B.Base64.encode (fromHex hexString)


-- QUESTION 2 FIXED LENGTH XOR
fixedXorHexEconded :: B.ByteString -> B.ByteString -> B.ByteString
fixedXorHexEconded a b = B.Base16.encode (fixedXor (fromHex a) (fromHex b))

--Replace this with a B.zipWith
fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor a b 
    | (B.null a) = B.empty
    | (B.null b) = B.empty
    | otherwise = B.cons (charXor (B.head a) (B.head b)) (fixedXor (B.tail a) (B.tail b))

testStr1 :: B.ByteString
testStr1 = "1c0111001f010100061a024b53535009181c"

testStr2 :: B.ByteString
testStr2 = "686974207468652062756c6c277320657965"

-- QUESTION 3 SINGLE BYTE XOR
--this type sig doesn't work - i cannot fathom what the Word8 is meant to be!
--singleCharXor :: B.ByteString -> GHC.Word.Word8 -> B.ByteString
singleCharXor :: B.ByteString -> Char -> B.ByteString
singleCharXor rawString char = B.map (charXor char) rawString

charXor :: Char -> Char -> Char
charXor a b = C.chr $ Bits.xor (C.ord a) (C.ord b)

testStr3 :: B.ByteString
testStr3 = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

decryptSingleByteCypherHex :: B.ByteString -> (B.ByteString, Char)
decryptSingleByteCypherHex hexString = decryptSingleByteCypher ( fromHex hexString)

decryptSingleByteCypher :: B.ByteString -> (B.ByteString, Char)
decryptSingleByteCypher bString = maxScore (allDecryptions bString)

maxScore :: [(B.ByteString, Char)] -> (B.ByteString, Char)
maxScore list = L.maximumBy maxBString list 

maxBString (a, keyA) (b, keyB) 
    | scoreString a > scoreString b = GT
    | otherwise = LT

--score this string as a piece of english by counting the spaces and
-- the "e"s - crude but good enough for now
-- Again, should just use a fold or (sum B.map...)
scoreString:: B.ByteString -> Int 
--scoreString bString = sum B.Map charScore bString 
scoreString bString 
    | B.null bString = 0
    | isLetter (B.head bString) = 1 + scoreString (B.tail bString)
    | fromEnum (B.head bString) == fromEnum ' ' = 1 + scoreString (B.tail bString)
    | otherwise = scoreString (B.tail bString)

isLetter thing = (fromEnum thing > fromEnum 'A') && (fromEnum thing < fromEnum 'z')

allDecryptions :: B.ByteString -> [(B.ByteString, Char)]
allDecryptions string = zip decryptions keys
                    where decryptions = map (singleCharXor string) keys
                          keys = ['\0'..'\255']

--QUESTION 4 Detect single-char XOR
--They all technically have a max english-ness score, so just map the
--above code over the input and select the most english of those
detect :: [B.ByteString] -> B.ByteString
--detect bStrings = B.intercalate "\n" (map decryptSingleByteCypherHex bStrings)
detect bStrings = fst (maxScore (map decryptSingleByteCypherHex bStrings))

fullInput = B.readFile "xor.txt"

--delimiter :: B.ByteStri
delimiter = B.head "\n"

--I have no idea what I'm doing
answer = do fullInput <- B.readFile "xor.txt"
--detect (B.split delimiter )
            B.putStr (detect (B.split delimiter fullInput))

--QUESTION 5 implement repeating-key XOR
plaintext :: B.ByteString
plaintext = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

key :: B.ByteString
key = "ICE"

expectedCiphertext :: B.ByteString
expectedCiphertext = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

--should be called "...Xor", whoops
encodeRepeatingOr :: B.ByteString -> B.ByteString -> B.ByteString
encodeRepeatingOr bString key  = encodeRepeatingOrHelper B.empty bString key

encodeRepeatingOrHelper bStringSoFar bStringLeft key 
    | B.null bStringLeft = bStringSoFar
    | otherwise = encodeRepeatingOrHelper (B.append bStringSoFar newSection) (B.drop keyLength bStringLeft) key
    where newSection = encodeSection key (B.take keyLength bStringLeft)
          keyLength = (B.length key)

--may not have got a section as long as the key from the above, so trim it if needs be.
encodeSection key section = fixedXor (B.take (B.length section) key) section

--SET 6. 
-- 6i) 
keySizes = [2..40]
--6ii) Hamming distance = number different bits = "1"s in XOR
testStr6a :: B.ByteString
testStr6a = "this is a test"
testStr6b :: B.ByteString
testStr6b = "wokka wokka!!!"

hammingDistace :: B.ByteString -> B.ByteString -> Int 
hammingDistace a b = bitCount (fixedXor a b)

bitCount :: B.ByteString -> Int
bitCount bString = B.foldl (\total byte -> total + Bits.popCount (C.ord byte)) 0 bString

--6iii) Finding keysize
--start with the most basic diff + guess for the time being.
--Later  appears that this wasn't good enough :
keySizeDiff ::  B.ByteString -> Int -> Float
keySizeDiff str keySize = normHamDist firstChunk secChunk keySize
                        where firstChunk = B.take keySize str
                              secChunk = B.take keySize (B.drop keySize str)

keySizeDiffBetter ::  B.ByteString -> Int -> Float
keySizeDiffBetter str keySize = keySizeDiffBetterHelp firstChunk secChunk thirdChunk forthChunk keySize
                              where forthChunk = B.take keySize thirdRest
                                    (thirdChunk, forthRest) = B.splitAt keySize thirdRest
                                    (secChunk, thirdRest) = B.splitAt keySize secRest
                                    (firstChunk, secRest) = B.splitAt keySize str

keySizeDiffBetterHelp first second third forth size = (normHamDist first second size) + (normHamDist first third size) + (normHamDist first forth size) + (normHamDist second third size) + (normHamDist second forth size) +(normHamDist third forth size) 

normHamDist :: B.ByteString -> B.ByteString -> Int -> Float
normHamDist firstChunk secChunk chunkLen = fromIntegral (hammingDistace firstChunk secChunk) / fromIntegral chunkLen

guessKeySize :: B.ByteString -> Int
guessKeySize bString = L.maximumBy (keySizeDiffComp bString) keySizes
--guessKeySize bString = map (keySizeDiff bString) keySizes

--IF the difference is larger, then it's less likley to be the key
keySizeDiffComp bString a b 
    | keySizeDiffA < keySizeDiffB = GT
    | keySizeDiffA == keySizeDiffB = EQ
    | otherwise = LT
    where keySizeDiffA = keySizeDiffBetter bString a
          keySizeDiffB = keySizeDiffBetter bString b
--6iv)
probableKeyLength = do ciphertext <- B.readFile "6.txt"
                       --print (guessKeySize (B.Base64.decodeLenient ciphertext))
                       --print (map (keySizeDiff (B.Base64.decodeLenient ciphertext)) keySizes)
                       print (L.maximumBy (keySizeDiffComp (B.Base64.decodeLenient ciphertext)) keySizes)

--6v) In built way to do this?
segmentByte ::  B.ByteString -> Int -> [B.ByteString]
segmentByte bString n 
    | B.null bString = [] 
    | otherwise = (B.take n bString) : segmentByte (B.drop n bString) n

--6vi) 
transposedSegments :: B.ByteString -> Int -> [B.ByteString]
transposedSegments bString n = B.transpose (segmentByte bString n)

--6vii)
--Takes in base 64!

--solveHelper :: B.ByteString -> [Char] -> B.ByteString
solveHelper ciphertext key = encodeRepeatingOr (B.Base64.decodeLenient ciphertext) (B.pack key)

getKey :: B.ByteString -> Int -> B.ByteString
getKey ciphertext keySize = B.pack keys 
                          where keys = map snd (map decryptSingleByteCypher (transposedSegments unBasedCiphertext keySize))
                                --probableKeyLength = guessKeySize unBasedCiphertext
                                unBasedCiphertext = (B.Base64.decodeLenient ciphertext)

--solveHelper ciphertext key = map (solveHelper1 ciphertext) keySizesReverseLikelyhood
   --                    where keySizesReverseLikelyhood = (L.sortBy (keySizeDiffComp (B.Base64.decodeLenient ciphertext)) keySizes)

solvedKey = do ciphertext <- B.readFile "6.txt"
               --print (guessKeySize (B.Base64.decodeLenient ciphertext))
               --print (head (transposedSegments (B.Base64.decodeLenient ciphertext) (guessKeySize (B.Base64.decodeLenient ciphertext))))
               --let probableKeyLength = guessKeySize (B.Base64.decodeLenient ciphertext)
               let key = getKey ciphertext (guessKeySize (B.Base64.decodeLenient ciphertext))
               print "THE KEY IS"
               print key 
               print "THE DECRYPTED TEXT IS"
               print (encodeRepeatingOr (B.Base64.decodeLenient ciphertext) key)

-- 7) AES ECB decryption 

aesKey = "YELLOW SUBMARINE"

--AES makes us supply an IV even though its not used for ECB
--Just use aesKey as we need a 16byte thing to fill that slot!  
aesEcbDecrypt key ciphertext = AES.crypt' AES.ECB key aesKey AES.Decrypt ciphertext

solveAes = do ciphertext <- B.readFile "7.txt"
              --print (AES.crypt' AES.ECB aesKey aesKey AES.Decrypt (B.Base64.decodeLenient ciphertext))
              print (aesEcbDecrypt aesKey (B.Base64.decodeLenient ciphertext))

-- 8) Detect AES ECB decryption

--split each bytestring into 16byte
--count highest unique

--This might be a v slow method (are list concats slow?) but probably doesn't matter
chunks16 :: B.ByteString -> [B.ByteString]
chunks16 bString
    | B.null bString = []
    | otherwise = [chunk] ++ (chunks16 rest)
                where (chunk, rest) = B.splitAt 16 bString

--how many times is the most repeated chunk of a string repeated?
mostRepeats :: B.ByteString -> Int
mostRepeats bString
    | B.null bString = 0
    | otherwise = mostRepeatsHelper 0 B.empty 0 (L.sort (chunks16 bString)) 

mostRepeatsComp a b 
    | mostRepeats a > mostRepeats b = GT
    | otherwise = LT

mostRepeatsHelper :: Int -> B.ByteString -> Int -> [B.ByteString] -> Int
mostRepeatsHelper maxScore curChunk curScore [] = max curScore maxScore
mostRepeatsHelper maxScore curChunk curScore (nextChunk:rest)
    | nextChunk == curChunk = mostRepeatsHelper maxScore curChunk (curScore + 1) rest
    | otherwise = mostRepeatsHelper (max curScore maxScore) nextChunk 1 rest  

--This is not actually right, need to do the split BEFORE the fromHex
detectAes = do ciphertext <- B.readFile "8.txt"
               print (L.maximumBy mostRepeatsComp (map fromHex (B.split delimiter ciphertext)))