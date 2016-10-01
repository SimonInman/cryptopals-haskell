{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Word as W
import qualified Data.Char as C
import qualified Codec.Crypto.AES as AES
import qualified Set1 as S1
import qualified Data.ByteString.Base64 as B.Base64
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as B.Char8
import System.Entropy
import System.Random

-- Challenge 9 Implement PKCS#7 padding
-- I don't think this is actually right, it's not clear what encoding
-- the text to pad is in. 

--Just returns original string when blocksize is already too big 
-- not clear from site if that was the desired behaviour? 
toPad :: B.ByteString
toPad = "YELLOW SUBMARINE"

pkcsPad :: B.ByteString -> Int -> B.ByteString
pkcsPad block sizeToPadTo
    | B.length block > sizeToPadTo = block
    | otherwise = B.append block (padderHelper padLen)
                where padLen = sizeToPadTo - (B.length block)

padderHelper :: Int -> B.ByteString
padderHelper len = B.concat (replicate len (padderHelper2 len))

padderHelper2 :: Int -> B.ByteString
padderHelper2 1 = "\x01"
padderHelper2 2 = "\x02"
padderHelper2 3 = "\x03"
padderHelper2 4 = "\x04"
padderHelper2 5 = "\x05"
padderHelper2 6 = "\x06"
padderHelper2 7 = "\x07"
padderHelper2 8 = "\x08"
padderHelper2 9 = "\x09"
padderHelper2 10 = "\x0a"
padderHelper2 11 = "\x0b"
padderHelper2 12 = "\x0c"
padderHelper2 13 = "\x0d"
padderHelper2 14 = "\x0e"
padderHelper2 15 = "\x0f"
padderHelper2 x = "ERROOR" -- :< Erlang would be happy to leave this out and crash

-- Challenge 10

--encrypt as decrypted before - again toPad is used as it is an arbitraty 16 byte bString
aesEcbEncrypt key ciphertext = AES.crypt' AES.ECB key toPad AES.Encrypt ciphertext

--test using toPad as the key
testEncryptDecrypt text = S1.aesEcbDecrypt toPad (aesEcbEncrypt toPad text)

testEncryptDecryptReal = do testText <- (B.readFile "10.txt")
                            print (testText == (testEncryptDecrypt testText))

aesCbcEncrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
aesCbcEncrypt key iv plaintext = aesCbcEncryptHelper [] (key, iv) (S1.chunks16 plaintext)

aesCbcEncryptHelper :: [B.ByteString] -> (B.ByteString, B.ByteString) -> [B.ByteString] -> B.ByteString
aesCbcEncryptHelper soFar (key, mix) [] = B.concat soFar
aesCbcEncryptHelper soFar (key, mixBlock) (plainChunk:plainChunks) = 
    let newCipherBlock = encryptCbcBlock plainChunk mixBlock key 
    in aesCbcEncryptHelper (soFar ++ [newCipherBlock]) (key, newCipherBlock) plainChunks

--Do CBC encryption using our ECB encryption
encryptCbcBlock :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
encryptCbcBlock plainBlock otherBlock key = aesEcbEncrypt key (S1.fixedXor plainBlock otherBlock)

--Decrypt is just the same thing as encrypt, except for the call back into S1, I htink,
-- so that could just be written into a general function. 
aesCbcDecrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
--aesCbcDecrypt key iv ciphertext = aesCbcDecryptHelper [] (key, iv) (S1.chunks16 ciphertext)
aesCbcDecrypt key iv ciphertext
    | B.null ciphertext = B.empty
    | otherwise = B.append plainBlock (aesCbcDecrypt key firstCipherBlock rest) 
    where (firstCipherBlock, rest) = B.splitAt 16 ciphertext
          plainBlock = S1.fixedXor iv (S1.aesEcbDecrypt key firstCipherBlock)

nullIv :: B.ByteString
nullIv = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

testCbc = do ciphertext <- (B.readFile "10.txt")
             print (aesCbcDecrypt "YELLOW SUBMARINE" nullIv (B.Base64.decodeLenient ciphertext))

-- challenge 11
--random aes key (i.e 16 byte)
randOracle = do randKey <- getEntropy 16
                rand1 <- randomRIO(5,10)
                rand2 <- randomRIO(5,10)
                randPad1 <- getEntropy rand1
                randPad2 <- getEntropy rand2
                ecbOrCbc <- randomRIO(0,1)
                print ecbOrCbc
                ciphertext <- (B.readFile "10.txt")
                print (encryptionOracle (randKey, ecbOrCbc, randPad1, randPad2) "YELLOW SUBMARINE")

--use an arbitrary digit from Key to decide if on ECB vs CBC
-- also just use the key as an IV too because meh
encryptionOracle :: (B.ByteString, Int, B.ByteString, B.ByteString) -> B.ByteString -> B.ByteString
encryptionOracle (randKey, useAes, pad1, pad2) plaintext
    | useAes == 0 = aesCbcEncrypt randKey randKey paddedText
    | otherwise = aesEcbEncrypt randKey paddedText
    where paddedText = (padTo16Multiple (B.append (B.append pad1 plaintext) pad2))

padTo16Multiple :: B.ByteString -> B.ByteString
padTo16Multiple bString
    | (B.length bString) `mod` 16 == 0 = bString
    | otherwise = pkcsPad bString sizeToPadTo
    where sizeToPadTo = (((B.length bString) `div` 16 ) + 1) * 16

testPlaintext :: B.ByteString
testPlaintext = "YELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINE"

isEcbNotCbc :: (B.ByteString -> B.ByteString) -> Bool
isEcbNotCbc encryptionOracle = S1.mostRepeats (encryptionOracle testPlaintext) > 1

testOracleDetector = do randKey <- getEntropy 16
                        rand1 <- randomRIO(5,10)
                        rand2 <- randomRIO(5,10)
                        randPad1 <- getEntropy rand1
                        randPad2 <- getEntropy rand2
                        ecbOrCbc <- randomRIO(0,1)
                        print ecbOrCbc
                        print randPad1
                        print randPad2
                        print (isEcbNotCbc (encryptionOracle (randKey, ecbOrCbc, randPad1, randPad2)))

-- Challence 12 

-- Just get an arbitrary 16 bytes by encoding this strings 
fixedKey :: B.ByteString
fixedKey = aesEcbEncrypt "YELLOW SUBMARINE" "YELLOW SUBMARINE"

toAppend :: B.ByteString
toAppend = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

encrypter12 :: B.ByteString -> B.ByteString
encrypter12 plaintext = aesEcbEncrypt fixedKey (padTo16Multiple (B.append plaintext (B.Base64.decodeLenient toAppend)))

-- If encrypt(unknown) = ciphertext
-- then encrypt(<key size bytes> || unknown) = <key size garbage> || ciphertext
-- so find first integer that does that!
findBlockSize :: (B.ByteString -> B.ByteString) -> Int
findBlockSize oracle = head (filter (isBlockSize refString) [1..])
                     where refString = (oracle B.empty)

isBlockSize :: B.ByteString -> Int -> Bool
isBlockSize refString guessedSize = 
    let outputString = encrypter12 (B.concat (replicate guessedSize "A"))
    in B.drop guessedSize outputString == refString

--Main System.Random> findBlockSize encrypter12 
--16
--Main System.Random> isEcbNotCbc encrypter12 
--True

decryptOracle :: (B.ByteString -> B.ByteString) -> B.ByteString
decryptOracle oracle =
    let blockSize = findBlockSize oracle
        lenHidden = B.length (oracle B.empty)
    in decryptOracleHelper oracle B.empty lenHidden blockSize

--Decript block by block till we have the whole thing. 
decryptOracleHelper :: (B.ByteString -> B.ByteString) -> B.ByteString -> Int -> Int -> B.ByteString
decryptOracleHelper oracle knownSoFar desiredLen blockSize
    | (B.length knownSoFar) >= desiredLen = knownSoFar
    | otherwise = 
    let newKnown = decryptByteByByteHelper oracle knownSoFar (blockSize + B.length knownSoFar)
    in decryptOracleHelper oracle newKnown desiredLen blockSize

decryptByteByByteHelper :: (B.ByteString -> B.ByteString) -> B.ByteString -> Int -> B.ByteString
decryptByteByByteHelper oracle knownSoFar desiredLen
    | B.length knownSoFar == desiredLen = knownSoFar
    | otherwise = 
    let nextChar = B.singleton (returnNextChar oracle knownSoFar desiredLen)
    in decryptByteByByteHelper oracle (B.append knownSoFar nextChar) desiredLen

returnNextChar :: (B.ByteString -> B.ByteString) -> B.ByteString -> Int -> Char
returnNextChar oracle knownText desiredLen =
    --let blockSize = findBlockSize oracle
    let bufferString = (B.concat (replicate (desiredLen - (1 + B.length knownText)) "A"))
        keys = ['\0'..'\255']
        correctCharList = (filter (isNextChar oracle bufferString knownText) keys)
    in head correctCharList

isNextChar :: (B.ByteString -> B.ByteString) -> B.ByteString -> B.ByteString -> Char -> Bool
--This is a hack: The message doesn't fill a block and this breaks the decoding. I would tidy
--it up, but it seemed simpler to just whack a load of 255 chars on the end - may come back to bite though!
isNextChar _ _ _ '\255' = True
isNextChar oracle bufferString knownText charUnderTest = 
    let guessBuffer = B.append bufferString (B.append knownText (B.singleton charUnderTest))
        neededLen = B.length guessBuffer
    in (B.take neededLen (oracle guessBuffer)) == (B.take neededLen (oracle bufferString))

-- Challenge 13: hacks

data Profile = Profile { email :: String
                       , uid :: Int
                       , role :: String 
                       } deriving (Show)

class Encode a where
    encode :: a -> String

instance Encode Profile where
    encode prof = "email=" ++ (email prof)  ++ "&uid=" ++ show (uid prof) ++ "&role=" ++ (role prof)

-- NOT the generic decoder the exercise asked for, but meh.
-- I actually don't know the sensible way to do this, maybe there's a more idiomatic way. 
decodeProf :: String -> Profile
decodeProf prof = 
    let parts = splitOn '&' prof
        value = splitOn '='
        parsedParts = map value parts
    in Profile{email = (getValue "email" parsedParts), uid = (read (getValue "uid" parsedParts)), role = (getValue "role" parsedParts)}

splitOn :: Char -> String -> [String]
splitOn splitter [] = []
splitOn splitter remainder =
    let (before, after) = L.span (/= splitter) remainder
        rest = if after == [] then [] else (tail after)
    in [before] ++ (splitOn splitter rest)

getValue :: String -> [[String]] -> String
getValue keyWanted [] = "ERROR" 
getValue keyWanted ([key, value]:rest)
    | key == keyWanted = value
    | otherwise = getValue keyWanted rest

-- I expected this would be built in?
cleanString :: String -> Char -> String
cleanString [] char = []
cleanString (h:rest) char
    | h == char = cleanString rest char
    | otherwise = [h] ++ cleanString rest char


profileFor :: String -> Profile
profileFor email = Profile{email = cleanEmail, uid = 10, role = "user"}
                 where cleanEmail = cleanString (cleanString email '&') '=' 

encryptProfile :: Profile -> B.ByteString
encryptProfile prof = aesEcbEncrypt fixedKey (padTo16Multiple (B.Char8.pack (encode prof)))

decryptProfile :: B.ByteString -> Profile
decryptProfile bString = 
    let decrypted = S1.aesEcbDecrypt fixedKey bString
        reversed = reverse (B.unpack decrypted)
        stripped = (dropWhile ( == '\n') reversed)
    in decodeProf (reverse stripped)

-- What do we want know? a ciphertext that has role = admin at the end. 
-- if we pick the right length email, we can get plaintext blocks that line up as follows:
-- 0 1 2 3 ...11 12 13 14 15 | 0123 4 5 6...
-- b l a h ... r  o  l  e  = | user\n\n\n
-- That means if we can get a block that looks like "admin\n\n\n..." we can replace
-- the last block above with that and we win!

--Format is email=<email>&uid=10&role=user
-- so there are 19 packing chars before "user", so we need to add 13 to get a round block
encrypterFromStr :: String -> B.ByteString
encrypterFromStr str = encryptProfile (profileFor str)

firstBlocks = B.take 32 (encrypterFromStr "abcd@efgh.com")

--Now to get our "admin" block. We want the second block from it
toEncrypt :: String
toEncrypt = prefiller ++ block
          where prefiller = replicate prefillLen 'A'
                prefillLen = (16 - 6) -- (L.length "email="))
                block = "admin" ++ replicate 11 '\n'

lastBlock = B.take 16 (B.drop 16 encrypted)
          where encrypted = encrypterFromStr toEncrypt

solve13 = decryptProfile (B.append firstBlocks lastBlock)

--Main> solve13 
--Profile {email = "abcd@efgh.com", uid = 10, role = "admin"}
-- awwwwwww yeah

--14 Byte at a time decryption with prefix
--modify 12 so that you get 
-- AES-128-ECB(random-prefix (r-p) || attacker-controlled (a-c) || target-bytes (t-b), random-key)
-- note that there are 5 types of block above: 
-- just r-p; r-p and a-c; just a-c; a-c and t-b; and just t-b.

encrypter14 :: B.ByteString -> B.ByteString
encrypter14 plaintext = aesEcbEncrypt fixedKey (padTo16Multiple (B.append arbitraryBytes (B.append plaintext (B.Base64.decodeLenient toAppend))))
                      where arbitraryBytes = B.take 19 testPlaintext -- picked at random being more than block size.

-- First question: how many blocks of just r-p? we don't care about the contents of those. 
-- Work out the first block that changes when we go from some to no input?
numRandBlocks :: (B.ByteString -> B.ByteString) -> Int
numRandBlocks oracle = 
    let zeroInput = S1.chunks16 (oracle B.empty)
        singleInput = S1.chunks16 (oracle (B.singleton 'A'))
    in lenSamePrefix zeroInput singleInput

--Hmm, inbuilt way to do this?
lenSamePrefix [] l2 = 0
lenSamePrefix l1 [] = 0
lenSamePrefix (h1:t1) (h2:t2)
    | h1 == h2 = 1 + (lenSamePrefix t1 t2)
    | otherwise = 0 
--Main> numRandBlocks encrypter14
--1

-- now find the length of the remaining r-p. 
-- How much attacker controlled text do we need to add before we get into the next block?
changesOneBlock :: (B.ByteString -> B.ByteString) -> Int -> Bool
changesOneBlock oracle guessedSize = 
    let aString = B.concat (replicate guessedSize "A")
        bString = B.concat (replicate guessedSize "B")
        aInput = (S1.chunks16 (oracle aString))
        bInput = (S1.chunks16 (oracle bString))
    in (countSame aInput bInput == 1) 

countSame [] l2 = 0
countSame l1 [] = 0
countSame (h1:t1) (h2:t2)
    | h1 == h2 = countSame t1 t2
    | otherwise = 1 + countSame t1 t2

lenRP :: (B.ByteString -> B.ByteString) -> (Int, Int) -- (whole blocks, leftover)
lenRP oracle =
    let randBlocks = numRandBlocks oracle
        maxLen = maximum (filter (changesOneBlock oracle) [1..16])
        leftover = 16 - maxLen
    in (randBlocks, leftover)

--Main> lenRP encrypter14
--19   
-- Correct, but feels like the above could be done with less code somehow

--So now i think we shold be able to answer this as per Q12. 
-- as the blocks don't effect one another, we can add a bit of filler to the beginning
-- of our attack string, and essentially be back to having no prefix string!

--evidently possible 

solvePrefixOracle :: (B.ByteString -> B.ByteString) -> B.ByteString
solvePrefixOracle oracle =
    let (blocks, leftover) = lenRP oracle
        zeroInputLen = B.length (oracle B.empty)
        interestingLen = zeroInputLen - (16 * blocks) - leftover
    in solvePrefixHelper oracle B.empty interestingLen

solvePrefixHelper oracle knownSoFar desiredLen 
    | (B.length knownSoFar) >= desiredLen = knownSoFar
    | otherwise = 
    let newKnown = solve14Block oracle knownSoFar (16 + B.length knownSoFar)
    in solvePrefixHelper oracle newKnown desiredLen

solve14Block :: (B.ByteString -> B.ByteString) -> B.ByteString -> Int -> B.ByteString
solve14Block oracle knownSoFar desiredLen
    | (B.length knownSoFar >= desiredLen) = knownSoFar
    | otherwise = 
    let nextChar = B.singleton (getNextCharPrefix oracle (lenRP oracle) knownSoFar desiredLen)
    in solve14Block oracle (B.append knownSoFar nextChar) desiredLen


getNextCharPrefix :: (B.ByteString -> B.ByteString) -> (Int, Int) -> B.ByteString -> Int -> Char
getNextCharPrefix oracle (rPBlocks, rPRest) knownSoFar desiredLen =
    let preBuffer = B.concat (replicate (16 - rPRest) "A") --hardcode blocksize, cba
        bufferString = (B.concat (replicate (desiredLen - (1 + B.length knownSoFar)) "A"))
        keys = ['\0'..'\255']
        correctCharList = (filter (isNextCharPrefix oracle (rPBlocks, preBuffer) bufferString knownSoFar) keys)
    in head correctCharList

isNextCharPrefix :: (B.ByteString -> B.ByteString) -> (Int, B.ByteString) -> B.ByteString -> B.ByteString -> Char -> Bool
--Same hack as 12 needed
isNextCharPrefix _ _ _ _ '\255' = True
isNextCharPrefix oracle (rPBlocks, preBuffer) bufferString knownText charUnderTest = 
    let guessBuffer = (B.append bufferString (B.append knownText (B.singleton charUnderTest)))
        neededLen = B.length guessBuffer
        interestingPart = B.take neededLen . B.drop (16 * (rPBlocks +1 )) --hmm what about where rest = 0? A: WOrks if rPRest above is 16?
        preBufferAndEncrypt = oracle . B.append preBuffer
    in (interestingPart (preBufferAndEncrypt guessBuffer)) == interestingPart (preBufferAndEncrypt bufferString)

-- 15 PKCS#7 padding 
stripPadding :: B.ByteString -> Maybe B.ByteString
stripPadding bString
    | B.length bString /= 16 = Nothing
    | hasValidPadding bString expectedPadLen = Just (stripValidPadding bString expectedPadLen)
    | otherwise = Nothing
    where lastChar = B.index bString 15
          validPads = ['\1'..'\15']
          expectedPadLen = (+1) <$> (L.elemIndex lastChar validPads)
    
hasValidPadding :: B.ByteString -> Maybe Int -> Bool
hasValidPadding bString Nothing = True
hasValidPadding bString (Just padLen) = 
    let actualPad = B.drop (16 - padLen) bString
        expectedPad = padderHelper padLen
    in actualPad == expectedPad

stripValidPadding bString Nothing = bString
stripValidPadding bString (Just padLen) = B.take (16 - padLen) bString

testStr :: B.ByteString
testStr = "ICE ICE BABY\x01\x02\x02\x02"

-- 16 CBC Bitflipping attacks

encrypter16 :: String -> B.ByteString
encrypter16 plaintext = 
    --Don't think I actually need to quote these out?
    let prepend = "comment1=cooking%20MCs;userdata="
        append = ";comment2=%20like%20a%20pound%20of%20bacon"
        toEncrypt = padTo16Multiple (B.append prepend (B.append (B.Char8.pack plaintext) append))
    in aesCbcEncrypt fixedKey fixedKey toEncrypt

decrypt16HasAdmin :: B.ByteString -> Bool
decrypt16HasAdmin ciphertext = 
    let decryption = aesCbcDecrypt fixedKey fixedKey ciphertext
        --atIndex = B.findSubstring ";admin=true;" decryption
    in case B.breakSubstring ";admin=true;" decryption of 
          (x, y) | B.null y -> False
                 | otherwise -> True

-- The final step of decrypting CBC block N+1 is to XOR the ciphertext of block
-- N against the decrypted plaintext of block N+1 to uncover the original message.
-- Thus mangling a single bit of block N will flip the same bit in the 
-- "decrypted" N+1 block (while mangling block N, of course)

innocentBlock :: String
innocentBlock = "datumXadminXtrue"

-- "prepend" above is 2 complete blocks, so innocentBlock will be encrypted as 
-- the third block under encrypter16
-- so if we flip bits in the 6th and 12th bytes of the second cipher block ,
-- we can turn those X's into a ';' and a '='!

--This can be used to work out bits to flip, and to get the 
bitsToFlip :: Char -> Char -> Char
bitsToFlip current target = C.chr $ Bits.xor (C.ord current) (C.ord target)

--Takes zero indexed index. 
flipBits :: B.ByteString -> Int -> (Char, Char) -> B.ByteString
flipBits bString index (realPlainChar, targetPlainChar) = B.append before $ B.append newChar after
         where (before, rest) = B.splitAt index bString
               (realCipherChar, after)  = (B.head rest, B.tail rest)
               toFlip = bitsToFlip realPlainChar targetPlainChar
               newChar = B.singleton $ bitsToFlip realCipherChar toFlip

attackBlock :: B.ByteString
attackBlock = 
    let cipherBlock = encrypter16 innocentBlock
        (toAttack, toKeep) = B.splitAt 32 cipherBlock
        attackedBlock = flipBits (flipBits toAttack (16 + 5) ('X', ';')) (16 + 11) ('X','=')
    in B.append attackedBlock toKeep

--Main> decrypt16HasAdmin attackBlock 
--True
