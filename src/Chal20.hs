{-# LANGUAGE OverloadedStrings #-}

module Chal20 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Set1 as S1
import qualified Set2 as S2
import qualified Chal17 as C17
import qualified Chal18 as C18
import qualified Chal19 as C19
import qualified Data.ByteString.Base64 as B.Base64

decryptSameNonceCtr :: [B.ByteString] -> B.ByteString
decryptSameNonceCtr listOfCiphers = S1.encodeRepeatingOr (B.concat truncatedCiphers) keyString 
                                  where truncatedCiphers = truncateCiphers listOfCiphers
                                        keyString = getKey (B.concat truncatedCiphers) keyLen
                                        keyLen = B.length $ head truncatedCiphers

getKey :: B.ByteString -> Int -> B.ByteString
getKey ciphertext keySize = B.pack keys 
                          where keys = map snd (map S1.decryptSingleByteCypher (S1.transposedSegments ciphertext keySize))

truncateCiphers :: [B.ByteString] -> [B.ByteString]
truncateCiphers list = truncated
                    where truncated = map (B.take minLen) list
                          minLen = minimum $ map B.length zeroRemoved
                          zeroRemoved = L.delete B.empty list

solve = do fullFile <- B.readFile "20.txt"
           let b64 = B.split '\n' fullFile
           let ciphers = map C19.fixedCtrEncrypt $ map B.Base64.decodeLenient b64
           let trunc = truncateCiphers ciphers
           let answer = decryptSameNonceCtr ciphers
           print answer

-- Hmm this **basically ** works. can't tell if it's just a hallmark of 
	--the truncation that makes it weird, but i think at least one letter is wrong *shrug*
--Sample: 
--Chal20> solve 
--"i'm rated \"R\"...this is a warning, ya better void / Pcuz I came back to attack others in spite- / Strike lbut don't be afraid in the dark, in a park / Not a scya tremble like a alcoholic, muscles tighten up / Whasuddenly you feel like your in a horror flick / You gmusic's the clue, when I come your warned / Apocalypshaven't you ever heard of a MC-murderer? / This is thdeath wish, so come on, step to this /