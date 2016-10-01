{-# LANGUAGE OverloadedStrings #-}

module Chal18 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Serialize as Serialize
import qualified Set1 as S1
import qualified Set2 as S2
import qualified Chal17 as C17
import qualified Data.ByteString.Base64 as B.Base64

--Challenge 18 Implement CTR, the stream cipher mode

key18 :: B.ByteString
key18 = "YELLOW SUBMARINE"

plainText18 :: B.ByteString
plainText18 = "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

aesCtrCrypt :: B.ByteString -> Int -> B.ByteString -> B.ByteString
aesCtrCrypt key nonce text = C17.bStringXor text ks
                           where ks = (keyStream key nonce (B.length text))

--Hmm this would be much prettier with lazy byte strings?
-- Just xor with  B.concat $ map  (keyStreamN key nonce) [1..] ?
keyStream :: B.ByteString -> Int -> Int -> B.ByteString
keyStream key nonce len = 
    let infiniteStream = map (keyStreamN key nonce) [0..]
        blocksNeeded = (len `div` 16) + 1 -- don't always need the extra one, but it doesn't matter. 
    in B.concat $ take blocksNeeded infiniteStream

keyStreamN key nonce n = S2.aesEcbEncrypt key counterString
                       where counterString = B.append nonceString nString 
                             nonceString = intToLilEndByteString nonce
                             nString = intToLilEndByteString n

intToLilEndByteString :: Int -> B.ByteString
intToLilEndByteString n = B.reverse $ Serialize.encode n

solve :: B.ByteString
solve = aesCtrCrypt key18 0 (B.Base64.decodeLenient plainText18)

--Main> solve 
--"Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby "
