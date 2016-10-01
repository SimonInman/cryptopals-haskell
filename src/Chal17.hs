{-# LANGUAGE OverloadedStrings #-}

module Chal17 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Set1 as S1
import qualified Set2 as S2
import System.Entropy
import System.Random
import qualified Data.Bits as Bits
import qualified Data.Char as C


--17 CBC padding oracle

-- Just get an arbitrary 16 bytes by encoding this strings, as before
fixedKey :: B.ByteString
fixedKey = S2.aesEcbEncrypt "YELLOW SUBMARINE" "YELLOW SUBMARINE"

--first just make this work with fixed plain text 
plaintext17a :: B.ByteString
plaintext17a = "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="

plaintext17b :: B.ByteString
plaintext17b = "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="

plaintext17c :: B.ByteString
plaintext17c = "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="

encrypter17 :: B.ByteString -> (B.ByteString, B.ByteString)
encrypter17 plaintext = (encrypted, iv)
                      where encrypted = S2.aesCbcEncrypt fixedKey iv padded
                            iv = fixedKey --Doesn't really matter
                            padded = S2.padTo16Multiple plaintext


decrypt17ValidPadding :: B.ByteString -> B.ByteString -> Bool
decrypt17ValidPadding ciphertext iv =
    let decrypted = S2.aesCbcDecrypt fixedKey iv ciphertext
    in hasValidPadding decrypted

hasValidPadding :: B.ByteString -> Bool
hasValidPadding bString
    | B.length bString > 16 = hasValidPadding (B.drop 16 bString)
    | otherwise = case S2.stripPadding bString of Nothing -> False
                                                  Just answer -> True

-- unclear from the site whether you pass IV to decrypt as I have
-- done, but i'm fairly sure that's the key to this.
-- the XOR with the IV is done after the decryption. We are going
--to fiddle with the last character. 

-- We know all the bytes of the IV. We can change what we tell the oracle
-- this is. After the AES decode, we are left with a partially Decoded text,
-- which we'll call D. then (D XOR IV) = P (plaintext). 
-- COnsider the last character. Whether the block is already padded or not,
-- there is  precisely one byte that we can pick as the final byte of IV
-- st d XOR iv = '/01'. The oracle oracle will tell us which, which tells
-- us d, which as we have the real IV, tells us the last byte of P. 

--if i only i actually understand monads, presumebly this is what i get free :(	
munge :: Either String B.ByteString -> Either String B.ByteString -> Either String B.ByteString
munge (Left a) (Left b) = Left (a ++ " AND ALSO " ++ b)
munge (Right a) (Left b) = Left ((B.unpack a) ++ " AND " ++ b)
munge (Right a) (Right b) = Right (B.append a b)

decryptCbcWPadding :: (B.ByteString, B.ByteString) -> Either String B.ByteString
decryptCbcWPadding (ciphertext, iv)
    | B.length ciphertext == 16 = firstPlainBlock
    | B.length ciphertext > 16 = munge firstPlainBlock plainRest
    where (firstCipherBlock, restCipherText) = B.splitAt 16 ciphertext
          firstPlainBlock = decryptOneBlock (firstCipherBlock, iv) B.empty
          plainRest = decryptCbcWPadding (restCipherText, firstCipherBlock)

decryptOneBlock :: (B.ByteString, B.ByteString) -> B.ByteString -> Either String B.ByteString
decryptOneBlock (cipherBlock, iv) dKnownSoFar
    | B.length dKnownSoFar == 16 = Right (bStringXor iv dKnownSoFar)
    | B.length cipherBlock < 16 = Left "ERROR: Block too short!"
    | otherwise =
    let nextChar = getOneCharD (cipherBlock, iv) dKnownSoFar
    in case nextChar of Right result -> decryptOneBlock (cipherBlock, iv) (B.append result dKnownSoFar)
                        Left x -> Left $ x ++ ". cipherBlock: " ++ (B.unpack cipherBlock) ++ ". iv: " ++ (B.unpack iv)

getOneCharD :: (B.ByteString, B.ByteString) -> B.ByteString -> Either String B.ByteString
getOneCharD (cipherBlock, iv) dKnownSoFar =
    let targetIndex = 16 - (B.length dKnownSoFar)
        before = B.take (targetIndex - 1) iv
        backTarget = B.tail $ S2.padderHelper (1 + B.length dKnownSoFar) -- we're trying to guess the first pad char, so knock one off front
        backPad = bStringXor backTarget dKnownSoFar
        guesses = ['\0'..'\255'] 
        --commented out list doesn't include 'h' ??!?!
        correctGuesses = filter (isTargetChar cipherBlock (before, backPad)) guesses
        correctGuess = parseCorrectGuesses correctGuesses iv
        --We know know which character, when xored with D, produced the correct pad
        -- so it follos that D = (correct pad) XOR (guess)
    in case correctGuess of Right asBString -> Right $ bStringXor (asBString) $ S2.padderHelper2 (1 + B.length dKnownSoFar)
                            Left err -> Left $ err ++ ". dKnownSoFar: " ++ (B.unpack dKnownSoFar) ++ ". before: " ++ (B.unpack before) ++ ". backPad: " ++ (B.unpack backPad)

isTargetChar :: B.ByteString -> (B.ByteString, B.ByteString) -> Char -> Bool
isTargetChar ciphertext (before, after) guessedTarget = 
    let attackIv = B.append before $ B.append (B.singleton guessedTarget) after
    in decrypt17ValidPadding ciphertext attackIv

--This is more complicated than I thought at firsst. If you ALREADY have a correct
--paddding, then there are TWO correct paddings including the original IV
parseCorrectGuesses :: [Char] -> B.ByteString -> Either String B.ByteString
parseCorrectGuesses [] _ = Left "ERROR: No solutions found."
parseCorrectGuesses [correctGuess] _ = Right (B.singleton correctGuess)
--We're looking for the block
parseCorrectGuesses correctGuesses iv =
    let realChar = B.last iv 
    in case L.elem realChar correctGuesses of True -> parseCorrectGuesses (L.delete realChar correctGuesses) iv
                                              False -> Left "ERROR: Too many solutions"

bStringXor :: B.ByteString -> B.ByteString -> B.ByteString
bStringXor a b = B.pack $ B.zipWith charXor a b 

charXor :: Char -> Char -> Char
charXor a b = C.chr $ Bits.xor (C.ord a) (C.ord b)