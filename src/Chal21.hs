{-# LANGUAGE OverloadedStrings #-}

module Chal21 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Int
import qualified Data.Bits as Bits
import Data.Sequence as Seq

--Implement the MT19937 Mersenne Twister RNG

--three stages:
--initialise
--run through 63(?) numbers
--Twist!

data MT = MT { array :: Seq Int32
             , pointer :: Int} deriving (Show)

initialiseMT :: Int32 -> MT
initialiseMT seed = initialiseMTHelper (Seq.singleton seed) 1

initialiseMTHelper :: Seq Int32 -> Int -> MT
initialiseMTHelper values 624 = MT { array = values, pointer = 624 }
initialiseMTHelper values len =
    let lastEntry = Seq.index values (len - 1)
        newEntry = (magicF * Bits.xor lastEntry shiftLast30R) + fromIntegral len
        magicF = 1812433253
        shiftLast30R = shiftR lastEntry 30
    in initialiseMTHelper (values |> newEntry) (len + 1)

getNextNumber :: MT -> (Int32, MT)
getNextNumber mT
    | pointer mT == 624 = getNextNumber (twist mT)
    | otherwise =
    let currentValue = Seq.index (array mT) (pointer mT)
        toReturn = nextValue currentValue
        newArray = Seq.update (pointer mT) toReturn (array mT)
    in (toReturn, MT { array = newArray, pointer = 1 + pointer mT})

nextValue :: Int32 -> Int32
nextValue currentValue =
    let firstXorOp a =  Bits.xor a $ shiftR a 11
        secondXorOp a = Bits.xor a $ Bits.shiftL a 7 Bits..&. 2636928640
        thirdXorOp a = Bits.xor a $ Bits.shiftL a 15 Bits..&. 4022730752
        fourthXorOp a = Bits.xor a $ shiftR a 18
        val1 = firstXorOp currentValue
        val2 = secondXorOp val1
        val3 = thirdXorOp val2
    in fourthXorOp val3

twist :: MT -> MT
twist mT = MT { array = newArray, pointer = 0}
         where newArray = twistHelper (array mT) 0

twistHelper :: Seq Int32 -> Int -> Seq Int32
twistHelper mTArray 624 = mTArray
twistHelper mTArray pointer =
    --let highBit = (Seq.index mTArray pointer) Bits..&. (0x80000000 :: Int32)
    let highBit = Seq.index mTArray pointer Bits..&. (Bits.bit 31 :: Int32)
        nextVal = Seq.index mTArray ((pointer + 1) `mod` 624)
        nextValLowerBits = nextVal Bits..&. (0x7fffffff :: Int32)
        intermediate = (highBit + nextValLowerBits)
        val397 = Seq.index mTArray ((pointer + 397) `mod` 624)
        intermediate2 = Bits.xor val397 (shiftR intermediate 1)
        newValue = case intermediate `mod` 2 of 0 -> intermediate2
                                                _ -> Bits.xor intermediate2 0x9908b0df
    in twistHelper (Seq.update pointer newValue mTArray) (pointer + 1)                                              

--inbuilt shiftR maintains sign, but we don't want
shiftR :: Int32 -> Int -> Int32
shiftR toShift howFar
    | isNegative = safeShift toShift howFar
    | otherwise = Bits.shiftR toShift howFar
    where isNegative = Bits.testBit toShift 31 -- testBit true if nth bit is set

safeShift toShift howFar =
    let asPos = Bits.clearBit toShift 31
        shifted = Bits.shiftR asPos howFar
        toSet = 31 - howFar
    in Bits.setBit shifted toSet
