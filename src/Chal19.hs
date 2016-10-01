{-# LANGUAGE OverloadedStrings #-}

module Chal19 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
--import qualified Data.Serialize as Serialize
import qualified Set1 as S1
import qualified Set2 as S2
import qualified Chal17 as C17
import qualified Chal18 as C18
import qualified Data.ByteString.Base64 as B.Base64

fixedKey :: B.ByteString
fixedKey = S2.aesEcbEncrypt "KENDRICK LAMAR!!" "KENDRICK LAMAR!!"
                             
fixedCtrEncrypt text = C18.aesCtrCrypt fixedKey 0 text

cipherTexts :: [B.ByteString]
cipherTexts = map fixedCtrEncrypt plainTexts
            where plainTexts = map B.Base64.decodeLenient base64Texts

--items appearing more than once in a list
duplicates :: Eq a => [a] -> [a]
duplicates list = list L.\\ (L.nub list)

--Chal19> duplicates (map (B.take 3) cipherTexts )
--["\198J;","\192\CANs","\206\EOT7","\219\STX:","\220\ENQs","\219\STX:","\199\SIs","\219\ENQs","\198\EOTs","\199\SI\DEL","\206J'"]
-- \219\STX: appears at least three times, perhaps it is "the"

tryFirstThreeCharsGuess :: [B.ByteString] -> String -> [B.ByteString]
tryFirstThreeCharsGuess cipherTexts guess = map guessedKeyXor firstThreeChars
                                          where firstThreeChars = map (B.take 3) cipherTexts
                                                guessedKeyXor = C17.bStringXor guessedKey
                                                guessedKey = C17.bStringXor (B.pack guess) "\219\STX:"
--Hmm this isn't really working                                               

--Guess the keystream byte
decodeChar :: [Maybe Char] -> Char
decodeChar cipherChars = L.maximumBy (charScoreComp cipherChars) ['\0'..'\255']

charScoreComp cipherChars a b
    | charScore cipherChars a > charScore cipherChars b = GT 
    | otherwise = LT

charScore [] a = 0
charScore (possH:t) a
    | isGoodChar = 1 + charScore t a 
    | otherwise = charScore t a
    where isGoodChar = case possH of Just h -> elem (C17.charXor a h) (['a'..'z'] ++ ['A'..'Z'] ++ [' ', '\'', ',', '.'])
                                     Nothing -> False
cipherCharsN :: Int -> [Maybe Char]
cipherCharsN n = map (lenientIndex n) cipherTexts 

lenientIndex index bString
    | index >= B.length bString = Nothing
    | otherwise = Just $ B.index bString index

cipherChars :: [[Maybe Char]]
cipherChars = map cipherCharsN [0..38] --maximum $ map B.length cipherTexts 

solveKeystream = B.pack keySteamChars
               where keySteamChars = map decodeChar cipherChars

solve = map (C17.bStringXor (solveKeystream)) cipherTexts

--So...
--Chal19> solve
--["i'hfve met them at ckose oa yad","chmnng with vivid fades","fuoj counter or desk'among'goed","engoteenth-century hhuses.","i'hfve passed with a'nod oa ihx oiyO","ou wolite meaninglest wordt,","ou oave lingered awhnle anc natd","phlnte meaningless whrds,","aid'thought before I'had dhnx","oa f mocking tale or'a gibb","th wlease a companioi","auornd the fire at toe clue,","bbiig certain that toey anc T","brt'lived where motlby is poon'","akl'changed, changed'utterky'","a'tbrrible beauty is'born.","toas woman's days weue speit","ii ngnorant good wilk,","hbr'nights in argumeit","uitnl her voice grew'shrilk.","woas voice more swees than'hxrn","woei young and beautnful,","soe'rode to harriers8","toit man had kept a tchool","aid'rode our winged oorse.","toit other his helpeu and artesd","wfs'coming into his aorce;","hb jight have won faje in shx xnc ","sh tensitive his natrre sebmxd1","sh caring and sweet ois thhuzhi.","toit other man I had'dreambd","a'duunken, vain-glornous lhui.","hb oad done most bitser wrhnz","th tome who are near'my hefri,","ybt'I number him in she soig&","hb,'too, has resignec his waot","ii she casual comedy<","hb,'too, has been chfnged nn=hts'xmY,.","tuaisformed utterly:","a'tbrrible beauty is'born."]
--Not bad, but not exact. Close enough that i think it would be fairly easy to work out by hand from here...
-- as the next challenge is to automate, i won't bother for the time being. It's Yeats' Easter, 1916.  


base64Texts :: [B.ByteString]
base64Texts = ["SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ==",
               "Q29taW5nIHdpdGggdml2aWQgZmFjZXM=",
               "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ==",
               "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4=",
               "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk",
               "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==",
               "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ=",
               "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==",
               "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU=",
               "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl",
               "VG8gcGxlYXNlIGEgY29tcGFuaW9u",
               "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA==",
               "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk=",
               "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg==",
               "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo=",
               "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4=",
               "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA==",
               "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA==",
               "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA==",
               "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg==",
               "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw==",
               "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA==",
               "U2hlIHJvZGUgdG8gaGFycmllcnM/",
               "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w=",
               "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4=",
               "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ=",
               "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs=",
               "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA==",
               "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA==",
               "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4=",
               "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA==",
               "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu",
               "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc=",
               "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs",
               "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs=",
               "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0",
               "SW4gdGhlIGNhc3VhbCBjb21lZHk7",
               "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw=",
               "VHJhbnNmb3JtZWQgdXR0ZXJseTo=",
               "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="]