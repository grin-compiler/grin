{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module PrimOpsSpec where

import Test.Hspec

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Foreign.C.String
import Foreign.Marshal.Alloc
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic


C.include "<stdio.h>"
C.include "../prim_ops.c"

spec :: Spec
spec = do
  let randomString = listOf $ elements ['a' .. 'z']
  let randomNonEmptyString = listOf1 $ elements ['a' .. 'z']
  describe "_prim_string_len" $ do
    let primStringLen str = do
          cstr <- newCString str
          l <- [C.block|long {
                    struct string* s1 = create_string_copy($(char* cstr));
                    return _prim_string_len(s1);
                }|]
          l `shouldBe` (fromIntegral $ length str)

    it "works for empty string"       $ primStringLen ""
    it "works for one element string" $ primStringLen "a"
    it "works for a longer string"    $ primStringLen "1234567890"
    it "works for a random string"    $ monadicIO $ do
      str <- pick randomString
      run $ primStringLen str

  describe "_prim_string_concat" $ do
    let primSringConcat str1 str2 = do
          cstr1 <- newCString str1
          cstr2 <- newCString str2
          let n = length str1 + length str2 + 1
          allocaBytes n $ \buffer -> do
            [C.block|void{
                struct string* s1 = create_string_copy($(char* cstr1));
                struct string* s2 = create_string_copy($(char* cstr2));
                struct string* s3 = _prim_string_concat(s1,s2);
                cstring($(char* buffer), s3);
              }|]
            res <- peekCString buffer
            res `shouldBe` (str1 ++ str2)
          pure ()
    it "works for empty strings" $ primSringConcat "" ""
    it "works for empty string left" $ primSringConcat "" "a"
    it "works for empty string right" $ primSringConcat "a" ""
    it "works for one length strings" $ primSringConcat "a" "a"
    it "works for longer strings" $ primSringConcat "abc" "def"
    it "works for random strings" $ monadicIO $ do
      str1 <- pick randomString
      str2 <- pick randomString
      run $ primSringConcat str1 str2

  describe "_prim_string_reverse" $ do
    let primStringReverse str = do
          cstr <- newCString str
          let n = length str + 1
          allocaBytes n $ \buffer -> do
            [C.block|void{
                struct string* s1 = create_string_copy($(char* cstr));
                struct string* s2 = _prim_string_reverse(s1);
                cstring($(char* buffer), s2);
            }|]
            res <- peekCString buffer
            res `shouldBe` (reverse str)
          pure ()
    it "works for empty string" $ primStringReverse ""
    it "works for one length string" $ primStringReverse "a"
    it "works for a longer string" $ primStringReverse "abcdefg"
    it "works for a random string" $ monadicIO $ do
      str <- pick randomString
      run $ primStringReverse str

  describe "_prim_string_eq" $ do
    let primStringEq str1 str2 = do
          cstr1 <- newCString str1
          cstr2 <- newCString str2
          r <- [C.block|long{
                  struct string* s1 = create_string_copy($(char* cstr1));
                  struct string* s2 = create_string_copy($(char* cstr2));
                  return _prim_string_eq(s1, s2);
                }|]
          r `shouldBe` (if str1 == str2 then 1 else 0)
    it "works for empty strings" $ primStringEq "" ""
    it "works for empty string left" $ primStringEq "" "a"
    it "works for empty string right" $ primStringEq "a" ""
    it "works for same one length strings" $ primStringEq "a" "a"
    it "works for same strings" $ primStringEq "aa" "aa"
    it "works for different strings" $ primStringEq "abcd" "abce"
    it "works for random strings" $ monadicIO $ do
      str1 <- pick randomString
      str2 <- pick randomString
      run $ primStringEq str1 str2

  describe "_prim_string_head" $ do
    let primStringHead str = do
          cstr <- newCString str
          r <- [C.block|long{
                  struct string* s1 = create_string_copy($(char* cstr));
                  return _prim_string_head(s1);
                }|]
          r `shouldBe` (fromIntegral $ fromEnum $ head str)
    it "works for one length string" $ primStringHead "a"
    it "works for a longer string" $ primStringHead "bfmdh"
    it "works for random non-empty strings" $ monadicIO $ do
      str1 <- pick randomNonEmptyString
      run $ primStringHead str1

  describe "_prim_string_tail" $ do
    let primStringTail str = do
          cstr <- newCString str
          let n = length str
          allocaBytes n $ \buffer -> do
            [C.block|void{
              struct string* s1 = create_string_copy($(char* cstr));
              struct string* s2 = _prim_string_tail(s1);
              cstring($(char* buffer), s2);
            }|]
            res <- peekCString buffer
            res `shouldBe` (tail str)
          pure ()
    it "works for one element string" $ primStringTail "a"
    it "works for a longer string" $ primStringTail "lksdjfoa"
    it "works for a random non-empty strings" $ monadicIO $ do
      str1 <- pick randomNonEmptyString
      run $ primStringTail str1

  describe "_prim_string_cons" $ do
    let primStringCons c0 str = do
          cstr <- newCString (str :: String)
          let n = length str + 1
          let c = C.CChar $ fromIntegral $ fromEnum c0
          allocaBytes n $ \buffer -> do
            [C.block|void{
              struct string* s1 = create_string_copy($(char* cstr));
              struct string* s2 = _prim_string_cons($(char c), s1);
              cstring($(char* buffer), s2);
            }|]
            res <- peekCString buffer
            res `shouldBe` (c0:str)
          pure ()
    it "works for empty string" $ primStringCons 'a' ""
    it "works for a one length string" $ primStringCons 'a' "b"
    it "works for a longer string" $ primStringCons 'a' "sdflkje"
    it "works for random string" $ monadicIO $ do
      c <- pick $ elements ['a' .. 'z']
      str <- pick $ randomString
      run $ primStringCons c str

  describe "_prim_string_lt" $ do
    let primStringLt str1 str2 = do
          cstr1 <- newCString str1
          cstr2 <- newCString str2
          r <- [C.block|long{
                  struct string* s1 = create_string_copy($(char* cstr1));
                  struct string* s2 = create_string_copy($(char* cstr2));
                  return _prim_string_lt(s1, s2);
                }|]
          r `shouldBe` (if str1 < str2 then 1 else 0)
    it "works for random strings" $ monadicIO $ do
      str1 <- pick randomString
      str2 <- pick randomString
      run $ primStringLt str1 str2

  describe "_prim_int_str" $ do
    let primIntStr i0 = do
          let i = C.CLong i0
          allocaBytes 256 $ \buffer -> do
            [C.block|void{
              struct string* s1 = _prim_int_str($(long i));
              cstring($(char* buffer), s1);
            }|]
            res <- peekCString buffer
            res `shouldBe` (show i)
          pure ()
    it "works for random integers" $ monadicIO $ do
      i <- pick arbitrary
      run $ primIntStr i

  describe "_prim_float_string" $ do
    let primIntStr f0 = do
          let f = C.CFloat f0
          allocaBytes 256 $ \buffer -> do
            [C.block|void{
              struct string* s1 = _prim_float_string($(float f));
              cstring($(char* buffer), s1);
            }|]
            res <- peekCString buffer
            res `shouldBe` (show f0)
          pure ()
    xit "works for random float" $ monadicIO $ do
      f <- pick arbitrary
      run $ primIntStr f

  describe "_prim_str_int" $ do
    let primStrInt i = do
          cstr <- newCString (show i)
          r <- [C.block|long{
                  struct string* s1 = create_string_copy($(char* cstr));
                  return _prim_str_int(s1);
                }|]
          r `shouldBe` i
    it "works for random integers" $ monadicIO $ do
      i <- pick arbitrary
      run $ primStrInt i

  describe "_prim_int_float" $ do
    let primIntFloat i0 = do
          let i = C.CLong i0
          r <- [C.block|float{
                  return _prim_int_float($(long i));
                }|]
          r `shouldBe` (fromIntegral i)
    it "works for random integers" $ monadicIO $ do
      i <- pick arbitrary
      run $ primIntFloat i

  describe "_prim_char_int" $ do
    let primCharInt c0 = do
          let c = C.CChar $ fromIntegral $ fromEnum c0
          r <- [C.block|long{
                  return _prim_char_int($(char c));
                }|]
          r `shouldBe` (fromIntegral $ fromEnum c0)
    it "works for random chars" $ monadicIO $ do
      c <- pick $ elements ['a' .. 'z']
      run $ primCharInt c
