module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, genCode, genDot, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.DblPendulum.Choices (codeChoices, codeSpecs)
import Drasil.DblPendulum.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX]) "DblPendulum_SRS") srs printSetting
  genCode codeChoices codeSpecs
  genDot fullSI
  genLog fullSI printSetting
