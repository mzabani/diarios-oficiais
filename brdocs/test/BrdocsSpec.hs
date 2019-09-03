module BrdocsSpec where

import Test.Hspec
import Test.QuickCheck
import Brdocs

spec :: Spec
spec = describe "Validação de CPFs e CNPJs" $ do
  it "CPF em formato bonitinho" $ 
    let cpfValido = "456.482.434-13" in printCpf <$> parseCpf cpfValido `shouldBe` Just cpfValido
  it "CPF em formato numérico" $
     printCpf <$> parseCpf "45648243413" `shouldBe` Just "456.482.434-13"
  it "CPF inválido" $
     parseCpf "45648243412" `shouldBe` Nothing
  it "CPF vazio" $
    parseCpf "" `shouldBe` Nothing
  it "CPF longo demais" $
    parseCpf "456.482.434-13 " `shouldBe` Nothing
  it "CNPJ em formato bonitinho" $ 
    let cnpjValido = "11.222.333/0001-81" in printCnpj <$> parseCnpj cnpjValido `shouldBe` Just cnpjValido

  it "CNPJ em formato numérico" $
    printCnpj <$> parseCnpj "11222333000181" `shouldBe` Just "11.222.333/0001-81"
  it "CNPJ inválido" $
    parseCnpj "11222333000180" `shouldBe` Nothing
  it "CNPJ vazio" $
    parseCnpj "" `shouldBe` Nothing
  it "CNPJ longo demais" $
    printCnpj <$> parseCnpj "11.222.333/0001-81 " `shouldBe` Nothing
