import           Lib
import           Test.Hspec

main :: IO ()
main =
    hspec $ do
        testFooDef
        testInvalidPersonIsNothing
        testPanicOnEmptyName
        testLeftNegativeAge
        testMkLongevity
        testLarnneire
        testLoyter
