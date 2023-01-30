import           Lib
import           Test.Hspec

main :: IO ()
main =
    hspec $ do
        testInvalidPersonIsNothing
        testPanicOnEmptyName
        testLeftNegativeAge
        testMkLongevity
        testLarnneire
