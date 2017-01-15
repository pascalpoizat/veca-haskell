-- tests
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
import           RoverTests
import           Test.Tasty
import           TreeTests

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" [treeTests,roverTests]
