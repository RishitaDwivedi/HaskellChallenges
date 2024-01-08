--These tests are for challenge 1 
--this test is for testing Absorb 
testC11 :: String 
testC11 | calcBBInteractions 10 [(1,1), (5,5), (10,10)] [(North, 5)] == expected = "Test C11 passed"
          | otherwise = "Test C11 failed" 
        where 
            expected = [((North,5),Absorb)]

--this tests for marking == Path 
testC12 :: String 
testC12 | calcBBInteractions  6 [(1,1), (5,5), (10,10)] [(West, 7)] == expected = "Test C12 passed"
       | otherwise = "Test C12 failed" 
        where 
            expected = [((West,7),Path (East,7))]
       
testC13 :: String 
testC13 | calcBBInteractions 8 [(2,4) , (6,2) , (4,7)] [(North , 3)] == expected = "Test C13 passed"
          | otherwise = "Test C13 failed" 
        where 
            expected = [((North , 3) , Path (East , 6))]

--This tests for markings Path and absorb 
testC14 :: String 
testC14 | calcBBInteractions 8 [ (2,3) , (7,3) , (4,6) , (7,8) ] [ (North,1) , (North,5) , (East, 7) , (South ,7) , (West,3) , (West,1) ] == expected = "Test C14 passed"
       | otherwise = "Test C14 failed" 
        where 
            expected = [((North,1),Path (West,2)),((North,5),Path (East,5)),((East,7),Path (East,4)),((South,7),Absorb),((West,1),Path (East,1)),((West,3),Absorb)]
       
testC15 :: String 
testC15 | calcBBInteractions 8 [ (2,3) , (7,3) , (4,6) , (7,8) ] [ (North,1) , (North,5) , (East, 7) , (South ,7) , (West,3) , (West,1) ] == expected = "Test C15 passed"
        | otherwise = "Test C15 failed" 
          where 
            expected = [((North,1),Path (West,2)),((North,5),Path (East,5)),((East,7),Path (East,4)),((South,7),Absorb),((West,1),Path (East,1)),((West,3),Absorb)]

testC16 :: String 
testC16 | calcBBInteractions 13 [(1,1), (5,5), (10,10)] [ (North,1) , (North,5) , (East, 7) , (South ,7) , (West,3) , (West,1) ] == expected = "Test C16 passed"
        | otherwise = "Test C16 failed" 
          where 
            expected = [((North,1),Absorb),((North,5),Absorb),((East,7),Path (West,7)),((South,7),Path (North,7)),((West,3),Path (East,3)),((West,1),Absorb)]

--this is a test for edge reflection special case 
testC17 :: String 
testC17 | calcBBInteractions 4 [(1,1)] [(West , 2)] == expected = "Test C17 passed "
        | otherwise = "Test C17 failed"
          where 
            expected = [((West,2),Reflect)]



--This test is for challenge 2 
testC21 :: String             
testC21 | solveBB 8 [((North,1),Path (West,2)), ((North,5),Path (East,5)), ((East,7),Path (East,4)), ((South,7),Absorb), ((West,1),Path (East,1)), ((West,3),Absorb) ] == expected = "Test C21 passed "
        | otherwise = "Test C21 failed"
          where 
            expected =  [ (2,3) , (7,3) , (4,6) , (7,8) ]            

--These tests are for challenge 3 
--this test is to make sure the brackets are in the right place 
testC31 :: String 
testC31 | prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) == expected = "Test C31 passed"
        | otherwise = "Test C31 failed"
          where 
            expected = "(\\x0->x0)\\x0->x0"

testC32 :: String 
testC32 | prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) == expected = "Test C32 passed"
        | otherwise = "Test C32 failed"
          where 
            expected = "\\x0->x0\\x0->x0"
                       
testC33 :: String 
testC33 | prettyPrint (LabApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))) == expected = "Test C33 passed"
        | otherwise = "Test C33 failed"
          where 
            expected = "x2 \\x0 -> \\x1 -> x0"

testC34 :: String 
testC34 | prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) == expected = "Test C34 passed"
        | otherwise = "Test C34 failed"
          where 
            expected = "\\x0->\\x0-> x0\\x0->\\x1->x0"

testC35 :: String 
testC35 | prettyPrint (LamApp (LamAbs 2 (LamVar 1)) (LamVar 3)) == expected = "Test C35 passed"
        | otherwise = "Test C35 failed"
          where 
            expected = "(\\x0->x1)x3"

testC36 :: String 
testC36 | prettyPrint (LamAbs 1 (LamApp (LamVar 2) (LamAbs 3 (LamVar 1)))) == expected = "Test C36 passed"
        | otherwise = "Test C36 failed"
          where 
            expected = "\\x0->x2\\x1->x0"          


--These tests are for challenge 4 
testC41 :: String 
testC41 | parseArith "1+2*3" == expected = "Test C41 passed"
        | otherwise = "Test C41 failed"
          where 
            expected = Just (Mul (Add (ArithNum 1) (ArithNum 2)) (ArithNum 3))


testC42 :: String 
testC42 | parseArith "1+2*3+4" == expected = "Test C42 passed"
        | otherwise = "Test C42 failed"
          where 
            expected = Just (Mul (Add (ArithNum 1) (ArithNum 2)) (Add (ArithNum 3) (ArithNum 4)))   

testC43 :: String 
testC43 | parseArith "1 + 2" == expected = "Test C43 passed"
        | otherwise = "Test C43 failed"
          where 
            expected = Just (Add (ArithNum 1) (ArithNum 2))                       

--this test is to check that (+n) is identified as SecApp 
testC44 :: String 
testC44 | parseArith "(+1) 2" == expected = "Test C44 passed"
        | otherwise = "Test C44 failed"
          where 
            expected = Just (SecApp (Section (ArithNum 1) (ArithNum 2)))   

testC45 :: String 
testC45 | parseArith "2 (+1)" == expected = "Test C45 passed"
        | otherwise = "Test C45 failed"
          where 
            expected = Nothing

testC46 :: String 
testC46 | parseArith "(+1) (+2) 3" == expected = "Test C46 passed"
        | otherwise = "Test C46 failed"
          where 
            expected = Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (ArithNum 3)))

--these tests are for challenge 5 
testC51 :: String 
testC51 | churchEnc (ArithNum 5) == expected = "Test C51 passed"
        | otherwise = "Test C51 failed"
          where 
            expected = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))

--this test checks to make sure churchEnc works for Add 
testC52 :: String 
testC52 |churchEnc (Add (ArithNum 5) (ArithNum 3)) == expected = "Test C52 passed"
        | otherwise = "Test C52 failed"
          where 
            expected = LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))    
--this test checks to make sure churchEnc works for Mul
testC53 :: String 
testC53 | churchEnc (Mul (ArithNum 5) (ArithNum 3)) == expected = "Test C53 passed"
        | otherwise = "Test C53 failed"
          where 
            expected = LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))
--this test checks to make sure churchEnc works for SecApp
testC54 :: String 
testC54 | churchEnc (SecApp (ArithNum 5) (ArithNum 3)) == expected = "Test C54 passed"
        | otherwise = "Test C54 failed"
          where 
            expected = LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))

testC55 :: String 
testC55 | churchEnc (ArithNum 2) == expected = "Test C55 passed"
        | otherwise = "Test C55 failed"
          where 
            expected = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))

testC56 :: String 
testC56 |churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 1)) == expected = "Test C56 passed"
        | otherwise = "Test C56 failed"
          where 
            expected = LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))

--these tests are for challenge 6 
--this is to test the helper function innerArithRedn1
testC61 :: String 
testC61 | innerArithRedn1(Add (ArithNum 2) (ArithNum 3)) == expected = "Test C61 passed"
        | otherwise = "Test C61 failed"
          where 
            expected = Just (ArithNum 5)
--this test is to test helper function innerRedn1
testC62 :: String 
testC62 | innerRedn1 (LamApp (LamAbs 1 (ArithNum 2)) (ArithNum 3)) == expected = "Test C62 passed"
        | otherwise = "Test C62 failed"
          where 
            expected =  (Just (ArithNum 2))

--this test is to test the compareArithLam function 
testC63 :: String 
testC63 | compareArithLam (Add (ArithNum 2) (ArithNum 3)) == expected = "Test C63 passed"
        | otherwise = "Test C63 failed"
          where 
            expected = (1,6)
--this function tests helper function myFromJust 
testC64 :: String 
testC64 | myFromJust(Just (ArithNum 2)) == expected = "Test C64 passed"
        | otherwise = "Test C64 failed"
          where 
            expected =  (ArithNum 2)
