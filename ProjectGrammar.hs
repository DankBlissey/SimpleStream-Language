{-# OPTIONS_GHC -w #-}
module ProjectGrammar where 
import ProjectLexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,350) ([32768,0,0,0,8,0,0,16414,25600,386,0,0,0,7680,65,33380,1,0,0,0,0,0,1024,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,7680,64,33380,1,1024,0,0,16384,0,0,0,4,0,24576,32842,8195,3,0,0,16,1024,64,0,16384,0,0,24576,32842,8195,3,1190,56,50,15360,63,28,0,0,0,0,0,0,0,0,0,4,19040,896,800,42496,14340,12800,0,0,0,0,0,0,0,0,0,0,42496,14340,12800,0,0,16384,0,0,0,16,19040,896,800,42496,14340,12800,24576,32842,8195,3,2048,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,16316,7168,0,64448,49155,1,48128,63,28,0,0,0,24576,32842,8195,3,62400,3,0,48128,63,28,0,0,0,24576,32842,8195,3,1190,56,50,19040,896,800,42496,14340,12800,24576,32842,8195,3,1190,56,50,19040,896,800,42496,14340,12800,24576,32842,8195,3,1190,56,50,19040,896,800,42496,14340,12800,24576,32842,8195,3,1190,56,50,15360,63,28,49152,1011,448,0,0,0,0,62400,3,0,15360,63,16,49152,1011,256,0,60,0,0,960,0,0,15360,0,0,49152,3,0,0,60,0,0,960,0,0,12288,0,0,0,3,0,0,48,0,0,768,0,0,15360,63,28,0,0,0,0,16188,7168,57344,1025,9792,24,16414,25600,386,480,16388,6182,24576,32842,8195,3,62400,49155,1,16414,31744,386,480,16388,6190,7680,64,33508,1,0,0,0,0,0,0,0,0,0,7680,64,33380,57345,1025,9920,24,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_projectParser","Program","Actions","Action","Begin","Exp","Type","BOOL","INT","STRING","ARRAY","int","string","'{'","'}'","IN","'+'","'-'","'*'","'/'","'('","')'","'<'","'>'","'='","'!='","'<='","'>='","'=*'","intArray","TRUE","FALSE","IF","ENDIF","ELSE","WHILE","FOR","ENDLOOP","':'","RETURN","AND","OR","XOR","NOT","AT","ADD","var","LENGTH","';'","%eof"]
        bit_start = st Prelude.* 52
        bit_end = (st Prelude.+ 1) Prelude.* 52
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..51]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (16) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (10) = happyShift action_8
action_2 (11) = happyShift action_9
action_2 (12) = happyShift action_10
action_2 (13) = happyShift action_11
action_2 (23) = happyShift action_12
action_2 (35) = happyShift action_13
action_2 (38) = happyShift action_14
action_2 (39) = happyShift action_15
action_2 (42) = happyShift action_16
action_2 (48) = happyShift action_17
action_2 (49) = happyShift action_18
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (52) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (10) = happyShift action_8
action_4 (11) = happyShift action_9
action_4 (12) = happyShift action_10
action_4 (13) = happyShift action_11
action_4 (17) = happyShift action_41
action_4 (23) = happyShift action_12
action_4 (35) = happyShift action_13
action_4 (38) = happyShift action_14
action_4 (39) = happyShift action_15
action_4 (42) = happyShift action_16
action_4 (48) = happyShift action_17
action_4 (49) = happyShift action_18
action_4 (6) = happyGoto action_40
action_4 (7) = happyGoto action_6
action_4 (9) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_3

action_6 (51) = happyShift action_39
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (41) = happyShift action_38
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_40

action_9 _ = happyReduce_41

action_10 _ = happyReduce_42

action_11 _ = happyReduce_43

action_12 (10) = happyShift action_8
action_12 (11) = happyShift action_9
action_12 (12) = happyShift action_10
action_12 (13) = happyShift action_11
action_12 (23) = happyShift action_12
action_12 (35) = happyShift action_13
action_12 (38) = happyShift action_14
action_12 (39) = happyShift action_15
action_12 (42) = happyShift action_16
action_12 (48) = happyShift action_17
action_12 (49) = happyShift action_18
action_12 (7) = happyGoto action_37
action_12 (9) = happyGoto action_7
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (23) = happyShift action_36
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (23) = happyShift action_35
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (23) = happyShift action_34
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (14) = happyShift action_23
action_16 (15) = happyShift action_24
action_16 (18) = happyShift action_25
action_16 (20) = happyShift action_26
action_16 (23) = happyShift action_27
action_16 (32) = happyShift action_28
action_16 (33) = happyShift action_29
action_16 (34) = happyShift action_30
action_16 (46) = happyShift action_31
action_16 (49) = happyShift action_32
action_16 (50) = happyShift action_33
action_16 (8) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (49) = happyShift action_21
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_19
action_18 (31) = happyShift action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (19) = happyShift action_68
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (14) = happyShift action_23
action_20 (15) = happyShift action_24
action_20 (18) = happyShift action_25
action_20 (20) = happyShift action_26
action_20 (23) = happyShift action_27
action_20 (32) = happyShift action_28
action_20 (33) = happyShift action_29
action_20 (34) = happyShift action_30
action_20 (46) = happyShift action_31
action_20 (49) = happyShift action_32
action_20 (50) = happyShift action_33
action_20 (8) = happyGoto action_67
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (14) = happyShift action_23
action_21 (15) = happyShift action_24
action_21 (18) = happyShift action_25
action_21 (20) = happyShift action_26
action_21 (23) = happyShift action_27
action_21 (32) = happyShift action_28
action_21 (33) = happyShift action_29
action_21 (34) = happyShift action_30
action_21 (46) = happyShift action_31
action_21 (49) = happyShift action_32
action_21 (50) = happyShift action_33
action_21 (8) = happyGoto action_66
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (19) = happyShift action_53
action_22 (20) = happyShift action_54
action_22 (21) = happyShift action_55
action_22 (22) = happyShift action_56
action_22 (25) = happyShift action_57
action_22 (26) = happyShift action_58
action_22 (27) = happyShift action_59
action_22 (28) = happyShift action_60
action_22 (29) = happyShift action_61
action_22 (30) = happyShift action_62
action_22 (43) = happyShift action_63
action_22 (44) = happyShift action_64
action_22 (45) = happyShift action_65
action_22 _ = happyReduce_9

action_23 _ = happyReduce_15

action_24 _ = happyReduce_16

action_25 (47) = happyShift action_52
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_23
action_26 (15) = happyShift action_24
action_26 (18) = happyShift action_25
action_26 (20) = happyShift action_26
action_26 (23) = happyShift action_27
action_26 (32) = happyShift action_28
action_26 (33) = happyShift action_29
action_26 (34) = happyShift action_30
action_26 (46) = happyShift action_31
action_26 (49) = happyShift action_32
action_26 (50) = happyShift action_33
action_26 (8) = happyGoto action_51
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (14) = happyShift action_23
action_27 (15) = happyShift action_24
action_27 (18) = happyShift action_25
action_27 (20) = happyShift action_26
action_27 (23) = happyShift action_27
action_27 (32) = happyShift action_28
action_27 (33) = happyShift action_29
action_27 (34) = happyShift action_30
action_27 (46) = happyShift action_31
action_27 (49) = happyShift action_32
action_27 (50) = happyShift action_33
action_27 (8) = happyGoto action_50
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_17

action_29 _ = happyReduce_20

action_30 _ = happyReduce_21

action_31 (14) = happyShift action_23
action_31 (15) = happyShift action_24
action_31 (18) = happyShift action_25
action_31 (20) = happyShift action_26
action_31 (23) = happyShift action_27
action_31 (32) = happyShift action_28
action_31 (33) = happyShift action_29
action_31 (34) = happyShift action_30
action_31 (46) = happyShift action_31
action_31 (49) = happyShift action_32
action_31 (50) = happyShift action_33
action_31 (8) = happyGoto action_49
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (47) = happyShift action_48
action_32 _ = happyReduce_18

action_33 (49) = happyShift action_47
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (14) = happyShift action_23
action_34 (15) = happyShift action_24
action_34 (18) = happyShift action_25
action_34 (20) = happyShift action_26
action_34 (23) = happyShift action_27
action_34 (32) = happyShift action_28
action_34 (33) = happyShift action_29
action_34 (34) = happyShift action_30
action_34 (46) = happyShift action_31
action_34 (49) = happyShift action_32
action_34 (50) = happyShift action_33
action_34 (8) = happyGoto action_46
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (14) = happyShift action_23
action_35 (15) = happyShift action_24
action_35 (18) = happyShift action_25
action_35 (20) = happyShift action_26
action_35 (23) = happyShift action_27
action_35 (32) = happyShift action_28
action_35 (33) = happyShift action_29
action_35 (34) = happyShift action_30
action_35 (46) = happyShift action_31
action_35 (49) = happyShift action_32
action_35 (50) = happyShift action_33
action_35 (8) = happyGoto action_45
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (14) = happyShift action_23
action_36 (15) = happyShift action_24
action_36 (18) = happyShift action_25
action_36 (20) = happyShift action_26
action_36 (23) = happyShift action_27
action_36 (32) = happyShift action_28
action_36 (33) = happyShift action_29
action_36 (34) = happyShift action_30
action_36 (46) = happyShift action_31
action_36 (49) = happyShift action_32
action_36 (50) = happyShift action_33
action_36 (8) = happyGoto action_44
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (24) = happyShift action_43
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (49) = happyShift action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_4

action_40 _ = happyReduce_2

action_41 _ = happyReduce_1

action_42 (31) = happyShift action_88
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_13

action_44 (19) = happyShift action_53
action_44 (20) = happyShift action_54
action_44 (21) = happyShift action_55
action_44 (22) = happyShift action_56
action_44 (24) = happyShift action_87
action_44 (25) = happyShift action_57
action_44 (26) = happyShift action_58
action_44 (27) = happyShift action_59
action_44 (28) = happyShift action_60
action_44 (29) = happyShift action_61
action_44 (30) = happyShift action_62
action_44 (43) = happyShift action_63
action_44 (44) = happyShift action_64
action_44 (45) = happyShift action_65
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (19) = happyShift action_53
action_45 (20) = happyShift action_54
action_45 (21) = happyShift action_55
action_45 (22) = happyShift action_56
action_45 (24) = happyShift action_86
action_45 (25) = happyShift action_57
action_45 (26) = happyShift action_58
action_45 (27) = happyShift action_59
action_45 (28) = happyShift action_60
action_45 (29) = happyShift action_61
action_45 (30) = happyShift action_62
action_45 (43) = happyShift action_63
action_45 (44) = happyShift action_64
action_45 (45) = happyShift action_65
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (19) = happyShift action_53
action_46 (20) = happyShift action_54
action_46 (21) = happyShift action_55
action_46 (22) = happyShift action_56
action_46 (24) = happyShift action_85
action_46 (25) = happyShift action_57
action_46 (26) = happyShift action_58
action_46 (27) = happyShift action_59
action_46 (28) = happyShift action_60
action_46 (29) = happyShift action_61
action_46 (30) = happyShift action_62
action_46 (43) = happyShift action_63
action_46 (44) = happyShift action_64
action_46 (45) = happyShift action_65
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_38

action_48 (14) = happyShift action_23
action_48 (15) = happyShift action_24
action_48 (18) = happyShift action_25
action_48 (20) = happyShift action_26
action_48 (23) = happyShift action_27
action_48 (32) = happyShift action_28
action_48 (33) = happyShift action_29
action_48 (34) = happyShift action_30
action_48 (46) = happyShift action_31
action_48 (49) = happyShift action_32
action_48 (50) = happyShift action_33
action_48 (8) = happyGoto action_84
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (19) = happyShift action_53
action_49 (20) = happyShift action_54
action_49 (21) = happyShift action_55
action_49 (22) = happyShift action_56
action_49 (25) = happyShift action_57
action_49 (26) = happyShift action_58
action_49 (27) = happyShift action_59
action_49 (28) = happyShift action_60
action_49 (29) = happyShift action_61
action_49 (30) = happyShift action_62
action_49 _ = happyReduce_32

action_50 (19) = happyShift action_53
action_50 (20) = happyShift action_54
action_50 (21) = happyShift action_55
action_50 (22) = happyShift action_56
action_50 (24) = happyShift action_83
action_50 (25) = happyShift action_57
action_50 (26) = happyShift action_58
action_50 (27) = happyShift action_59
action_50 (28) = happyShift action_60
action_50 (29) = happyShift action_61
action_50 (30) = happyShift action_62
action_50 (43) = happyShift action_63
action_50 (44) = happyShift action_64
action_50 (45) = happyShift action_65
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_19

action_52 (14) = happyShift action_23
action_52 (15) = happyShift action_24
action_52 (18) = happyShift action_25
action_52 (20) = happyShift action_26
action_52 (23) = happyShift action_27
action_52 (32) = happyShift action_28
action_52 (33) = happyShift action_29
action_52 (34) = happyShift action_30
action_52 (46) = happyShift action_31
action_52 (49) = happyShift action_32
action_52 (50) = happyShift action_33
action_52 (8) = happyGoto action_82
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (14) = happyShift action_23
action_53 (15) = happyShift action_24
action_53 (18) = happyShift action_25
action_53 (20) = happyShift action_26
action_53 (23) = happyShift action_27
action_53 (32) = happyShift action_28
action_53 (33) = happyShift action_29
action_53 (34) = happyShift action_30
action_53 (46) = happyShift action_31
action_53 (49) = happyShift action_32
action_53 (50) = happyShift action_33
action_53 (8) = happyGoto action_81
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (14) = happyShift action_23
action_54 (15) = happyShift action_24
action_54 (18) = happyShift action_25
action_54 (20) = happyShift action_26
action_54 (23) = happyShift action_27
action_54 (32) = happyShift action_28
action_54 (33) = happyShift action_29
action_54 (34) = happyShift action_30
action_54 (46) = happyShift action_31
action_54 (49) = happyShift action_32
action_54 (50) = happyShift action_33
action_54 (8) = happyGoto action_80
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (14) = happyShift action_23
action_55 (15) = happyShift action_24
action_55 (18) = happyShift action_25
action_55 (20) = happyShift action_26
action_55 (23) = happyShift action_27
action_55 (32) = happyShift action_28
action_55 (33) = happyShift action_29
action_55 (34) = happyShift action_30
action_55 (46) = happyShift action_31
action_55 (49) = happyShift action_32
action_55 (50) = happyShift action_33
action_55 (8) = happyGoto action_79
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (14) = happyShift action_23
action_56 (15) = happyShift action_24
action_56 (18) = happyShift action_25
action_56 (20) = happyShift action_26
action_56 (23) = happyShift action_27
action_56 (32) = happyShift action_28
action_56 (33) = happyShift action_29
action_56 (34) = happyShift action_30
action_56 (46) = happyShift action_31
action_56 (49) = happyShift action_32
action_56 (50) = happyShift action_33
action_56 (8) = happyGoto action_78
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (14) = happyShift action_23
action_57 (15) = happyShift action_24
action_57 (18) = happyShift action_25
action_57 (20) = happyShift action_26
action_57 (23) = happyShift action_27
action_57 (32) = happyShift action_28
action_57 (33) = happyShift action_29
action_57 (34) = happyShift action_30
action_57 (46) = happyShift action_31
action_57 (49) = happyShift action_32
action_57 (50) = happyShift action_33
action_57 (8) = happyGoto action_77
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (14) = happyShift action_23
action_58 (15) = happyShift action_24
action_58 (18) = happyShift action_25
action_58 (20) = happyShift action_26
action_58 (23) = happyShift action_27
action_58 (32) = happyShift action_28
action_58 (33) = happyShift action_29
action_58 (34) = happyShift action_30
action_58 (46) = happyShift action_31
action_58 (49) = happyShift action_32
action_58 (50) = happyShift action_33
action_58 (8) = happyGoto action_76
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (14) = happyShift action_23
action_59 (15) = happyShift action_24
action_59 (18) = happyShift action_25
action_59 (20) = happyShift action_26
action_59 (23) = happyShift action_27
action_59 (32) = happyShift action_28
action_59 (33) = happyShift action_29
action_59 (34) = happyShift action_30
action_59 (46) = happyShift action_31
action_59 (49) = happyShift action_32
action_59 (50) = happyShift action_33
action_59 (8) = happyGoto action_75
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (14) = happyShift action_23
action_60 (15) = happyShift action_24
action_60 (18) = happyShift action_25
action_60 (20) = happyShift action_26
action_60 (23) = happyShift action_27
action_60 (32) = happyShift action_28
action_60 (33) = happyShift action_29
action_60 (34) = happyShift action_30
action_60 (46) = happyShift action_31
action_60 (49) = happyShift action_32
action_60 (50) = happyShift action_33
action_60 (8) = happyGoto action_74
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (14) = happyShift action_23
action_61 (15) = happyShift action_24
action_61 (18) = happyShift action_25
action_61 (20) = happyShift action_26
action_61 (23) = happyShift action_27
action_61 (32) = happyShift action_28
action_61 (33) = happyShift action_29
action_61 (34) = happyShift action_30
action_61 (46) = happyShift action_31
action_61 (49) = happyShift action_32
action_61 (50) = happyShift action_33
action_61 (8) = happyGoto action_73
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (14) = happyShift action_23
action_62 (15) = happyShift action_24
action_62 (18) = happyShift action_25
action_62 (20) = happyShift action_26
action_62 (23) = happyShift action_27
action_62 (32) = happyShift action_28
action_62 (33) = happyShift action_29
action_62 (34) = happyShift action_30
action_62 (46) = happyShift action_31
action_62 (49) = happyShift action_32
action_62 (50) = happyShift action_33
action_62 (8) = happyGoto action_72
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (14) = happyShift action_23
action_63 (15) = happyShift action_24
action_63 (18) = happyShift action_25
action_63 (20) = happyShift action_26
action_63 (23) = happyShift action_27
action_63 (32) = happyShift action_28
action_63 (33) = happyShift action_29
action_63 (34) = happyShift action_30
action_63 (46) = happyShift action_31
action_63 (49) = happyShift action_32
action_63 (50) = happyShift action_33
action_63 (8) = happyGoto action_71
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (14) = happyShift action_23
action_64 (15) = happyShift action_24
action_64 (18) = happyShift action_25
action_64 (20) = happyShift action_26
action_64 (23) = happyShift action_27
action_64 (32) = happyShift action_28
action_64 (33) = happyShift action_29
action_64 (34) = happyShift action_30
action_64 (46) = happyShift action_31
action_64 (49) = happyShift action_32
action_64 (50) = happyShift action_33
action_64 (8) = happyGoto action_70
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (14) = happyShift action_23
action_65 (15) = happyShift action_24
action_65 (18) = happyShift action_25
action_65 (20) = happyShift action_26
action_65 (23) = happyShift action_27
action_65 (32) = happyShift action_28
action_65 (33) = happyShift action_29
action_65 (34) = happyShift action_30
action_65 (46) = happyShift action_31
action_65 (49) = happyShift action_32
action_65 (50) = happyShift action_33
action_65 (8) = happyGoto action_69
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (19) = happyShift action_53
action_66 (20) = happyShift action_54
action_66 (21) = happyShift action_55
action_66 (22) = happyShift action_56
action_66 (25) = happyShift action_57
action_66 (26) = happyShift action_58
action_66 (27) = happyShift action_59
action_66 (28) = happyShift action_60
action_66 (29) = happyShift action_61
action_66 (30) = happyShift action_62
action_66 (43) = happyShift action_63
action_66 (44) = happyShift action_64
action_66 (45) = happyShift action_65
action_66 _ = happyReduce_14

action_67 (19) = happyShift action_53
action_67 (20) = happyShift action_54
action_67 (21) = happyShift action_55
action_67 (22) = happyShift action_56
action_67 (25) = happyShift action_57
action_67 (26) = happyShift action_58
action_67 (27) = happyShift action_59
action_67 (28) = happyShift action_60
action_67 (29) = happyShift action_61
action_67 (30) = happyShift action_62
action_67 (43) = happyShift action_63
action_67 (44) = happyShift action_64
action_67 (45) = happyShift action_65
action_67 _ = happyReduce_11

action_68 _ = happyReduce_12

action_69 (19) = happyShift action_53
action_69 (20) = happyShift action_54
action_69 (21) = happyShift action_55
action_69 (22) = happyShift action_56
action_69 (25) = happyShift action_57
action_69 (26) = happyShift action_58
action_69 (27) = happyShift action_59
action_69 (28) = happyShift action_60
action_69 (29) = happyShift action_61
action_69 (30) = happyShift action_62
action_69 (45) = happyFail []
action_69 _ = happyReduce_33

action_70 (19) = happyShift action_53
action_70 (20) = happyShift action_54
action_70 (21) = happyShift action_55
action_70 (22) = happyShift action_56
action_70 (25) = happyShift action_57
action_70 (26) = happyShift action_58
action_70 (27) = happyShift action_59
action_70 (28) = happyShift action_60
action_70 (29) = happyShift action_61
action_70 (30) = happyShift action_62
action_70 (45) = happyShift action_65
action_70 _ = happyReduce_35

action_71 (19) = happyShift action_53
action_71 (20) = happyShift action_54
action_71 (21) = happyShift action_55
action_71 (22) = happyShift action_56
action_71 (25) = happyShift action_57
action_71 (26) = happyShift action_58
action_71 (27) = happyShift action_59
action_71 (28) = happyShift action_60
action_71 (29) = happyShift action_61
action_71 (30) = happyShift action_62
action_71 (45) = happyShift action_65
action_71 _ = happyReduce_34

action_72 (19) = happyShift action_53
action_72 (20) = happyShift action_54
action_72 (21) = happyShift action_55
action_72 (22) = happyShift action_56
action_72 (25) = happyFail []
action_72 (26) = happyFail []
action_72 (27) = happyFail []
action_72 (28) = happyFail []
action_72 (29) = happyFail []
action_72 (30) = happyFail []
action_72 _ = happyReduce_29

action_73 (19) = happyShift action_53
action_73 (20) = happyShift action_54
action_73 (21) = happyShift action_55
action_73 (22) = happyShift action_56
action_73 (25) = happyFail []
action_73 (26) = happyFail []
action_73 (27) = happyFail []
action_73 (28) = happyFail []
action_73 (29) = happyFail []
action_73 (30) = happyFail []
action_73 _ = happyReduce_28

action_74 (19) = happyShift action_53
action_74 (20) = happyShift action_54
action_74 (21) = happyShift action_55
action_74 (22) = happyShift action_56
action_74 (25) = happyFail []
action_74 (26) = happyFail []
action_74 (27) = happyFail []
action_74 (28) = happyFail []
action_74 (29) = happyFail []
action_74 (30) = happyFail []
action_74 _ = happyReduce_30

action_75 (19) = happyShift action_53
action_75 (20) = happyShift action_54
action_75 (21) = happyShift action_55
action_75 (22) = happyShift action_56
action_75 (25) = happyFail []
action_75 (26) = happyFail []
action_75 (27) = happyFail []
action_75 (28) = happyFail []
action_75 (29) = happyFail []
action_75 (30) = happyFail []
action_75 _ = happyReduce_31

action_76 (19) = happyShift action_53
action_76 (20) = happyShift action_54
action_76 (21) = happyShift action_55
action_76 (22) = happyShift action_56
action_76 (25) = happyFail []
action_76 (26) = happyFail []
action_76 (27) = happyFail []
action_76 (28) = happyFail []
action_76 (29) = happyFail []
action_76 (30) = happyFail []
action_76 _ = happyReduce_27

action_77 (19) = happyShift action_53
action_77 (20) = happyShift action_54
action_77 (21) = happyShift action_55
action_77 (22) = happyShift action_56
action_77 (25) = happyFail []
action_77 (26) = happyFail []
action_77 (27) = happyFail []
action_77 (28) = happyFail []
action_77 (29) = happyFail []
action_77 (30) = happyFail []
action_77 _ = happyReduce_26

action_78 (21) = happyShift action_55
action_78 (22) = happyShift action_56
action_78 _ = happyReduce_23

action_79 (21) = happyShift action_55
action_79 (22) = happyShift action_56
action_79 _ = happyReduce_22

action_80 (21) = happyShift action_55
action_80 (22) = happyShift action_56
action_80 _ = happyReduce_25

action_81 (21) = happyShift action_55
action_81 (22) = happyShift action_56
action_81 _ = happyReduce_24

action_82 (19) = happyShift action_53
action_82 (20) = happyShift action_54
action_82 (21) = happyShift action_55
action_82 (22) = happyShift action_56
action_82 (25) = happyShift action_57
action_82 (26) = happyShift action_58
action_82 (27) = happyShift action_59
action_82 (28) = happyShift action_60
action_82 (29) = happyShift action_61
action_82 (30) = happyShift action_62
action_82 (43) = happyShift action_63
action_82 (44) = happyShift action_64
action_82 (45) = happyShift action_65
action_82 _ = happyReduce_37

action_83 _ = happyReduce_39

action_84 (19) = happyShift action_53
action_84 (20) = happyShift action_54
action_84 (21) = happyShift action_55
action_84 (22) = happyShift action_56
action_84 (25) = happyShift action_57
action_84 (26) = happyShift action_58
action_84 (27) = happyShift action_59
action_84 (28) = happyShift action_60
action_84 (29) = happyShift action_61
action_84 (30) = happyShift action_62
action_84 (43) = happyShift action_63
action_84 (44) = happyShift action_64
action_84 (45) = happyShift action_65
action_84 _ = happyReduce_36

action_85 (10) = happyShift action_8
action_85 (11) = happyShift action_9
action_85 (12) = happyShift action_10
action_85 (13) = happyShift action_11
action_85 (23) = happyShift action_12
action_85 (35) = happyShift action_13
action_85 (38) = happyShift action_14
action_85 (39) = happyShift action_15
action_85 (42) = happyShift action_16
action_85 (48) = happyShift action_17
action_85 (49) = happyShift action_18
action_85 (5) = happyGoto action_92
action_85 (6) = happyGoto action_5
action_85 (7) = happyGoto action_6
action_85 (9) = happyGoto action_7
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (10) = happyShift action_8
action_86 (11) = happyShift action_9
action_86 (12) = happyShift action_10
action_86 (13) = happyShift action_11
action_86 (23) = happyShift action_12
action_86 (35) = happyShift action_13
action_86 (38) = happyShift action_14
action_86 (39) = happyShift action_15
action_86 (42) = happyShift action_16
action_86 (48) = happyShift action_17
action_86 (49) = happyShift action_18
action_86 (5) = happyGoto action_91
action_86 (6) = happyGoto action_5
action_86 (7) = happyGoto action_6
action_86 (9) = happyGoto action_7
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (10) = happyShift action_8
action_87 (11) = happyShift action_9
action_87 (12) = happyShift action_10
action_87 (13) = happyShift action_11
action_87 (23) = happyShift action_12
action_87 (35) = happyShift action_13
action_87 (38) = happyShift action_14
action_87 (39) = happyShift action_15
action_87 (42) = happyShift action_16
action_87 (48) = happyShift action_17
action_87 (49) = happyShift action_18
action_87 (5) = happyGoto action_90
action_87 (6) = happyGoto action_5
action_87 (7) = happyGoto action_6
action_87 (9) = happyGoto action_7
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (14) = happyShift action_23
action_88 (15) = happyShift action_24
action_88 (18) = happyShift action_25
action_88 (20) = happyShift action_26
action_88 (23) = happyShift action_27
action_88 (32) = happyShift action_28
action_88 (33) = happyShift action_29
action_88 (34) = happyShift action_30
action_88 (46) = happyShift action_31
action_88 (49) = happyShift action_32
action_88 (50) = happyShift action_33
action_88 (8) = happyGoto action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (19) = happyShift action_53
action_89 (20) = happyShift action_54
action_89 (21) = happyShift action_55
action_89 (22) = happyShift action_56
action_89 (25) = happyShift action_57
action_89 (26) = happyShift action_58
action_89 (27) = happyShift action_59
action_89 (28) = happyShift action_60
action_89 (29) = happyShift action_61
action_89 (30) = happyShift action_62
action_89 (43) = happyShift action_63
action_89 (44) = happyShift action_64
action_89 (45) = happyShift action_65
action_89 _ = happyReduce_10

action_90 (10) = happyShift action_8
action_90 (11) = happyShift action_9
action_90 (12) = happyShift action_10
action_90 (13) = happyShift action_11
action_90 (23) = happyShift action_12
action_90 (35) = happyShift action_13
action_90 (36) = happyShift action_95
action_90 (37) = happyShift action_96
action_90 (38) = happyShift action_14
action_90 (39) = happyShift action_15
action_90 (42) = happyShift action_16
action_90 (48) = happyShift action_17
action_90 (49) = happyShift action_18
action_90 (6) = happyGoto action_40
action_90 (7) = happyGoto action_6
action_90 (9) = happyGoto action_7
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (10) = happyShift action_8
action_91 (11) = happyShift action_9
action_91 (12) = happyShift action_10
action_91 (13) = happyShift action_11
action_91 (23) = happyShift action_12
action_91 (35) = happyShift action_13
action_91 (38) = happyShift action_14
action_91 (39) = happyShift action_15
action_91 (40) = happyShift action_94
action_91 (42) = happyShift action_16
action_91 (48) = happyShift action_17
action_91 (49) = happyShift action_18
action_91 (6) = happyGoto action_40
action_91 (7) = happyGoto action_6
action_91 (9) = happyGoto action_7
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (10) = happyShift action_8
action_92 (11) = happyShift action_9
action_92 (12) = happyShift action_10
action_92 (13) = happyShift action_11
action_92 (23) = happyShift action_12
action_92 (35) = happyShift action_13
action_92 (38) = happyShift action_14
action_92 (39) = happyShift action_15
action_92 (40) = happyShift action_93
action_92 (42) = happyShift action_16
action_92 (48) = happyShift action_17
action_92 (49) = happyShift action_18
action_92 (6) = happyGoto action_40
action_92 (7) = happyGoto action_6
action_92 (9) = happyGoto action_7
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_8

action_94 _ = happyReduce_7

action_95 _ = happyReduce_5

action_96 (10) = happyShift action_8
action_96 (11) = happyShift action_9
action_96 (12) = happyShift action_10
action_96 (13) = happyShift action_11
action_96 (23) = happyShift action_12
action_96 (35) = happyShift action_13
action_96 (38) = happyShift action_14
action_96 (39) = happyShift action_15
action_96 (42) = happyShift action_16
action_96 (48) = happyShift action_17
action_96 (49) = happyShift action_18
action_96 (5) = happyGoto action_97
action_96 (6) = happyGoto action_5
action_96 (7) = happyGoto action_6
action_96 (9) = happyGoto action_7
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (10) = happyShift action_8
action_97 (11) = happyShift action_9
action_97 (12) = happyShift action_10
action_97 (13) = happyShift action_11
action_97 (23) = happyShift action_12
action_97 (35) = happyShift action_13
action_97 (36) = happyShift action_98
action_97 (38) = happyShift action_14
action_97 (39) = happyShift action_15
action_97 (42) = happyShift action_16
action_97 (48) = happyShift action_17
action_97 (49) = happyShift action_18
action_97 (6) = happyGoto action_40
action_97 (7) = happyGoto action_6
action_97 (9) = happyGoto action_7
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_6

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PIf happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 8 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 6 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PWhile happy_var_3 (reverse (happy_var_5))
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PFor happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (PReturn happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PAssignVar happy_var_1 (PVar happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn7
		 (PUpdateVar (PVar happy_var_1) happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 _
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn7
		 (PUpdateVar (PVar happy_var_1) (PAdd (PVar happy_var_1) (PInt 1))
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal (TokenVar _ happy_var_2))
	_
	 =  HappyAbsSyn7
		 (PAddToIntArray (PVar happy_var_2) happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn8
		 (PInt happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyTerminal (TokenString _ happy_var_1))
	 =  HappyAbsSyn8
		 (PString happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyTerminal (TokenIntArray _ happy_var_1))
	 =  HappyAbsSyn8
		 (PIntArray happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn8
		 (PVar happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  8 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (PNegative happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn8
		 (PTrue
	)

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn8
		 (PFalse
	)

happyReduce_22 = happySpecReduce_3  8 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PMult happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  8 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PDiv happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  8 happyReduction_24
happyReduction_24 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PAdd happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  8 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PSub happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  8 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PCompareLessMore happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  8 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PCompareMoreLess happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  8 happyReduction_28
happyReduction_28 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PCompareEqLessMore happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  8 happyReduction_29
happyReduction_29 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PCompareEqMoreLess happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  8 happyReduction_30
happyReduction_30 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PNotEqual happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  8 happyReduction_31
happyReduction_31 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PEqual happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  8 happyReduction_32
happyReduction_32 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (PNot happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  8 happyReduction_33
happyReduction_33 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PXor happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  8 happyReduction_34
happyReduction_34 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (PAnd happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  8 happyReduction_35
happyReduction_35 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (POr happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  8 happyReduction_36
happyReduction_36 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn8
		 (PGetIntValueAt (PVar happy_var_1) happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  8 happyReduction_37
happyReduction_37 (HappyAbsSyn8  happy_var_3)
	_
	_
	 =  HappyAbsSyn8
		 (PGetInputValueAt happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  8 happyReduction_38
happyReduction_38 (HappyTerminal (TokenVar _ happy_var_2))
	_
	 =  HappyAbsSyn8
		 (PLength (PVar happy_var_2)
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  8 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  9 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn9
		 (PBOOL
	)

happyReduce_41 = happySpecReduce_1  9 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn9
		 (PINT
	)

happyReduce_42 = happySpecReduce_1  9 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn9
		 (PSTRING
	)

happyReduce_43 = happySpecReduce_1  9 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn9
		 (PARRAY
	)

happyNewToken action sts stk [] =
	action 52 52 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenTypeBool _ -> cont 10;
	TokenTypeInt _ -> cont 11;
	TokenTypeString _ -> cont 12;
	TokenTypeArray _ -> cont 13;
	TokenInt _ happy_dollar_dollar -> cont 14;
	TokenString _ happy_dollar_dollar -> cont 15;
	TokenStartFile _ -> cont 16;
	TokenEndFile _ -> cont 17;
	TokenInput _ -> cont 18;
	TokenPlus _ -> cont 19;
	TokenSubtract _ -> cont 20;
	TokenMultiply _ -> cont 21;
	TokenDivide _ -> cont 22;
	TokenLParen _ -> cont 23;
	TokenRParen _ -> cont 24;
	TokenLessThan _ -> cont 25;
	TokenGreaterThan _ -> cont 26;
	TokenEq _ -> cont 27;
	TokenNotEq _ -> cont 28;
	TokenLessOrEq _ -> cont 29;
	TokenGreaterOrEq _ -> cont 30;
	TokenAssignment _ -> cont 31;
	TokenIntArray _ happy_dollar_dollar -> cont 32;
	TokenTrue _ -> cont 33;
	TokenFalse _ -> cont 34;
	TokenIf _ -> cont 35;
	TokenEndIf _ -> cont 36;
	TokenElse _ -> cont 37;
	TokenWhile _ -> cont 38;
	TokenFor _ -> cont 39;
	TokenEndLoop _ -> cont 40;
	TokenHasType _ -> cont 41;
	TokenReturn _ -> cont 42;
	TokenAnd _ -> cont 43;
	TokenOr _ -> cont 44;
	TokenXor _ -> cont 45;
	TokenNot _ -> cont 46;
	TokenAt _ -> cont 47;
	TokenAddToArr _ -> cont 48;
	TokenVar _ happy_dollar_dollar -> cont 49;
	TokenLength _ -> cont 50;
	TokenEndStmt _ -> cont 51;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 52 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
projectParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Type = PINT | PBOOL | PSTRING | PARRAY
   deriving (Show,Eq)

type Environment = [ (String,Expr) ]

data Expr = PInt Int | PString String | PIntArray [Int] | PVar String | PNegative Expr | PIn [[Int]] 
            | PTrue | PFalse 
            | PMult Expr Expr | PDiv Expr Expr | PAdd Expr Expr | PSub Expr Expr 
            | PCompareLessMore Expr Expr | PCompareMoreLess Expr Expr | PCompareEqLessMore Expr Expr | PCompareEqMoreLess Expr Expr 
            | PNotEqual Expr Expr | PEqual Expr Expr | PNot Expr | PXor Expr Expr | PAnd Expr Expr | POr Expr Expr
            | PIf Expr [Expr] | PIfElse Expr [Expr] [Expr] | PWhile Expr [Expr] | PFor Expr [Expr] 
            | PReturn Expr | PAssignVar Type Expr Expr | PUpdateVar Expr Expr | PGetIntValueAt Expr Expr | PAddToIntArray Expr Expr
            | PGetInputValueAt Expr | PLength Expr
    deriving (Show,Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
