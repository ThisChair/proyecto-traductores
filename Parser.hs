{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import TokenInfo
import Tree

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36

action_0 (54) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (7) = happyGoto action_2
action_0 (8) = happyGoto action_3
action_0 (10) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 _ = happyReduce_27

action_1 (54) = happyShift action_6
action_1 (7) = happyGoto action_2
action_1 (8) = happyGoto action_3
action_1 (10) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 _ = happyFail

action_2 (37) = happyShift action_10
action_2 _ = happyFail

action_3 (78) = happyShift action_9
action_3 _ = happyFail

action_4 _ = happyReduce_29

action_5 _ = happyReduce_30

action_6 (83) = happyShift action_8
action_6 _ = happyFail

action_7 (86) = happyAccept
action_7 _ = happyFail

action_8 (72) = happyShift action_38
action_8 _ = happyFail

action_9 (54) = happyShift action_6
action_9 (7) = happyGoto action_37
action_9 (8) = happyGoto action_3
action_9 (10) = happyGoto action_4
action_9 (11) = happyGoto action_5
action_9 _ = happyReduce_27

action_10 (38) = happyShift action_27
action_10 (39) = happyShift action_28
action_10 (40) = happyShift action_29
action_10 (42) = happyShift action_30
action_10 (44) = happyShift action_31
action_10 (47) = happyShift action_32
action_10 (48) = happyShift action_33
action_10 (52) = happyShift action_34
action_10 (66) = happyShift action_35
action_10 (78) = happyReduce_53
action_10 (83) = happyShift action_36
action_10 (5) = happyGoto action_11
action_10 (9) = happyGoto action_12
action_10 (16) = happyGoto action_13
action_10 (22) = happyGoto action_14
action_10 (23) = happyGoto action_15
action_10 (24) = happyGoto action_16
action_10 (25) = happyGoto action_17
action_10 (26) = happyGoto action_18
action_10 (29) = happyGoto action_19
action_10 (30) = happyGoto action_20
action_10 (31) = happyGoto action_21
action_10 (32) = happyGoto action_22
action_10 (33) = happyGoto action_23
action_10 (34) = happyGoto action_24
action_10 (35) = happyGoto action_25
action_10 (36) = happyGoto action_26
action_10 _ = happyReduce_2

action_11 (41) = happyShift action_70
action_11 _ = happyFail

action_12 _ = happyReduce_60

action_13 _ = happyReduce_59

action_14 (78) = happyShift action_69
action_14 _ = happyFail

action_15 _ = happyReduce_58

action_16 _ = happyReduce_55

action_17 _ = happyReduce_56

action_18 _ = happyReduce_57

action_19 _ = happyReduce_54

action_20 _ = happyReduce_69

action_21 _ = happyReduce_70

action_22 _ = happyReduce_71

action_23 _ = happyReduce_72

action_24 _ = happyReduce_73

action_25 _ = happyReduce_74

action_26 _ = happyReduce_75

action_27 (83) = happyShift action_68
action_27 _ = happyFail

action_28 (56) = happyShift action_49
action_28 (69) = happyShift action_50
action_28 (72) = happyShift action_51
action_28 (81) = happyShift action_52
action_28 (82) = happyShift action_53
action_28 (83) = happyShift action_54
action_28 (84) = happyShift action_55
action_28 (85) = happyShift action_66
action_28 (6) = happyGoto action_63
action_28 (16) = happyGoto action_48
action_28 (27) = happyGoto action_64
action_28 (28) = happyGoto action_67
action_28 _ = happyFail

action_29 (56) = happyShift action_49
action_29 (69) = happyShift action_50
action_29 (72) = happyShift action_51
action_29 (81) = happyShift action_52
action_29 (82) = happyShift action_53
action_29 (83) = happyShift action_54
action_29 (84) = happyShift action_55
action_29 (85) = happyShift action_66
action_29 (6) = happyGoto action_63
action_29 (16) = happyGoto action_48
action_29 (27) = happyGoto action_64
action_29 (28) = happyGoto action_65
action_29 _ = happyFail

action_30 (79) = happyShift action_43
action_30 (80) = happyShift action_44
action_30 (15) = happyGoto action_60
action_30 (19) = happyGoto action_61
action_30 (20) = happyGoto action_62
action_30 _ = happyReduce_46

action_31 (56) = happyShift action_49
action_31 (69) = happyShift action_50
action_31 (72) = happyShift action_51
action_31 (81) = happyShift action_52
action_31 (82) = happyShift action_53
action_31 (83) = happyShift action_54
action_31 (84) = happyShift action_55
action_31 (6) = happyGoto action_59
action_31 (16) = happyGoto action_48
action_31 _ = happyFail

action_32 (56) = happyShift action_49
action_32 (69) = happyShift action_50
action_32 (72) = happyShift action_51
action_32 (81) = happyShift action_52
action_32 (82) = happyShift action_53
action_32 (83) = happyShift action_54
action_32 (84) = happyShift action_55
action_32 (6) = happyGoto action_58
action_32 (16) = happyGoto action_48
action_32 _ = happyFail

action_33 (83) = happyShift action_57
action_33 _ = happyFail

action_34 (56) = happyShift action_49
action_34 (69) = happyShift action_50
action_34 (72) = happyShift action_51
action_34 (81) = happyShift action_52
action_34 (82) = happyShift action_53
action_34 (83) = happyShift action_54
action_34 (84) = happyShift action_55
action_34 (6) = happyGoto action_56
action_34 (16) = happyGoto action_48
action_34 _ = happyFail

action_35 (56) = happyShift action_49
action_35 (69) = happyShift action_50
action_35 (72) = happyShift action_51
action_35 (81) = happyShift action_52
action_35 (82) = happyShift action_53
action_35 (83) = happyShift action_54
action_35 (84) = happyShift action_55
action_35 (6) = happyGoto action_47
action_35 (16) = happyGoto action_48
action_35 _ = happyFail

action_36 (67) = happyShift action_45
action_36 (72) = happyShift action_46
action_36 _ = happyFail

action_37 _ = happyReduce_28

action_38 (79) = happyShift action_43
action_38 (80) = happyShift action_44
action_38 (12) = happyGoto action_39
action_38 (13) = happyGoto action_40
action_38 (14) = happyGoto action_41
action_38 (15) = happyGoto action_42
action_38 _ = happyReduce_34

action_39 (73) = happyShift action_105
action_39 _ = happyFail

action_40 _ = happyReduce_35

action_41 (77) = happyShift action_104
action_41 _ = happyReduce_36

action_42 (83) = happyShift action_103
action_42 _ = happyFail

action_43 _ = happyReduce_40

action_44 _ = happyReduce_39

action_45 (56) = happyShift action_49
action_45 (69) = happyShift action_50
action_45 (72) = happyShift action_51
action_45 (81) = happyShift action_52
action_45 (82) = happyShift action_53
action_45 (83) = happyShift action_54
action_45 (84) = happyShift action_55
action_45 (6) = happyGoto action_102
action_45 (16) = happyGoto action_48
action_45 _ = happyFail

action_46 (56) = happyShift action_49
action_46 (69) = happyShift action_50
action_46 (72) = happyShift action_51
action_46 (81) = happyShift action_52
action_46 (82) = happyShift action_53
action_46 (83) = happyShift action_54
action_46 (84) = happyShift action_55
action_46 (6) = happyGoto action_99
action_46 (16) = happyGoto action_48
action_46 (17) = happyGoto action_100
action_46 (18) = happyGoto action_101
action_46 _ = happyReduce_42

action_47 (57) = happyShift action_74
action_47 (58) = happyShift action_75
action_47 (59) = happyShift action_76
action_47 (60) = happyShift action_77
action_47 (61) = happyShift action_78
action_47 (62) = happyShift action_79
action_47 (63) = happyShift action_80
action_47 (64) = happyShift action_81
action_47 (68) = happyShift action_82
action_47 (69) = happyShift action_83
action_47 (70) = happyShift action_84
action_47 (71) = happyShift action_85
action_47 (74) = happyShift action_86
action_47 (75) = happyShift action_87
action_47 (76) = happyShift action_88
action_47 _ = happyReduce_31

action_48 _ = happyReduce_26

action_49 (56) = happyShift action_49
action_49 (69) = happyShift action_50
action_49 (72) = happyShift action_51
action_49 (81) = happyShift action_52
action_49 (82) = happyShift action_53
action_49 (83) = happyShift action_54
action_49 (84) = happyShift action_55
action_49 (6) = happyGoto action_98
action_49 (16) = happyGoto action_48
action_49 _ = happyFail

action_50 (56) = happyShift action_49
action_50 (69) = happyShift action_50
action_50 (72) = happyShift action_51
action_50 (81) = happyShift action_52
action_50 (82) = happyShift action_53
action_50 (83) = happyShift action_54
action_50 (84) = happyShift action_55
action_50 (6) = happyGoto action_97
action_50 (16) = happyGoto action_48
action_50 _ = happyFail

action_51 (56) = happyShift action_49
action_51 (69) = happyShift action_50
action_51 (72) = happyShift action_51
action_51 (81) = happyShift action_52
action_51 (82) = happyShift action_53
action_51 (83) = happyShift action_54
action_51 (84) = happyShift action_55
action_51 (6) = happyGoto action_96
action_51 (16) = happyGoto action_48
action_51 _ = happyFail

action_52 _ = happyReduce_23

action_53 _ = happyReduce_24

action_54 (72) = happyShift action_46
action_54 _ = happyReduce_25

action_55 _ = happyReduce_13

action_56 (53) = happyShift action_95
action_56 (57) = happyShift action_74
action_56 (58) = happyShift action_75
action_56 (59) = happyShift action_76
action_56 (60) = happyShift action_77
action_56 (61) = happyShift action_78
action_56 (62) = happyShift action_79
action_56 (63) = happyShift action_80
action_56 (64) = happyShift action_81
action_56 (68) = happyShift action_82
action_56 (69) = happyShift action_83
action_56 (70) = happyShift action_84
action_56 (71) = happyShift action_85
action_56 (74) = happyShift action_86
action_56 (75) = happyShift action_87
action_56 (76) = happyShift action_88
action_56 _ = happyFail

action_57 (49) = happyShift action_94
action_57 _ = happyFail

action_58 (43) = happyShift action_93
action_58 (57) = happyShift action_74
action_58 (58) = happyShift action_75
action_58 (59) = happyShift action_76
action_58 (60) = happyShift action_77
action_58 (61) = happyShift action_78
action_58 (62) = happyShift action_79
action_58 (63) = happyShift action_80
action_58 (64) = happyShift action_81
action_58 (68) = happyShift action_82
action_58 (69) = happyShift action_83
action_58 (70) = happyShift action_84
action_58 (71) = happyShift action_85
action_58 (74) = happyShift action_86
action_58 (75) = happyShift action_87
action_58 (76) = happyShift action_88
action_58 _ = happyFail

action_59 (45) = happyShift action_92
action_59 (57) = happyShift action_74
action_59 (58) = happyShift action_75
action_59 (59) = happyShift action_76
action_59 (60) = happyShift action_77
action_59 (61) = happyShift action_78
action_59 (62) = happyShift action_79
action_59 (63) = happyShift action_80
action_59 (64) = happyShift action_81
action_59 (68) = happyShift action_82
action_59 (69) = happyShift action_83
action_59 (70) = happyShift action_84
action_59 (71) = happyShift action_85
action_59 (74) = happyShift action_86
action_59 (75) = happyShift action_87
action_59 (76) = happyShift action_88
action_59 _ = happyFail

action_60 (83) = happyShift action_91
action_60 _ = happyFail

action_61 (43) = happyShift action_90
action_61 _ = happyFail

action_62 (78) = happyShift action_89
action_62 _ = happyFail

action_63 (57) = happyShift action_74
action_63 (58) = happyShift action_75
action_63 (59) = happyShift action_76
action_63 (60) = happyShift action_77
action_63 (61) = happyShift action_78
action_63 (62) = happyShift action_79
action_63 (63) = happyShift action_80
action_63 (64) = happyShift action_81
action_63 (68) = happyShift action_82
action_63 (69) = happyShift action_83
action_63 (70) = happyShift action_84
action_63 (71) = happyShift action_85
action_63 (74) = happyShift action_86
action_63 (75) = happyShift action_87
action_63 (76) = happyShift action_88
action_63 _ = happyReduce_66

action_64 (77) = happyShift action_73
action_64 _ = happyReduce_67

action_65 _ = happyReduce_63

action_66 _ = happyReduce_65

action_67 _ = happyReduce_64

action_68 _ = happyReduce_62

action_69 (38) = happyShift action_27
action_69 (39) = happyShift action_28
action_69 (40) = happyShift action_29
action_69 (42) = happyShift action_30
action_69 (44) = happyShift action_31
action_69 (47) = happyShift action_32
action_69 (48) = happyShift action_33
action_69 (52) = happyShift action_34
action_69 (66) = happyShift action_35
action_69 (78) = happyReduce_53
action_69 (83) = happyShift action_36
action_69 (5) = happyGoto action_72
action_69 (9) = happyGoto action_12
action_69 (16) = happyGoto action_13
action_69 (22) = happyGoto action_14
action_69 (23) = happyGoto action_15
action_69 (24) = happyGoto action_16
action_69 (25) = happyGoto action_17
action_69 (26) = happyGoto action_18
action_69 (29) = happyGoto action_19
action_69 (30) = happyGoto action_20
action_69 (31) = happyGoto action_21
action_69 (32) = happyGoto action_22
action_69 (33) = happyGoto action_23
action_69 (34) = happyGoto action_24
action_69 (35) = happyGoto action_25
action_69 (36) = happyGoto action_26
action_69 _ = happyReduce_2

action_70 (78) = happyShift action_71
action_70 _ = happyFail

action_71 _ = happyReduce_1

action_72 _ = happyReduce_3

action_73 (56) = happyShift action_49
action_73 (69) = happyShift action_50
action_73 (72) = happyShift action_51
action_73 (81) = happyShift action_52
action_73 (82) = happyShift action_53
action_73 (83) = happyShift action_54
action_73 (84) = happyShift action_55
action_73 (85) = happyShift action_66
action_73 (6) = happyGoto action_63
action_73 (16) = happyGoto action_48
action_73 (27) = happyGoto action_64
action_73 (28) = happyGoto action_135
action_73 _ = happyFail

action_74 (56) = happyShift action_49
action_74 (69) = happyShift action_50
action_74 (72) = happyShift action_51
action_74 (81) = happyShift action_52
action_74 (82) = happyShift action_53
action_74 (83) = happyShift action_54
action_74 (84) = happyShift action_55
action_74 (6) = happyGoto action_134
action_74 (16) = happyGoto action_48
action_74 _ = happyFail

action_75 (56) = happyShift action_49
action_75 (69) = happyShift action_50
action_75 (72) = happyShift action_51
action_75 (81) = happyShift action_52
action_75 (82) = happyShift action_53
action_75 (83) = happyShift action_54
action_75 (84) = happyShift action_55
action_75 (6) = happyGoto action_133
action_75 (16) = happyGoto action_48
action_75 _ = happyFail

action_76 (56) = happyShift action_49
action_76 (69) = happyShift action_50
action_76 (72) = happyShift action_51
action_76 (81) = happyShift action_52
action_76 (82) = happyShift action_53
action_76 (83) = happyShift action_54
action_76 (84) = happyShift action_55
action_76 (6) = happyGoto action_132
action_76 (16) = happyGoto action_48
action_76 _ = happyFail

action_77 (56) = happyShift action_49
action_77 (69) = happyShift action_50
action_77 (72) = happyShift action_51
action_77 (81) = happyShift action_52
action_77 (82) = happyShift action_53
action_77 (83) = happyShift action_54
action_77 (84) = happyShift action_55
action_77 (6) = happyGoto action_131
action_77 (16) = happyGoto action_48
action_77 _ = happyFail

action_78 (56) = happyShift action_49
action_78 (69) = happyShift action_50
action_78 (72) = happyShift action_51
action_78 (81) = happyShift action_52
action_78 (82) = happyShift action_53
action_78 (83) = happyShift action_54
action_78 (84) = happyShift action_55
action_78 (6) = happyGoto action_130
action_78 (16) = happyGoto action_48
action_78 _ = happyFail

action_79 (56) = happyShift action_49
action_79 (69) = happyShift action_50
action_79 (72) = happyShift action_51
action_79 (81) = happyShift action_52
action_79 (82) = happyShift action_53
action_79 (83) = happyShift action_54
action_79 (84) = happyShift action_55
action_79 (6) = happyGoto action_129
action_79 (16) = happyGoto action_48
action_79 _ = happyFail

action_80 (56) = happyShift action_49
action_80 (69) = happyShift action_50
action_80 (72) = happyShift action_51
action_80 (81) = happyShift action_52
action_80 (82) = happyShift action_53
action_80 (83) = happyShift action_54
action_80 (84) = happyShift action_55
action_80 (6) = happyGoto action_128
action_80 (16) = happyGoto action_48
action_80 _ = happyFail

action_81 (56) = happyShift action_49
action_81 (69) = happyShift action_50
action_81 (72) = happyShift action_51
action_81 (81) = happyShift action_52
action_81 (82) = happyShift action_53
action_81 (83) = happyShift action_54
action_81 (84) = happyShift action_55
action_81 (6) = happyGoto action_127
action_81 (16) = happyGoto action_48
action_81 _ = happyFail

action_82 (56) = happyShift action_49
action_82 (69) = happyShift action_50
action_82 (72) = happyShift action_51
action_82 (81) = happyShift action_52
action_82 (82) = happyShift action_53
action_82 (83) = happyShift action_54
action_82 (84) = happyShift action_55
action_82 (6) = happyGoto action_126
action_82 (16) = happyGoto action_48
action_82 _ = happyFail

action_83 (56) = happyShift action_49
action_83 (69) = happyShift action_50
action_83 (72) = happyShift action_51
action_83 (81) = happyShift action_52
action_83 (82) = happyShift action_53
action_83 (83) = happyShift action_54
action_83 (84) = happyShift action_55
action_83 (6) = happyGoto action_125
action_83 (16) = happyGoto action_48
action_83 _ = happyFail

action_84 (56) = happyShift action_49
action_84 (69) = happyShift action_50
action_84 (72) = happyShift action_51
action_84 (81) = happyShift action_52
action_84 (82) = happyShift action_53
action_84 (83) = happyShift action_54
action_84 (84) = happyShift action_55
action_84 (6) = happyGoto action_124
action_84 (16) = happyGoto action_48
action_84 _ = happyFail

action_85 (56) = happyShift action_49
action_85 (69) = happyShift action_50
action_85 (72) = happyShift action_51
action_85 (81) = happyShift action_52
action_85 (82) = happyShift action_53
action_85 (83) = happyShift action_54
action_85 (84) = happyShift action_55
action_85 (6) = happyGoto action_123
action_85 (16) = happyGoto action_48
action_85 _ = happyFail

action_86 (56) = happyShift action_49
action_86 (69) = happyShift action_50
action_86 (72) = happyShift action_51
action_86 (81) = happyShift action_52
action_86 (82) = happyShift action_53
action_86 (83) = happyShift action_54
action_86 (84) = happyShift action_55
action_86 (6) = happyGoto action_122
action_86 (16) = happyGoto action_48
action_86 _ = happyFail

action_87 (56) = happyShift action_49
action_87 (69) = happyShift action_50
action_87 (72) = happyShift action_51
action_87 (81) = happyShift action_52
action_87 (82) = happyShift action_53
action_87 (83) = happyShift action_54
action_87 (84) = happyShift action_55
action_87 (6) = happyGoto action_121
action_87 (16) = happyGoto action_48
action_87 _ = happyFail

action_88 (56) = happyShift action_49
action_88 (69) = happyShift action_50
action_88 (72) = happyShift action_51
action_88 (81) = happyShift action_52
action_88 (82) = happyShift action_53
action_88 (83) = happyShift action_54
action_88 (84) = happyShift action_55
action_88 (6) = happyGoto action_120
action_88 (16) = happyGoto action_48
action_88 _ = happyFail

action_89 (79) = happyShift action_43
action_89 (80) = happyShift action_44
action_89 (15) = happyGoto action_60
action_89 (19) = happyGoto action_119
action_89 (20) = happyGoto action_62
action_89 _ = happyReduce_46

action_90 (38) = happyShift action_27
action_90 (39) = happyShift action_28
action_90 (40) = happyShift action_29
action_90 (42) = happyShift action_30
action_90 (44) = happyShift action_31
action_90 (47) = happyShift action_32
action_90 (48) = happyShift action_33
action_90 (52) = happyShift action_34
action_90 (66) = happyShift action_35
action_90 (78) = happyReduce_53
action_90 (83) = happyShift action_36
action_90 (5) = happyGoto action_118
action_90 (9) = happyGoto action_12
action_90 (16) = happyGoto action_13
action_90 (22) = happyGoto action_14
action_90 (23) = happyGoto action_15
action_90 (24) = happyGoto action_16
action_90 (25) = happyGoto action_17
action_90 (26) = happyGoto action_18
action_90 (29) = happyGoto action_19
action_90 (30) = happyGoto action_20
action_90 (31) = happyGoto action_21
action_90 (32) = happyGoto action_22
action_90 (33) = happyGoto action_23
action_90 (34) = happyGoto action_24
action_90 (35) = happyGoto action_25
action_90 (36) = happyGoto action_26
action_90 _ = happyReduce_2

action_91 (67) = happyShift action_116
action_91 (77) = happyShift action_117
action_91 _ = happyReduce_48

action_92 (38) = happyShift action_27
action_92 (39) = happyShift action_28
action_92 (40) = happyShift action_29
action_92 (42) = happyShift action_30
action_92 (44) = happyShift action_31
action_92 (47) = happyShift action_32
action_92 (48) = happyShift action_33
action_92 (52) = happyShift action_34
action_92 (66) = happyShift action_35
action_92 (78) = happyReduce_53
action_92 (83) = happyShift action_36
action_92 (5) = happyGoto action_115
action_92 (9) = happyGoto action_12
action_92 (16) = happyGoto action_13
action_92 (22) = happyGoto action_14
action_92 (23) = happyGoto action_15
action_92 (24) = happyGoto action_16
action_92 (25) = happyGoto action_17
action_92 (26) = happyGoto action_18
action_92 (29) = happyGoto action_19
action_92 (30) = happyGoto action_20
action_92 (31) = happyGoto action_21
action_92 (32) = happyGoto action_22
action_92 (33) = happyGoto action_23
action_92 (34) = happyGoto action_24
action_92 (35) = happyGoto action_25
action_92 (36) = happyGoto action_26
action_92 _ = happyReduce_2

action_93 (38) = happyShift action_27
action_93 (39) = happyShift action_28
action_93 (40) = happyShift action_29
action_93 (42) = happyShift action_30
action_93 (44) = happyShift action_31
action_93 (47) = happyShift action_32
action_93 (48) = happyShift action_33
action_93 (52) = happyShift action_34
action_93 (66) = happyShift action_35
action_93 (78) = happyReduce_53
action_93 (83) = happyShift action_36
action_93 (5) = happyGoto action_114
action_93 (9) = happyGoto action_12
action_93 (16) = happyGoto action_13
action_93 (22) = happyGoto action_14
action_93 (23) = happyGoto action_15
action_93 (24) = happyGoto action_16
action_93 (25) = happyGoto action_17
action_93 (26) = happyGoto action_18
action_93 (29) = happyGoto action_19
action_93 (30) = happyGoto action_20
action_93 (31) = happyGoto action_21
action_93 (32) = happyGoto action_22
action_93 (33) = happyGoto action_23
action_93 (34) = happyGoto action_24
action_93 (35) = happyGoto action_25
action_93 (36) = happyGoto action_26
action_93 _ = happyReduce_2

action_94 (56) = happyShift action_49
action_94 (69) = happyShift action_50
action_94 (72) = happyShift action_51
action_94 (81) = happyShift action_52
action_94 (82) = happyShift action_53
action_94 (83) = happyShift action_54
action_94 (84) = happyShift action_55
action_94 (6) = happyGoto action_113
action_94 (16) = happyGoto action_48
action_94 _ = happyFail

action_95 (38) = happyShift action_27
action_95 (39) = happyShift action_28
action_95 (40) = happyShift action_29
action_95 (42) = happyShift action_30
action_95 (44) = happyShift action_31
action_95 (47) = happyShift action_32
action_95 (48) = happyShift action_33
action_95 (52) = happyShift action_34
action_95 (66) = happyShift action_35
action_95 (78) = happyReduce_53
action_95 (83) = happyShift action_36
action_95 (5) = happyGoto action_112
action_95 (9) = happyGoto action_12
action_95 (16) = happyGoto action_13
action_95 (22) = happyGoto action_14
action_95 (23) = happyGoto action_15
action_95 (24) = happyGoto action_16
action_95 (25) = happyGoto action_17
action_95 (26) = happyGoto action_18
action_95 (29) = happyGoto action_19
action_95 (30) = happyGoto action_20
action_95 (31) = happyGoto action_21
action_95 (32) = happyGoto action_22
action_95 (33) = happyGoto action_23
action_95 (34) = happyGoto action_24
action_95 (35) = happyGoto action_25
action_95 (36) = happyGoto action_26
action_95 _ = happyReduce_2

action_96 (57) = happyShift action_74
action_96 (58) = happyShift action_75
action_96 (59) = happyShift action_76
action_96 (60) = happyShift action_77
action_96 (61) = happyShift action_78
action_96 (62) = happyShift action_79
action_96 (63) = happyShift action_80
action_96 (64) = happyShift action_81
action_96 (68) = happyShift action_82
action_96 (69) = happyShift action_83
action_96 (70) = happyShift action_84
action_96 (71) = happyShift action_85
action_96 (73) = happyShift action_111
action_96 (74) = happyShift action_86
action_96 (75) = happyShift action_87
action_96 (76) = happyShift action_88
action_96 _ = happyFail

action_97 _ = happyReduce_12

action_98 _ = happyReduce_16

action_99 (57) = happyShift action_74
action_99 (58) = happyShift action_75
action_99 (59) = happyShift action_76
action_99 (60) = happyShift action_77
action_99 (61) = happyShift action_78
action_99 (62) = happyShift action_79
action_99 (63) = happyShift action_80
action_99 (64) = happyShift action_81
action_99 (68) = happyShift action_82
action_99 (69) = happyShift action_83
action_99 (70) = happyShift action_84
action_99 (71) = happyShift action_85
action_99 (74) = happyShift action_86
action_99 (75) = happyShift action_87
action_99 (76) = happyShift action_88
action_99 (77) = happyShift action_110
action_99 _ = happyReduce_44

action_100 (73) = happyShift action_109
action_100 _ = happyFail

action_101 _ = happyReduce_43

action_102 (57) = happyShift action_74
action_102 (58) = happyShift action_75
action_102 (59) = happyShift action_76
action_102 (60) = happyShift action_77
action_102 (61) = happyShift action_78
action_102 (62) = happyShift action_79
action_102 (63) = happyShift action_80
action_102 (64) = happyShift action_81
action_102 (68) = happyShift action_82
action_102 (69) = happyShift action_83
action_102 (70) = happyShift action_84
action_102 (71) = happyShift action_85
action_102 (74) = happyShift action_86
action_102 (75) = happyShift action_87
action_102 (76) = happyShift action_88
action_102 _ = happyReduce_61

action_103 _ = happyReduce_38

action_104 (79) = happyShift action_43
action_104 (80) = happyShift action_44
action_104 (13) = happyGoto action_108
action_104 (14) = happyGoto action_41
action_104 (15) = happyGoto action_42
action_104 _ = happyFail

action_105 (55) = happyShift action_106
action_105 (65) = happyShift action_107
action_105 _ = happyFail

action_106 (38) = happyShift action_27
action_106 (39) = happyShift action_28
action_106 (40) = happyShift action_29
action_106 (42) = happyShift action_30
action_106 (44) = happyShift action_31
action_106 (47) = happyShift action_32
action_106 (48) = happyShift action_33
action_106 (52) = happyShift action_34
action_106 (66) = happyShift action_35
action_106 (78) = happyReduce_53
action_106 (83) = happyShift action_36
action_106 (5) = happyGoto action_147
action_106 (9) = happyGoto action_12
action_106 (16) = happyGoto action_13
action_106 (22) = happyGoto action_14
action_106 (23) = happyGoto action_15
action_106 (24) = happyGoto action_16
action_106 (25) = happyGoto action_17
action_106 (26) = happyGoto action_18
action_106 (29) = happyGoto action_19
action_106 (30) = happyGoto action_20
action_106 (31) = happyGoto action_21
action_106 (32) = happyGoto action_22
action_106 (33) = happyGoto action_23
action_106 (34) = happyGoto action_24
action_106 (35) = happyGoto action_25
action_106 (36) = happyGoto action_26
action_106 _ = happyReduce_2

action_107 (79) = happyShift action_43
action_107 (80) = happyShift action_44
action_107 (15) = happyGoto action_146
action_107 _ = happyFail

action_108 _ = happyReduce_37

action_109 _ = happyReduce_41

action_110 (56) = happyShift action_49
action_110 (69) = happyShift action_50
action_110 (72) = happyShift action_51
action_110 (81) = happyShift action_52
action_110 (82) = happyShift action_53
action_110 (83) = happyShift action_54
action_110 (84) = happyShift action_55
action_110 (6) = happyGoto action_99
action_110 (16) = happyGoto action_48
action_110 (18) = happyGoto action_145
action_110 _ = happyFail

action_111 _ = happyReduce_11

action_112 (41) = happyShift action_144
action_112 _ = happyFail

action_113 (50) = happyShift action_143
action_113 (57) = happyShift action_74
action_113 (58) = happyShift action_75
action_113 (59) = happyShift action_76
action_113 (60) = happyShift action_77
action_113 (61) = happyShift action_78
action_113 (62) = happyShift action_79
action_113 (63) = happyShift action_80
action_113 (64) = happyShift action_81
action_113 (68) = happyShift action_82
action_113 (69) = happyShift action_83
action_113 (70) = happyShift action_84
action_113 (71) = happyShift action_85
action_113 (74) = happyShift action_86
action_113 (75) = happyShift action_87
action_113 (76) = happyShift action_88
action_113 _ = happyFail

action_114 (41) = happyShift action_142
action_114 _ = happyFail

action_115 (41) = happyShift action_140
action_115 (46) = happyShift action_141
action_115 _ = happyFail

action_116 (56) = happyShift action_49
action_116 (69) = happyShift action_50
action_116 (72) = happyShift action_51
action_116 (81) = happyShift action_52
action_116 (82) = happyShift action_53
action_116 (83) = happyShift action_54
action_116 (84) = happyShift action_55
action_116 (6) = happyGoto action_139
action_116 (16) = happyGoto action_48
action_116 _ = happyFail

action_117 (83) = happyShift action_138
action_117 (21) = happyGoto action_137
action_117 _ = happyFail

action_118 (41) = happyShift action_136
action_118 _ = happyFail

action_119 _ = happyReduce_47

action_120 _ = happyReduce_8

action_121 (59) = happyShift action_76
action_121 (60) = happyShift action_77
action_121 (61) = happyFail
action_121 (62) = happyFail
action_121 (63) = happyFail
action_121 (64) = happyFail
action_121 (68) = happyShift action_82
action_121 (69) = happyShift action_83
action_121 (70) = happyShift action_84
action_121 (71) = happyShift action_85
action_121 (74) = happyFail
action_121 (75) = happyFail
action_121 (76) = happyShift action_88
action_121 _ = happyReduce_18

action_122 (59) = happyShift action_76
action_122 (60) = happyShift action_77
action_122 (61) = happyFail
action_122 (62) = happyFail
action_122 (63) = happyFail
action_122 (64) = happyFail
action_122 (68) = happyShift action_82
action_122 (69) = happyShift action_83
action_122 (70) = happyShift action_84
action_122 (71) = happyShift action_85
action_122 (74) = happyFail
action_122 (75) = happyFail
action_122 (76) = happyShift action_88
action_122 _ = happyReduce_20

action_123 _ = happyReduce_7

action_124 _ = happyReduce_6

action_125 (59) = happyShift action_76
action_125 (60) = happyShift action_77
action_125 (70) = happyShift action_84
action_125 (71) = happyShift action_85
action_125 (76) = happyShift action_88
action_125 _ = happyReduce_5

action_126 (59) = happyShift action_76
action_126 (60) = happyShift action_77
action_126 (70) = happyShift action_84
action_126 (71) = happyShift action_85
action_126 (76) = happyShift action_88
action_126 _ = happyReduce_4

action_127 (59) = happyShift action_76
action_127 (60) = happyShift action_77
action_127 (61) = happyFail
action_127 (62) = happyFail
action_127 (63) = happyFail
action_127 (64) = happyFail
action_127 (68) = happyShift action_82
action_127 (69) = happyShift action_83
action_127 (70) = happyShift action_84
action_127 (71) = happyShift action_85
action_127 (74) = happyFail
action_127 (75) = happyFail
action_127 (76) = happyShift action_88
action_127 _ = happyReduce_19

action_128 (59) = happyShift action_76
action_128 (60) = happyShift action_77
action_128 (61) = happyFail
action_128 (62) = happyFail
action_128 (63) = happyFail
action_128 (64) = happyFail
action_128 (68) = happyShift action_82
action_128 (69) = happyShift action_83
action_128 (70) = happyShift action_84
action_128 (71) = happyShift action_85
action_128 (74) = happyFail
action_128 (75) = happyFail
action_128 (76) = happyShift action_88
action_128 _ = happyReduce_17

action_129 (59) = happyShift action_76
action_129 (60) = happyShift action_77
action_129 (61) = happyFail
action_129 (62) = happyFail
action_129 (63) = happyFail
action_129 (64) = happyFail
action_129 (68) = happyShift action_82
action_129 (69) = happyShift action_83
action_129 (70) = happyShift action_84
action_129 (71) = happyShift action_85
action_129 (74) = happyFail
action_129 (75) = happyFail
action_129 (76) = happyShift action_88
action_129 _ = happyReduce_22

action_130 (59) = happyShift action_76
action_130 (60) = happyShift action_77
action_130 (61) = happyFail
action_130 (62) = happyFail
action_130 (63) = happyFail
action_130 (64) = happyFail
action_130 (68) = happyShift action_82
action_130 (69) = happyShift action_83
action_130 (70) = happyShift action_84
action_130 (71) = happyShift action_85
action_130 (74) = happyFail
action_130 (75) = happyFail
action_130 (76) = happyShift action_88
action_130 _ = happyReduce_21

action_131 _ = happyReduce_10

action_132 _ = happyReduce_9

action_133 (57) = happyShift action_74
action_133 (59) = happyShift action_76
action_133 (60) = happyShift action_77
action_133 (61) = happyShift action_78
action_133 (62) = happyShift action_79
action_133 (63) = happyShift action_80
action_133 (64) = happyShift action_81
action_133 (68) = happyShift action_82
action_133 (69) = happyShift action_83
action_133 (70) = happyShift action_84
action_133 (71) = happyShift action_85
action_133 (74) = happyShift action_86
action_133 (75) = happyShift action_87
action_133 (76) = happyShift action_88
action_133 _ = happyReduce_14

action_134 (59) = happyShift action_76
action_134 (60) = happyShift action_77
action_134 (61) = happyShift action_78
action_134 (62) = happyShift action_79
action_134 (63) = happyShift action_80
action_134 (64) = happyShift action_81
action_134 (68) = happyShift action_82
action_134 (69) = happyShift action_83
action_134 (70) = happyShift action_84
action_134 (71) = happyShift action_85
action_134 (74) = happyShift action_86
action_134 (75) = happyShift action_87
action_134 (76) = happyShift action_88
action_134 _ = happyReduce_15

action_135 _ = happyReduce_68

action_136 _ = happyReduce_76

action_137 _ = happyReduce_50

action_138 (77) = happyShift action_152
action_138 _ = happyReduce_51

action_139 (57) = happyShift action_74
action_139 (58) = happyShift action_75
action_139 (59) = happyShift action_76
action_139 (60) = happyShift action_77
action_139 (61) = happyShift action_78
action_139 (62) = happyShift action_79
action_139 (63) = happyShift action_80
action_139 (64) = happyShift action_81
action_139 (68) = happyShift action_82
action_139 (69) = happyShift action_83
action_139 (70) = happyShift action_84
action_139 (71) = happyShift action_85
action_139 (74) = happyShift action_86
action_139 (75) = happyShift action_87
action_139 (76) = happyShift action_88
action_139 _ = happyReduce_49

action_140 _ = happyReduce_77

action_141 (38) = happyShift action_27
action_141 (39) = happyShift action_28
action_141 (40) = happyShift action_29
action_141 (42) = happyShift action_30
action_141 (44) = happyShift action_31
action_141 (47) = happyShift action_32
action_141 (48) = happyShift action_33
action_141 (52) = happyShift action_34
action_141 (66) = happyShift action_35
action_141 (78) = happyReduce_53
action_141 (83) = happyShift action_36
action_141 (5) = happyGoto action_151
action_141 (9) = happyGoto action_12
action_141 (16) = happyGoto action_13
action_141 (22) = happyGoto action_14
action_141 (23) = happyGoto action_15
action_141 (24) = happyGoto action_16
action_141 (25) = happyGoto action_17
action_141 (26) = happyGoto action_18
action_141 (29) = happyGoto action_19
action_141 (30) = happyGoto action_20
action_141 (31) = happyGoto action_21
action_141 (32) = happyGoto action_22
action_141 (33) = happyGoto action_23
action_141 (34) = happyGoto action_24
action_141 (35) = happyGoto action_25
action_141 (36) = happyGoto action_26
action_141 _ = happyReduce_2

action_142 _ = happyReduce_79

action_143 (56) = happyShift action_49
action_143 (69) = happyShift action_50
action_143 (72) = happyShift action_51
action_143 (81) = happyShift action_52
action_143 (82) = happyShift action_53
action_143 (83) = happyShift action_54
action_143 (84) = happyShift action_55
action_143 (6) = happyGoto action_150
action_143 (16) = happyGoto action_48
action_143 _ = happyFail

action_144 _ = happyReduce_82

action_145 _ = happyReduce_45

action_146 (55) = happyShift action_149
action_146 _ = happyFail

action_147 (41) = happyShift action_148
action_147 _ = happyFail

action_148 _ = happyReduce_32

action_149 (38) = happyShift action_27
action_149 (39) = happyShift action_28
action_149 (40) = happyShift action_29
action_149 (42) = happyShift action_30
action_149 (44) = happyShift action_31
action_149 (47) = happyShift action_32
action_149 (48) = happyShift action_33
action_149 (52) = happyShift action_34
action_149 (66) = happyShift action_35
action_149 (78) = happyReduce_53
action_149 (83) = happyShift action_36
action_149 (5) = happyGoto action_157
action_149 (9) = happyGoto action_12
action_149 (16) = happyGoto action_13
action_149 (22) = happyGoto action_14
action_149 (23) = happyGoto action_15
action_149 (24) = happyGoto action_16
action_149 (25) = happyGoto action_17
action_149 (26) = happyGoto action_18
action_149 (29) = happyGoto action_19
action_149 (30) = happyGoto action_20
action_149 (31) = happyGoto action_21
action_149 (32) = happyGoto action_22
action_149 (33) = happyGoto action_23
action_149 (34) = happyGoto action_24
action_149 (35) = happyGoto action_25
action_149 (36) = happyGoto action_26
action_149 _ = happyReduce_2

action_150 (43) = happyShift action_155
action_150 (51) = happyShift action_156
action_150 (57) = happyShift action_74
action_150 (58) = happyShift action_75
action_150 (59) = happyShift action_76
action_150 (60) = happyShift action_77
action_150 (61) = happyShift action_78
action_150 (62) = happyShift action_79
action_150 (63) = happyShift action_80
action_150 (64) = happyShift action_81
action_150 (68) = happyShift action_82
action_150 (69) = happyShift action_83
action_150 (70) = happyShift action_84
action_150 (71) = happyShift action_85
action_150 (74) = happyShift action_86
action_150 (75) = happyShift action_87
action_150 (76) = happyShift action_88
action_150 _ = happyFail

action_151 (41) = happyShift action_154
action_151 _ = happyFail

action_152 (83) = happyShift action_138
action_152 (21) = happyGoto action_153
action_152 _ = happyFail

action_153 _ = happyReduce_52

action_154 _ = happyReduce_78

action_155 (38) = happyShift action_27
action_155 (39) = happyShift action_28
action_155 (40) = happyShift action_29
action_155 (42) = happyShift action_30
action_155 (44) = happyShift action_31
action_155 (47) = happyShift action_32
action_155 (48) = happyShift action_33
action_155 (52) = happyShift action_34
action_155 (66) = happyShift action_35
action_155 (78) = happyReduce_53
action_155 (83) = happyShift action_36
action_155 (5) = happyGoto action_160
action_155 (9) = happyGoto action_12
action_155 (16) = happyGoto action_13
action_155 (22) = happyGoto action_14
action_155 (23) = happyGoto action_15
action_155 (24) = happyGoto action_16
action_155 (25) = happyGoto action_17
action_155 (26) = happyGoto action_18
action_155 (29) = happyGoto action_19
action_155 (30) = happyGoto action_20
action_155 (31) = happyGoto action_21
action_155 (32) = happyGoto action_22
action_155 (33) = happyGoto action_23
action_155 (34) = happyGoto action_24
action_155 (35) = happyGoto action_25
action_155 (36) = happyGoto action_26
action_155 _ = happyReduce_2

action_156 (56) = happyShift action_49
action_156 (69) = happyShift action_50
action_156 (72) = happyShift action_51
action_156 (81) = happyShift action_52
action_156 (82) = happyShift action_53
action_156 (83) = happyShift action_54
action_156 (84) = happyShift action_55
action_156 (6) = happyGoto action_159
action_156 (16) = happyGoto action_48
action_156 _ = happyFail

action_157 (41) = happyShift action_158
action_157 _ = happyFail

action_158 _ = happyReduce_33

action_159 (43) = happyShift action_162
action_159 (57) = happyShift action_74
action_159 (58) = happyShift action_75
action_159 (59) = happyShift action_76
action_159 (60) = happyShift action_77
action_159 (61) = happyShift action_78
action_159 (62) = happyShift action_79
action_159 (63) = happyShift action_80
action_159 (64) = happyShift action_81
action_159 (68) = happyShift action_82
action_159 (69) = happyShift action_83
action_159 (70) = happyShift action_84
action_159 (71) = happyShift action_85
action_159 (74) = happyShift action_86
action_159 (75) = happyShift action_87
action_159 (76) = happyShift action_88
action_159 _ = happyFail

action_160 (41) = happyShift action_161
action_160 _ = happyFail

action_161 _ = happyReduce_80

action_162 (38) = happyShift action_27
action_162 (39) = happyShift action_28
action_162 (40) = happyShift action_29
action_162 (42) = happyShift action_30
action_162 (44) = happyShift action_31
action_162 (47) = happyShift action_32
action_162 (48) = happyShift action_33
action_162 (52) = happyShift action_34
action_162 (66) = happyShift action_35
action_162 (78) = happyReduce_53
action_162 (83) = happyShift action_36
action_162 (5) = happyGoto action_163
action_162 (9) = happyGoto action_12
action_162 (16) = happyGoto action_13
action_162 (22) = happyGoto action_14
action_162 (23) = happyGoto action_15
action_162 (24) = happyGoto action_16
action_162 (25) = happyGoto action_17
action_162 (26) = happyGoto action_18
action_162 (29) = happyGoto action_19
action_162 (30) = happyGoto action_20
action_162 (31) = happyGoto action_21
action_162 (32) = happyGoto action_22
action_162 (33) = happyGoto action_23
action_162 (34) = happyGoto action_24
action_162 (35) = happyGoto action_25
action_162 (36) = happyGoto action_26
action_162 _ = happyReduce_2

action_163 (41) = happyShift action_164
action_163 _ = happyFail

action_164 _ = happyReduce_81

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Init happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ESum   (Sum    happy_var_1 happy_var_3)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EDif   (Dif    happy_var_1 happy_var_3)
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EMul   (Mul    happy_var_1 happy_var_3)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EDiv   (Div    happy_var_1 happy_var_3)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EMod   (Mod    happy_var_1 happy_var_3)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EDivI  (DivI   happy_var_1 happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EModI  (ModI   happy_var_1 happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (ENeg happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (EToken happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EOr    (Or     happy_var_1 happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EAnd   (And    happy_var_1 happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (ENot   happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EGeq   (Geq    happy_var_1 happy_var_3)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EGr    (Gr     happy_var_1 happy_var_3)
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ELeq   (Leq    happy_var_1 happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ELess  (Less   happy_var_1 happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ENeq   (Neq    happy_var_1 happy_var_3)
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  6 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EEqual (Equal  happy_var_1 happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  6 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (EToken happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  6 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (EToken happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (EToken happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  6 happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn6
		 (EFCall happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  7 happyReduction_27
happyReduction_27  =  HappyAbsSyn7
		 ([]
	)

happyReduce_28 = happySpecReduce_3  7 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  8 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  8 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  9 happyReduction_31
happyReduction_31 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Ret happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 8 10 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (DFun happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 10 11 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (DFunR happy_var_2 happy_var_4 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_0  12 happyReduction_34
happyReduction_34  =  HappyAbsSyn12
		 ([]
	)

happyReduce_35 = happySpecReduce_1  12 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  13 happyReduction_36
happyReduction_36 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  13 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  14 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (Par happy_var_1 happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  15 happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  15 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 16 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (FCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_0  17 happyReduction_42
happyReduction_42  =  HappyAbsSyn17
		 ([]
	)

happyReduce_43 = happySpecReduce_1  17 happyReduction_43
happyReduction_43 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  18 happyReduction_44
happyReduction_44 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  18 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  19 happyReduction_46
happyReduction_46  =  HappyAbsSyn19
		 ([]
	)

happyReduce_47 = happySpecReduce_3  19 happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  20 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn20
		 (Dec1 happy_var_1 [happy_var_2]
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 20 happyReduction_49
happyReduction_49 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Dec2 happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 20 happyReduction_50
happyReduction_50 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Dec1 happy_var_1 ([happy_var_2] ++ happy_var_4)
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  21 happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  21 happyReduction_52
happyReduction_52 (HappyAbsSyn21  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  22 happyReduction_53
happyReduction_53  =  HappyAbsSyn22
		 (IEmpty
	)

happyReduce_54 = happySpecReduce_1  22 happyReduction_54
happyReduction_54 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn22
		 (IBlock   happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  22 happyReduction_55
happyReduction_55 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn22
		 (IReadId  happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  22 happyReduction_56
happyReduction_56 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (IWrite   happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  22 happyReduction_57
happyReduction_57 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn22
		 (IWriteL  happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  22 happyReduction_58
happyReduction_58 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (IAssig   happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  22 happyReduction_59
happyReduction_59 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn22
		 (IFcall   happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  22 happyReduction_60
happyReduction_60 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn22
		 (IRet     happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  23 happyReduction_61
happyReduction_61 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (Assig  happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  24 happyReduction_62
happyReduction_62 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn24
		 (ReadId happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  25 happyReduction_63
happyReduction_63 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Write  happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  26 happyReduction_64
happyReduction_64 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (WriteL happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  27 happyReduction_65
happyReduction_65 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (PToken happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn27
		 (PExp   happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  28 happyReduction_67
happyReduction_67 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 ([happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  28 happyReduction_68
happyReduction_68 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  29 happyReduction_69
happyReduction_69 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (BDo      happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  29 happyReduction_70
happyReduction_70 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn29
		 (BIf      happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  29 happyReduction_71
happyReduction_71 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn29
		 (BIfElse  happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  29 happyReduction_72
happyReduction_72 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn29
		 (BWhile   happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  29 happyReduction_73
happyReduction_73 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn29
		 (BFor     happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  29 happyReduction_74
happyReduction_74 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn29
		 (BForby   happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  29 happyReduction_75
happyReduction_75 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn29
		 (BRepeat  happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happyReduce 5 30 happyReduction_76
happyReduction_76 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (Do     happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 5 31 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 (If     happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_78 = happyReduce 7 32 happyReduction_78
happyReduction_78 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (IfElse happy_var_2 happy_var_4  happy_var_6
	) `HappyStk` happyRest

happyReduce_79 = happyReduce 5 33 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (While  happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 9 34 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (For    happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 11 35 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn35
		 (ForBy  happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 5 36 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (Repeat happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 86 86 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TProgram _ -> cont 37;
	TRead _ -> cont 38;
	TWriteLn _ -> cont 39;
	TWrite _ -> cont 40;
	TEnd _ -> cont 41;
	TWith _ -> cont 42;
	TDo _ -> cont 43;
	TIf _ -> cont 44;
	TThen _ -> cont 45;
	TElse _ -> cont 46;
	TWhile _ -> cont 47;
	TFor _ -> cont 48;
	TFrom _ -> cont 49;
	TTo _ -> cont 50;
	TBy _ -> cont 51;
	TRepeat _ -> cont 52;
	TTimes _ -> cont 53;
	TFunc _ -> cont 54;
	TBegin _ -> cont 55;
	TNot _ -> cont 56;
	TAnd _ -> cont 57;
	TOr _ -> cont 58;
	TDiv _ -> cont 59;
	TMod _ -> cont 60;
	TNotEq _ -> cont 61;
	TEq _ -> cont 62;
	TMoreEq _ -> cont 63;
	TLessEq _ -> cont 64;
	TArrow _ -> cont 65;
	TReturn _ -> cont 66;
	TAssign _ -> cont 67;
	TPlus _ -> cont 68;
	TMinus _ -> cont 69;
	TStar _ -> cont 70;
	TSlash _ -> cont 71;
	TOpenP _ -> cont 72;
	TCloseP _ -> cont 73;
	TLess _ -> cont 74;
	TMore _ -> cont 75;
	TPercent _ -> cont 76;
	TComma _ -> cont 77;
	TSColon _ -> cont 78;
	TBoolean _ -> cont 79;
	TNumber _ -> cont 80;
	TTrue _ -> cont 81;
	TFalse _ -> cont 82;
	TIdent _ _ -> cont 83;
	TNum _ _ -> cont 84;
	TString _ _ -> cont 85;
	_ -> happyError' (tk:tks)
	}

happyError_ 86 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseRet tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError [] = error $ "Final inesperado"
parseError t = error $ show_pos (head t) ++ ": token inesperado: " ++ show_val (head t)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}








{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 154 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 255 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 321 "templates/GenericTemplate.hs" #-}
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
