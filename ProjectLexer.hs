{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "ProjectLexer.x" #-}
module ProjectLexer where
#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#define ALEX_POSN 1
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

#if defined(ALEX_MONAD) || defined(ALEX_MONAD_BYTESTRING)
import Control.Applicative as App (Applicative (..))
#endif

import Data.Word (Word8)
#if defined(ALEX_BASIC_BYTESTRING) || defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING)

import Data.Int (Int64)
import qualified Data.Char
import qualified Data.ByteString.Lazy     as ByteString
import qualified Data.ByteString.Internal as ByteString (w2c)

#elif defined(ALEX_STRICT_BYTESTRING)

import qualified Data.Char
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString hiding (ByteString)
import qualified Data.ByteString.Unsafe   as ByteString

#else

import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

#endif

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_GSCAN)
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, s))
#endif

#if defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING)
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  ByteString.ByteString,        -- current input string
                  Int64)           -- bytes consumed so far

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i   -- no pending bytes when lexing bytestrings

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,_,cs,n) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (b, cs') ->
            let c   = ByteString.w2c b
                p'  = alexMove p c
                n'  = n+1
            in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs',n'))
#endif

#ifdef ALEX_BASIC_BYTESTRING
data AlexInput = AlexInput { alexChar :: {-# UNPACK #-} !Char,      -- previous char
                             alexStr ::  !ByteString.ByteString,    -- current input string
                             alexBytePos :: {-# UNPACK #-} !Int64}  -- bytes consumed so far

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte (AlexInput {alexStr=cs,alexBytePos=n}) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (c, rest) ->
            Just (c, AlexInput {
                alexChar = ByteString.w2c c,
                alexStr =  rest,
                alexBytePos = n+1})
#endif

#ifdef ALEX_STRICT_BYTESTRING
data AlexInput = AlexInput { alexChar :: {-# UNPACK #-} !Char,
                             alexStr :: {-# UNPACK #-} !ByteString.ByteString,
                             alexBytePos :: {-# UNPACK #-} !Int}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte (AlexInput {alexStr=cs,alexBytePos=n}) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (c, rest) ->
            Just (c, AlexInput {
                alexChar = ByteString.w2c c,
                alexStr =  rest,
                alexBytePos = n+1})
#endif

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING) || defined(ALEX_GSCAN)
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
#endif

-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)

#if defined(ALEX_MONAD) || defined(ALEX_MONAD_BYTESTRING)
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
#ifndef ALEX_MONAD_BYTESTRING
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
#else /* ALEX_MONAD_BYTESTRING */
        alex_bpos:: !Int64,     -- bytes consumed so far
        alex_inp :: ByteString.ByteString,      -- the current input
        alex_chr :: !Char,      -- the character before the input
#endif /* ALEX_MONAD_BYTESTRING */
        alex_scd :: !Int        -- the current startcode
#ifdef ALEX_MONAD_USER_STATE
      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
#endif
    }

-- Compile with -funbox-strict-fields for best results!

#ifndef ALEX_MONAD_BYTESTRING
runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bytes = [],
#else /* ALEX_MONAD_BYTESTRING */
runAlex :: ByteString.ByteString -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bpos = 0,
#endif /* ALEX_MONAD_BYTESTRING */
                        alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
#ifdef ALEX_MONAD_USER_STATE
                        alex_ust = alexInitUserState,
#endif
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput
#ifndef ALEX_MONAD_BYTESTRING
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))
#else /* ALEX_MONAD_BYTESTRING */
 = Alex $ \s@AlexState{alex_pos=pos,alex_bpos=bpos,alex_chr=c,alex_inp=inp__} ->
        Right (s, (pos,c,inp__,bpos))
#endif /* ALEX_MONAD_BYTESTRING */

alexSetInput :: AlexInput -> Alex ()
#ifndef ALEX_MONAD_BYTESTRING
alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
#else /* ALEX_MONAD_BYTESTRING */
alexSetInput (pos,c,inp__,bpos)
 = Alex $ \s -> case s{alex_pos=pos,
                       alex_bpos=bpos,
                       alex_chr=c,
                       alex_inp=inp__} of
#endif /* ALEX_MONAD_BYTESTRING */
                  state__@(AlexState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

#if !defined(ALEX_MONAD_BYTESTRING) && defined(ALEX_MONAD_USER_STATE)
alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())
#endif /* !defined(ALEX_MONAD_BYTESTRING) && defined(ALEX_MONAD_USER_STATE) */

alexMonadScan = do
#ifndef ALEX_MONAD_BYTESTRING
  inp__ <- alexGetInput
#else /* ALEX_MONAD_BYTESTRING */
  inp__@(_,_,_,n) <- alexGetInput
#endif /* ALEX_MONAD_BYTESTRING */
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
#ifndef ALEX_MONAD_BYTESTRING
    AlexToken inp__' len action -> do
#else /* ALEX_MONAD_BYTESTRING */
    AlexToken inp__'@(_,_,_,n') _ action -> let len = n'-n in do
#endif /* ALEX_MONAD_BYTESTRING */
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

-- -----------------------------------------------------------------------------
-- Useful token actions

#ifndef ALEX_MONAD_BYTESTRING
type AlexAction result = AlexInput -> Int -> Alex result
#else /* ALEX_MONAD_BYTESTRING */
type AlexAction result = AlexInput -> Int64 -> Alex result
#endif /* ALEX_MONAD_BYTESTRING */

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

#ifndef ALEX_MONAD_BYTESTRING
token :: (AlexInput -> Int -> token) -> AlexAction token
#else /* ALEX_MONAD_BYTESTRING */
token :: (AlexInput -> Int64 -> token) -> AlexAction token
#endif /* ALEX_MONAD_BYTESTRING */
token t input__ len = return (t input__ len)
#endif /* defined(ALEX_MONAD) || defined(ALEX_MONAD_BYTESTRING) */


-- -----------------------------------------------------------------------------
-- Basic wrapper

#ifdef ALEX_BASIC
type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode' c of
                             (b, bs) -> Just (b, (c, bs, s))
#endif


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

#ifdef ALEX_BASIC_BYTESTRING

-- alexScanTokens :: ByteString.ByteString -> [token]
alexScanTokens str = go (AlexInput '\n' str 0)
  where go inp__ =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' _ act ->
                  let len = alexBytePos inp__' - alexBytePos inp__ in
                  act (ByteString.take len (alexStr inp__)) : go inp__'

#endif

#ifdef ALEX_STRICT_BYTESTRING

-- alexScanTokens :: ByteString.ByteString -> [token]
alexScanTokens str = go (AlexInput '\n' str 0)
  where go inp__ =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' _ act ->
                  let len = alexBytePos inp__' - alexBytePos inp__ in
                  act (ByteString.take len (alexStr inp__)) : go inp__'

#endif


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

#ifdef ALEX_POSN
--alexScanTokens :: String -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'
#endif


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

#ifdef ALEX_POSN_BYTESTRING
--alexScanTokens :: ByteString.ByteString -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',str0,0)
  where go inp__@(pos,_,str,n) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _len       -> go inp__'
                AlexToken inp__'@(_,_,_,n') _ act ->
                  act pos (ByteString.take (n'-n) str) : go inp__'
#endif


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

#ifdef ALEX_GSCAN
alexGScan stop__ state__ inp__ =
  alex_gscan stop__ alexStartPos '\n' [] inp__ (0,state__)

alex_gscan stop__ p c bs inp__ (sc,state__) =
  case alexScan (p,c,bs,inp__) sc of
        AlexEOF     -> stop__ p c inp__ (sc,state__)
        AlexError _ -> stop__ p c inp__ (sc,state__)
        AlexSkip (p',c',bs',inp__') _len ->
          alex_gscan stop__ p' c' bs' inp__' (sc,state__)
        AlexToken (p',c',bs',inp__') len k ->
           k p c inp__ len (\scs -> alex_gscan stop__ p' c' bs' inp__' scs)                  (sc,state__)
#endif
alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 119)
  [ -8
  , -63
  , 0
  , -75
  , -77
  , -60
  , 0
  , -38
  , 72
  , 0
  , 82
  , 0
  , 0
  , 0
  , 0
  , -41
  , 0
  , -21
  , -39
  , -34
  , 0
  , 0
  , 0
  , 0
  , -22
  , 0
  , -42
  , -55
  , 0
  , 0
  , 0
  , 0
  , 0
  , 33
  , 98
  , 162
  , 290
  , 418
  , 674
  , 792
  , 1038
  , 92
  , -52
  , -27
  , -58
  , 0
  , -29
  , -26
  , -14
  , -18
  , -7
  , 0
  , 0
  , -16
  , -13
  , -9
  , 0
  , 0
  , 0
  , 0
  , 1151
  , 636
  , 881
  , 1279
  , 1407
  , 1663
  , 1781
  , 141
  , -4
  , 0
  , 1753
  , 0
  , 1
  , 88
  , 0
  , 5
  , -11
  , 7
  , 3
  , 13
  , 0
  , 0
  , 2
  , 6
  , 0
  , 36
  , 8
  , 0
  , 37
  , 0
  , 16
  , 0
  , 10
  , 73
  , 74
  , 76
  , 89
  , 0
  , 0
  , 77
  , 83
  , 84
  , 91
  , 93
  , 727
  , 0
  , 86
  , 739
  , 0
  , 100
  , 95
  , 101
  , 734
  , 0
  , 738
  , 725
  , 0
  , 740
  , 0
  , 726
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 2036)
  [ 0
  , 67
  , 67
  , 67
  , 67
  , 67
  , 2
  , 4
  , 5
  , 6
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 16
  , 12
  , 13
  , 66
  , 67
  , 15
  , 39
  , 14
  , 118
  , 52
  , 43
  , 45
  , 21
  , 20
  , 23
  , 25
  , 27
  , 24
  , 44
  , 22
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 97
  , 69
  , 19
  , 17
  , 18
  , 47
  , 48
  , 73
  , 53
  , 49
  , 50
  , 104
  , 107
  , 54
  , 51
  , 26
  , 55
  , 56
  , 79
  , 71
  , 83
  , 88
  , 119
  , 75
  , 96
  , 46
  , 3
  , 74
  , 76
  , 112
  , 86
  , 77
  , 78
  , 10
  , 89
  , 82
  , 81
  , 85
  , 91
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 29
  , 7
  , 28
  , 84
  , 87
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 67
  , 67
  , 67
  , 67
  , 67
  , 92
  , 72
  , 98
  , 95
  , 93
  , 94
  , 102
  , 99
  , 100
  , 117
  , 9
  , 90
  , 101
  , 105
  , 108
  , 42
  , 109
  , 80
  , 67
  , 110
  , 11
  , 34
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 35
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 37
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 38
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 115
  , 68
  , 103
  , 111
  , 113
  , 114
  , 1
  , 116
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 106
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 40
  , 62
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 38
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 35
  , 37
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 34
  , 36
  , 30
  , 30
  , 30
  , 33
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 40
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 38
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 32
  , 35
  , 37
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 34
  , 36
  , 30
  , 30
  , 30
  , 33
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 61
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 64
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 65
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 70
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 0
  , 0
  , 0
  , 0
  , 70
  , 0
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 62
  , 64
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 58
  , 61
  , 63
  , 57
  , 57
  , 57
  , 60
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 2036)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 69
  , 82
  , 85
  , 69
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 61
  , 42
  , 61
  , 45
  , 32
  , 33
  , 34
  , 61
  , 70
  , 84
  , 82
  , 89
  , 40
  , 41
  , 42
  , 43
  , 78
  , 45
  , 65
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 84
  , 82
  , 65
  , 66
  , 73
  , 78
  , 69
  , 70
  , 79
  , 71
  , 73
  , 79
  , 76
  , 76
  , 68
  , 78
  , 79
  , 76
  , 84
  , 82
  , 83
  , 84
  , 72
  , 71
  , 87
  , 88
  , 78
  , 69
  , 91
  , 68
  , 79
  , 84
  , 79
  , 78
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 44
  , 125
  , 82
  , 82
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 9
  , 10
  , 11
  , 12
  , 13
  , 82
  , 68
  , 80
  , 69
  , 85
  , 84
  , 68
  , 79
  , 79
  , 73
  , 93
  , 78
  , 76
  , 82
  , 69
  , 82
  , 76
  , 84
  , 32
  , 73
  , 93
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , 39
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , 91
  , 92
  , 93
  , 94
  , 95
  , 96
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 124
  , 125
  , 126
  , 127
  , 10
  , 76
  , 65
  , 78
  , 72
  , 69
  , 83
  , 83
  , 70
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 79
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 34
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 10
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 34
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , 39
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , 91
  , 92
  , 93
  , 94
  , 95
  , 96
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 124
  , 125
  , 126
  , 127
  , 10
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 119)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 31
  , 32
  , 39
  , 31
  , 32
  , 39
  , -1
  , -1
  , 39
  , 39
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 58
  , 59
  , 66
  , 58
  , 59
  , 66
  , -1
  , -1
  , 66
  , 66
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 119)
  [ AlexAccNone
  , AlexAccNone
  , AlexAcc 42
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 41
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 40
  , AlexAccNone
  , AlexAcc 39
  , AlexAcc 38
  , AlexAcc 37
  , AlexAcc 36
  , AlexAccNone
  , AlexAcc 35
  , AlexAcc 34
  , AlexAcc 33
  , AlexAcc 32
  , AlexAcc 31
  , AlexAcc 30
  , AlexAcc 29
  , AlexAcc 28
  , AlexAcc 27
  , AlexAcc 26
  , AlexAccNone
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 22
  , AlexAcc 21
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 20
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 19
  , AlexAcc 18
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 17
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccSkip
  , AlexAccSkip
  , AlexAccNone
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 13
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 12
  , AlexAcc 11
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 10
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 9
  , AlexAccNone
  , AlexAcc 8
  , AlexAccNone
  , AlexAcc 7
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 6
  , AlexAcc 5
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 4
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 3
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 2
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 1
  , AlexAccNone
  , AlexAcc 0
  , AlexAccNone
  ]

alex_actions = array (0 :: Int, 43)
  [ (42,alex_action_27)
  , (41,alex_action_26)
  , (40,alex_action_25)
  , (39,alex_action_24)
  , (38,alex_action_23)
  , (37,alex_action_22)
  , (36,alex_action_21)
  , (35,alex_action_20)
  , (34,alex_action_19)
  , (33,alex_action_18)
  , (32,alex_action_17)
  , (31,alex_action_16)
  , (30,alex_action_15)
  , (29,alex_action_14)
  , (28,alex_action_13)
  , (27,alex_action_12)
  , (26,alex_action_11)
  , (25,alex_action_10)
  , (24,alex_action_9)
  , (23,alex_action_8)
  , (22,alex_action_7)
  , (21,alex_action_6)
  , (20,alex_action_5)
  , (19,alex_action_4)
  , (18,alex_action_3)
  , (17,alex_action_2)
  , (16,alex_action_44)
  , (15,alex_action_43)
  , (14,alex_action_42)
  , (13,alex_action_41)
  , (12,alex_action_40)
  , (11,alex_action_39)
  , (10,alex_action_38)
  , (9,alex_action_37)
  , (8,alex_action_36)
  , (7,alex_action_35)
  , (6,alex_action_34)
  , (5,alex_action_33)
  , (4,alex_action_32)
  , (3,alex_action_31)
  , (2,alex_action_30)
  , (1,alex_action_29)
  , (0,alex_action_28)
  ]

{-# LINE 65 "ProjectLexer.x" #-}
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenTypeBool AlexPosn         | 
  TokenTypeInt  AlexPosn         |
  TokenTypeString AlexPosn       |
  TokenTypeArray AlexPosn        |
 -- TokenComma AlexPosn            |
  TokenInt AlexPosn Int          | 
  TokenString AlexPosn String    |
  TokenStartFile AlexPosn        |
  TokenEndFile AlexPosn          |
  TokenInput AlexPosn            |
  TokenPlus AlexPosn             |
  TokenSubtract AlexPosn         |
  TokenMultiply AlexPosn         |
  TokenDivide AlexPosn           |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLessThan AlexPosn         |
  TokenGreaterThan AlexPosn      |
  TokenEq AlexPosn               |
  TokenNotEq AlexPosn            |
  TokenLessOrEq AlexPosn         |
  TokenGreaterOrEq AlexPosn      |
  TokenAssignment AlexPosn       |
  TokenIntArray AlexPosn [Int]   |
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |
  TokenIf AlexPosn               |
  TokenEndIf AlexPosn            |
  TokenElse AlexPosn             |
  TokenWhile AlexPosn            |
  TokenFor AlexPosn              |
  TokenEndLoop AlexPosn          |
  TokenHasType AlexPosn          |
  TokenReturn AlexPosn           |
  TokenAnd AlexPosn              |
  TokenOr AlexPosn               |
  TokenXor AlexPosn              |
  TokenNot AlexPosn              |
  TokenAt AlexPosn               |
  TokenLength AlexPosn           |
  TokenAddToArr AlexPosn         |
  TokenVar AlexPosn String       |
  TokenEndStmt AlexPosn          
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeString (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeArray (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStartFile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndFile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtract (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessOrEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterOrEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAssignment (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntArray (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndLoop (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHasType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenXor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddToArr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndStmt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
alex_action_2 = tok (\p s -> TokenTypeBool p)
alex_action_3 = tok (\p s -> TokenTypeInt p)
alex_action_4 = tok (\p s -> TokenTypeString p)
alex_action_5 = tok (\p s -> TokenTypeArray p)
alex_action_6 = tok (\p s -> TokenInt p (read s))
alex_action_7 = tok (\p s -> TokenString p s)
alex_action_8 = tok (\p s -> TokenStartFile p)
alex_action_9 = tok (\p s -> TokenEndFile p)
alex_action_10 = tok (\p s -> TokenInput p)
alex_action_11 = tok (\p s -> TokenPlus p)
alex_action_12 = tok (\p s -> TokenSubtract p)
alex_action_13 = tok (\p s -> TokenMultiply p)
alex_action_14 = tok (\p s -> TokenDivide p)
alex_action_15 = tok (\p s -> TokenLParen p)
alex_action_16 = tok (\p s -> TokenRParen p)
alex_action_17 = tok (\p s -> TokenLessThan p)
alex_action_18 = tok (\p s -> TokenGreaterThan p)
alex_action_19 = tok (\p s -> TokenEq p )
alex_action_20 = tok (\p s -> TokenNotEq p)
alex_action_21 = tok (\p s -> TokenLessOrEq p)
alex_action_22 = tok (\p s -> TokenGreaterOrEq p)
alex_action_23 = tok (\p s -> TokenAssignment p)
alex_action_24 = tok (\p s -> TokenIntArray p [])
alex_action_25 = tok (\p s -> TokenIntArray p (read s :: [Int]))
alex_action_26 = tok (\p s -> TokenTrue p)
alex_action_27 = tok (\p s -> TokenFalse p)
alex_action_28 = tok (\p s -> TokenIf p)
alex_action_29 = tok (\p s -> TokenEndIf p)
alex_action_30 = tok (\p s -> TokenElse p)
alex_action_31 = tok (\p s -> TokenWhile p)
alex_action_32 = tok (\p s -> TokenFor p)
alex_action_33 = tok (\p s -> TokenEndLoop p)
alex_action_34 = tok (\p s -> TokenHasType p)
alex_action_35 = tok (\p s -> TokenReturn p)
alex_action_36 = tok (\p s -> TokenAnd p)
alex_action_37 = tok (\p s -> TokenOr p)
alex_action_38 = tok (\p s -> TokenXor p)
alex_action_39 = tok (\p s -> TokenNot p)
alex_action_40 = tok (\p s -> TokenAt p)
alex_action_41 = tok (\p s -> TokenLength p)
alex_action_42 = tok (\p s -> TokenAddToArr p)
alex_action_43 = tok (\p s -> TokenVar p s)
alex_action_44 = tok (\p s -> TokenEndStmt p)

#define ALEX_NOPRED 1
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

#ifdef ALEX_GHC
#  define ILIT(n) n#
#  define IBOX(n) (I# (n))
#  define FAST_INT Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#  if __GLASGOW_HASKELL__ > 706
#    define GTE(n,m) (tagToEnum# (n >=# m))
#    define EQ(n,m) (tagToEnum# (n ==# m))
#  else
#    define GTE(n,m) (n >=# m)
#    define EQ(n,m) (n ==# m)
#  endif
#  define PLUS(n,m) (n +# m)
#  define MINUS(n,m) (n -# m)
#  define TIMES(n,m) (n *# m)
#  define NEGATE(n) (negateInt# (n))
#  define IF_GHC(x) (x)
#else
#  define ILIT(n) (n)
#  define IBOX(n) (n)
#  define FAST_INT Int
#  define GTE(n,m) (n >= m)
#  define EQ(n,m) (n == m)
#  define PLUS(n,m) (n + m)
#  define MINUS(n,m) (n - m)
#  define TIMES(n,m) (n * m)
#  define NEGATE(n) (negate (n))
#  define IF_GHC(x)
#endif

#ifdef ALEX_GHC
data AlexAddr = AlexA# Addr#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ < 503
uncheckedShiftL# = shiftL#
#endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt16OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow16Int# i
  where
        i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
        high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
        low  = int2Word# (ord# (indexCharOffAddr# arr off'))
        off' = off *# 2#
#else
#if __GLASGOW_HASKELL__ >= 901
  int16ToInt#
#endif
    (indexInt16OffAddr# arr off)
#endif
#else
alexIndexInt16OffAddr arr off = arr ! off
#endif

#ifdef ALEX_GHC
{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt32OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
                     (b2 `uncheckedShiftL#` 16#) `or#`
                     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
#else
#if __GLASGOW_HASKELL__ >= 901
  int32ToInt#
#endif
    (indexInt32OffAddr# arr off)
#endif
#else
alexIndexInt32OffAddr arr off = arr ! off
#endif

#ifdef ALEX_GHC

#if __GLASGOW_HASKELL__ < 503
quickIndex arr i = arr ! i
#else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#endif
#else
quickIndex arr i = arr ! i
#endif

-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ IBOX(sc)
  = alexScanUser undefined input__ IBOX(sc)

alexScanUser user__ input__ IBOX(sc)
  = case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->
#ifdef ALEX_DEBUG
                                   trace ("End of input.") $
#endif
                                   AlexEOF
      Just _ ->
#ifdef ALEX_DEBUG
                                   trace ("Error.") $
#endif
                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->
#ifdef ALEX_DEBUG
    trace ("Skipping.") $
#endif
    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->
#ifdef ALEX_DEBUG
    trace ("Accept.") $
#endif
    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` IBOX(s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->
#ifdef ALEX_DEBUG
      trace ("State: " ++ show IBOX(s) ++ ", char: " ++ show c) $
#endif
      case fromIntegral c of { IBOX(ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = PLUS(base,ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if GTE(offset,ILIT(0)) && EQ(check,ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            ILIT(-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input
#ifdef ALEX_LATIN1
                   PLUS(len,ILIT(1))
                   -- issue 119: in the latin1 encoding, *each* byte is one character
#else
                   (if c < 0x80 || c >= 0xC0 then PLUS(len,ILIT(1)) else len)
                   -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
#endif
                   new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ IBOX(len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ IBOX(len)
#ifndef ALEX_NOPRED
        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastAcc a input__ IBOX(len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastSkip input__ IBOX(len)
           | otherwise
           = check_accs rest
#endif

data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip
#ifndef ALEX_NOPRED
  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext IBOX(sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
#endif
