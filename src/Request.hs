{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, ViewPatterns #-}

module Request (is_addressed_request, is_short_request, EditableRequest(..), EditableRequestKind(..), Context(..), Response(..), EvalOpt(..), EphemeralOpt(..), HistoryModification(..), modify_history, HistoricalRequest, Pos, Range(..), ARange, Anchor(..), BefAft(..), Edit(..), DualARange(..)) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception ()
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intercalate)
import Control.Monad.Error ()
import Text.ParserCombinators.Parsec (getInput, (<|>), oneOf, lookAhead, spaces, satisfy, CharParser, many1, parse)
import Util (Option(..), (.), (.∨.), total_tail, partitionMaybe)
import Prelude hiding (catch, (.))
import Prelude.Unicode

type Pos a = Int
  -- The 'a' phantom parameter denotes the element type for the position. This prevents accidental mix-ups of different kinds of positions.
data Range a = Range { start :: Pos a, size :: Int } deriving Eq

data BefAft = Before | After deriving (Eq, Ord)

data Anchor = Anchor { anchor_befAft :: BefAft, anchor_pos :: Pos Char } deriving Eq

type ARange = BefAft → Anchor

data DualARange = DualARange { full_range, replace_range :: ARange }
  -- In "void f(int i, double d);", the command "replace first parameter-declaration with char c" should produce "void f(char c, double d);", while the command "erase first parameter-declaration" should produce "void f(double d);". Hence, in the former, the clause "first parameter-declaration" should match "char c", while in the latter, it should match "char c, ". To accomodate this, our substring resolution functions return DualARanges containing both of these ranges.

data Edit
  = RangeReplaceEdit (Range Char) String
  | InsertEdit Anchor String
  | MoveEdit BefAft Int (Range Char)
    -- The Int is an offset. If it is a nonnegative number n, the insert position is n characters beyond the end of the source range. If it is a negative number -n, the insert position is n characters before the start of the source range. We use this instead of a normal Anchor because it ensures that silly "move into self"-edits are not representable. This constructor must not be used by anyone but the makeMoveEdit smart constructor, which detects such edits.
  | AddOptions [Request.EvalOpt]
  | RemoveOptions [Request.EvalOpt]
    deriving Eq
  -- We don't just use a RangeReplaceEdit with range length 0 for insertions, because it is not expressive enough. For instance, given "xy", insertions at the positions "after x" and "before y" would both designate position 1, but a prior "add z after x" edit should increment the latter position but not the former. InsertEdit's BefAft argument expresses this difference.

data EvalOpt = CompileOnly | Terse | NoWarn | NoUsingStd
  deriving (Eq, Enum, Bounded, Ord)

instance Option EvalOpt where
  short CompileOnly = Just 'c'
  short Terse = Just 't'
  short NoWarn = Just 'w'
  short NoUsingStd = Nothing
  long CompileOnly = "compile-only"
  long Terse = "terse"
  long NoWarn = "no-warn"
  long NoUsingStd = "no-using-std"

data EphemeralOpt = Resume | Help | Version deriving (Eq, Enum, Bounded)

instance Option EphemeralOpt where
  long Resume = "resume"; long Help = "help"; long Version = "version"
  short Resume = Just 'r'; short Help = Just 'h'; short Version = Just 'v'

type Nick = String

nickP :: CharParser st Nick
nickP = many1 $ satisfy $ isAlpha .∨. isDigit .∨. (∈ "[]\\`_^|}-")
  -- We don't include '{' because it messes up "geordi{...}", and no sane person would use it in a nick for a geordi bot anyway.

is_short_request :: String → Maybe String
is_short_request (dropWhile isSpace → s) = case s of
  '{' : s' | not $ all isSpace s' → Just s
    -- A '{' on a line of its own can occur as part of a small code fragments pasted in a channel. Of course, so can a '{' followed by more code on the same line, but for a '{' on a line of its own, we /know/ it's not intended for geordi.
  '<' : '<' : _ → Just s
  _ → Nothing

is_addressed_request :: String → Maybe (Nick, String)
is_addressed_request txt = either (const Nothing) Just (parse p "" txt)
  where
   p = do
    spaces
    nick ← nickP
    oneOf ":," <|> (spaces >> lookAhead (oneOf "<{-"))
    r ← getInput
    return (nick, r)

data Context = Context { request_history :: [HistoricalRequest] }

data EditableRequestKind = MakeType | Precedence | Evaluate (Set EvalOpt)
instance Show EditableRequestKind where
  show MakeType = "make type"
  show Precedence = "precedence"
  show (Evaluate s) = intercalate " " $ (if null shorts then id else (('-' : shorts) :) ) $ ("--"++) . long . longs
    where (longs, shorts) = partitionMaybe short (Set.elems s)

data EditableRequest = EditableRequest { kind :: EditableRequestKind, editable_body :: String }

type HistoricalRequest = (EditableRequest, Maybe Edit)

data HistoryModification = ReplaceLast HistoricalRequest | AddLast HistoricalRequest | DropLast

modify_history :: HistoryModification → Context → Context
modify_history m (Context l) = Context $ case m of
  ReplaceLast e → e : total_tail l
  AddLast e → e : l
  DropLast → total_tail l

data Response = Response
  { response_history_modification :: Maybe HistoryModification
  , response_output :: String }
