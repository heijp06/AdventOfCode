{-# LANGUAGE RecordWildCards, TupleSections #-}

module Lib
    ( State(..)
    , doAction
    , part1
    , part2
    , pressButton
    ) where

import qualified Data.Map as Map
import Modules

data State = State { lowPulses :: Int
                   , highPulses :: Int
                   , actions :: [(Pulse, String)]
                   , configuration :: Map.Map String Module
                   , rx :: Bool
                   } deriving Show

part1 :: [String] -> Int
part1 xs = lowPulses state * highPulses state
    where
        state = (!! 1000) . iterate pressButton $ State 0 0 [] config False
        config = parse xs

part2 :: [String] -> Int
part2 xs = length . takeWhile (not . rx) . iterate pressButton $ State 0 0 [] config False
    where
        config = parse xs

pressButton :: State -> State
pressButton state = head
                  . dropWhile (not . null . actions)
                  $ iterate doAction (state { actions = [(Low "button", "broadcaster")] })

doAction :: State -> State
doAction state@(State _ _ [] _ _) = state
doAction State{..} =
    case (pulse, module') of
        (Low _, Output) -> State { lowPulses = lowPulses + 1
                               , actions = tail actions
                               , rx = rx || name == "rx"
                               , ..
                               }
        (High _, Output) -> State { highPulses = highPulses + 1
                                , actions = tail actions
                                , ..
                                }
        (Low _, Broadcaster ds) -> State { lowPulses = lowPulses + 1
                                         , actions = tail actions ++ map (Low name,) ds
                                         , rx = rx || name == "rx"
                                         , ..
                                         }
        (High _, Broadcaster ds) -> State { highPulses = highPulses + 1
                                          , actions = tail actions ++ map (High name,) ds
                                          , ..
                                          }
        (Low _, FlipFlop Off ds) -> State { lowPulses = lowPulses + 1
                                          , actions = tail actions ++ map (High name,) ds
                                          , configuration = Map.insert name (FlipFlop On ds) configuration
                                          , rx = rx || name == "rx"
                                          , ..
                                          }
        (Low _, FlipFlop On ds) -> State { lowPulses = lowPulses + 1
                                         , actions = tail actions ++ map (Low name,) ds
                                         , configuration = Map.insert name (FlipFlop Off ds) configuration
                                         , rx = rx || name == "rx"
                                         , ..
                                         }
        (_, FlipFlop _ _) -> State { highPulses = highPulses + 1
                                   , actions = tail actions
                                   , ..
                                   }
        (Low _, Conjunction is ds) -> State { lowPulses = lowPulses + 1
                                            , actions = tail actions ++ map (High name,) ds
                                            , configuration =
                                                Map.insert name (Conjunction (replacePulse pulse is) ds) configuration
                                            , rx = rx || name == "rx"
                                            , ..
                                            }
        (_, Conjunction is ds) -> let
                                    is' = replacePulse pulse is
                                    allHigh = all isHighPulse is'
                                  in State { highPulses = highPulses + 1
                                           , actions = tail actions ++
                                                if allHigh then map (Low name,) ds else map (High name,) ds
                                           , configuration = Map.insert name (Conjunction is' ds) configuration
                                           , ..
                                           }
    where
        (pulse, name) = head actions
        module' = Map.findWithDefault Output name configuration

replacePulse :: Pulse -> [Pulse] -> [Pulse]
replacePulse p = map (\ p' -> if pulseSource p' == pulseSource p then p else p')

isHighPulse :: Pulse -> Bool
isHighPulse (Low _) = False
isHighPulse (High _) = True

pulseSource :: Pulse -> String
pulseSource (Low name) = name
pulseSource (High name) = name