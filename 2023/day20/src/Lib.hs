{-# LANGUAGE RecordWildCards, TupleSections #-}

module Lib
    ( State(..)
    , doAction
    , initialState
    , lowTo
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
                   , singleLow :: Bool
                   } deriving Show

part1 :: [String] -> Int
part1 xs = lowPulses state * highPulses state
    where
        state = (!! 1000) $ iterate pressButton (initialState xs)

part2 :: [String] -> Int
part2 xs = product $ map (solve2 xs) [("cr", "km"), ("tk", "qs"), ("rt", "xj"), ("fv", "kz")]

solve2 :: [String] -> (String, String) -> Int
solve2 xs (node1, node2) = length . takeWhile (not . singleLow) . iterate (lowTo node1 node2) $ initialState xs

initialState :: [String] -> State
initialState xs = State 0 0 [] config False
    where
        config = parse xs

pressButton :: State -> State
pressButton state = head
                  . dropWhile (not . null . actions)
                  $ iterate (doAction "??") (state { actions = [(Low "button", "broadcaster")] })

lowTo :: String -> String -> State -> State
lowTo name to state = head
                      . dropWhile (not . null . actions)
                      $ iterate (doAction to) (state { actions = [(Low "broadcaster", name)] })

doAction :: String -> State -> State
doAction _ state@(State _ _ [] _ _) = state
doAction to State{..} =
    case (pulse, module') of
        (Low _, Output) -> State { lowPulses = lowPulses + 1
                               , actions = tail actions
                               , singleLow = singleLow || name == to
                               , ..
                               }
        (High _, Output) -> State { highPulses = highPulses + 1
                                , actions = tail actions
                                , ..
                                }
        (Low _, Broadcaster ds) -> State { lowPulses = lowPulses + 1
                                         , actions = tail actions ++ map (Low name,) ds
                                         , singleLow = singleLow || name == to
                                         , ..
                                         }
        (High _, Broadcaster ds) -> State { highPulses = highPulses + 1
                                          , actions = tail actions ++ map (High name,) ds
                                          , ..
                                          }
        (Low _, FlipFlop Off ds) -> State { lowPulses = lowPulses + 1
                                          , actions = tail actions ++ map (High name,) ds
                                          , configuration = Map.insert name (FlipFlop On ds) configuration
                                          , singleLow = singleLow || name == to
                                          , ..
                                          }
        (Low _, FlipFlop On ds) -> State { lowPulses = lowPulses + 1
                                         , actions = tail actions ++ map (Low name,) ds
                                         , configuration = Map.insert name (FlipFlop Off ds) configuration
                                         , singleLow = singleLow || name == to
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
                                            , singleLow = singleLow || name == to
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