{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Icing.Client
  ( Colour
  , Client(..)
  , darkPallete
  ) where

import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(..)
                                                , object
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Network.WebSockets             ( Connection )

-- | Represents a HEX colour
type Colour = Text

-- | Represents a single client
data Client = Client
  { clientName       :: Text
  , clientColour     :: Colour
  , clientConnection :: Connection
  }
  deriving stock Generic

-- | Does not print the connection details.
instance Show Client where
  show c =
    "Client { name = "
      <> show (clientName c)
      <> ", colour = "
      <> show (clientColour c)
      <> " }"

-- | Does not serialize connection, that would make no sense!
instance ToJSON Client where
  toJSON c =
    object ["clientName" .= clientName c, "clientColour" .= clientColour c]

-- | A custom dark pallete for connections. 
--
-- TODO: if overflow, generate a random Colour
darkPallete :: Set Colour
darkPallete = Set.fromList
  [ "#e31a1c" -- red
  , "#1f78b4" -- dark blue
  , "#33a02c" -- dark green
  , "#6a3d9a" -- dark purple
  , "#fdbf6f" -- light orange
  , "#a6cee3" -- light blue
  , "#b2df8a" -- light green
  , "#fb9a99" -- pink
  , "#ffff99" -- yellow
  , "#778899" -- slate gray
  , "#cab2d6" -- light purple
  , "#b15928" -- brown
  , "#d8973c" -- dark orange ish
  ]

