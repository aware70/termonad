-- | Module    : Termonad.Options
--
-- This module exposes termonad's basic configuration options to the command line. These will
-- override the options that are set in your configuration file.
--
-- Currently displays values from the active config as default options.

{-# LANGUAGE ApplicativeDo #-}

module Termonad.Options where

import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import Control.Lens

import Termonad.Prelude
import Termonad.Lenses
import Termonad.Config

class HasOptions a where
  optionsHook :: a -> Parser a

instance HasOptions TMConfig where
  optionsHook :: TMConfig -> Parser TMConfig
  optionsHook = do
    builtInOptions <- traverseOf lensOptions parseConfigOptions 
    pure builtInOptions

-- | Simple reader for mapping on -> True etc.
readOnOff :: ReadM Bool
readOnOff = eitherReader $ \case
  "on" -> Right True
  "off" -> Right False
  _ -> Left ""

-- | FontFamily: [--font-family STRING]
parseFontFamily :: Text -> Parser Text
parseFontFamily defVal = strOption ( long "font-family"
                                  <> help "Set the font family"
                                  <> value defVal
                                  <> showDefault )

-- | FontSize: ([--font-size-points INT] | [--font-size-units DOUBLE])
parseFontSize :: FontSize -> Parser FontSize
parseFontSize defVal = optionPoints <|> optionUnits
  where optionPoints = option (FontSizePoints <$> auto) ( long "font-size-points"
                                                       <> help "Set the font size (points)"
                                                       <> value defVal
                                                       <> showDefault )
        optionUnits = option (FontSizeUnits <$> auto) ( long "font-size-units"
                                                     <> help "Set the font size (units)"
                                                     <> value defVal
                                                     <> showDefault )

parseFontConfig :: FontConfig -> Parser FontConfig
parseFontConfig (FontConfig family size) =
  FontConfig
    <$> parseFontFamily family
    <*> parseFontSize size

-- | ShowScrollbar: [--scrollbar (never | always | if-needed)]
parseShowScrollbar :: ShowScrollbar -> Parser ShowScrollbar
parseShowScrollbar defVal = option readScrollbar ( long "scrollbar"
                                             <> help "Scrollbar display mode"
                                             <> value defVal
                                             <> showDefault )
  where readScrollbar = eitherReader $ \case
          "never" -> Right ShowScrollbarNever
          "always" -> Right ShowScrollbarAlways
          "if-needed" -> Right ShowScrollbarIfNeeded
          _ -> Left ""

-- | ScrollbackLen: [--scrollback INT]
parseScrollbackLen :: Integer -> Parser Integer
parseScrollbackLen defVal = option auto ( long "scrollback"
                                       <> help "Set the scrollback length"
                                       <> value defVal
                                       <> showDefault )

-- | ConfirmExit: [--confirm-exit (off | on)]
parseConfirmExit :: Bool -> Parser Bool
parseConfirmExit defVal = option readOnOff ( long "confirm-exit"
                                          <> help "Ask for confirmation when closing Termonad."
                                          <> value defVal
                                          <> showDefault )

-- | WordCharExceptions: [--word-char-exceptions STRING]
parseWordCharExceptions :: Text -> Parser Text
parseWordCharExceptions defVal = strOption ( long "word-char-exceptions"
                                          <> help "Ask for confirmation when closing Termonad."
                                          <> value defVal
                                          <> showDefault )
-- | ShowMenu: [--menu (off | on)]
parseShowMenu :: Bool -> Parser Bool
parseShowMenu defVal = option readOnOff ( long "menu"
                                       <> help "Menu display mode"
                                       <> value defVal
                                       <> showDefault )

-- | ShowTabBar: [--tabbar (never | always | if-needed)]
parseShowTabBar :: ShowTabBar -> Parser ShowTabBar
parseShowTabBar defVal = option readTabBar ( long "tabbar"
                                          <> help "Tab bar display mode"
                                          <> value defVal
                                          <> showDefault )
  where readTabBar = eitherReader $ \case
          "never" -> Right ShowTabBarNever
          "always" -> Right ShowTabBarAlways
          "if-needed" -> Right ShowTabBarIfNeeded
          _ -> Left ""

parseCursorBlinkMode :: CursorBlinkMode -> Parser CursorBlinkMode
parseCursorBlinkMode defVal = option readCursorBlinkMode ( long "cursor-blink"
                                                        <> help "Cursor blink mode"
                                                        <> value defVal
                                                        <> showDefault )
  where readCursorBlinkMode = eitherReader $ \case
          "on" -> Right CursorBlinkModeOn
          "off" -> Right CursorBlinkModeOff
          _ -> Left ""

-- TODO: I don't like applying each parser to cfg to extract the default values.
-- There is probably a lensier way to do this.
parseConfigOptions :: ConfigOptions -> Parser ConfigOptions
parseConfigOptions cfg =
    ConfigOptions
      <$> parseFontConfig (cfg ^. lensFontConfig)
      <*> parseShowScrollbar (cfg ^. lensShowScrollbar)
      <*> parseScrollbackLen (cfg ^. lensScrollbackLen)
      <*> parseConfirmExit (cfg ^. lensConfirmExit)
      <*> parseWordCharExceptions (cfg ^. lensWordCharExceptions)
      <*> parseShowMenu (cfg ^. lensShowMenu)
      <*> parseShowTabBar (cfg ^. lensShowTabBar)
      <*> parseCursorBlinkMode (cfg ^. lensCursorBlinkMode)

parseConfigDir :: Parser (Maybe FilePath)
parseConfigDir = optional $ strOption ( long "config-dir"
                                     <> help "Look here for termonad.hs" )

runOptions :: Parser a -> IO a
runOptions p = execParser $ info (p <**> helper) desc
  where desc = ( fullDesc
              <> progDesc "Termonad"
              <> header "Termonad - a terminal emulator configurable in Haskell" )
