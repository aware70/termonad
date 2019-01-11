-- | Module    : Termonad.Options
--
-- This module exposes termonad's basic configuration options to the command line. These will
-- override the options that are set in your configuration file.
--
-- Currently displays values from the active config as default options.

{-# LANGUAGE ApplicativeDo #-}
module Termonad.Options ( TermonadOptions(..), termonadOptions ) where


import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import Control.Lens

import Termonad.Prelude
import Termonad.Lenses
import Termonad.Config ( ConfigOptions(..)
                       , ShowScrollbar(..)
                       , ShowTabBar(..)
                       , CursorBlinkMode(..)
                       , FontSize(..)
                       )
type ModifyConfigOptions = ConfigOptions -> ConfigOptions
data TermonadOptions = TermonadOptions { modConfigOptions :: ModifyConfigOptions
                                       , dyreCfgDir :: Maybe FilePath
                                       }

-- | Simple reader for mapping on -> True etc.
readOnOff :: ReadM Bool
readOnOff = eitherReader $ \case
  "on" -> Right True
  "off" -> Right False
  _ -> Left ""

-- | FontFamily: [--font-family STRING]
parseFontFamily :: ConfigOptions -> Parser Text
parseFontFamily cfg = strOption ( long "font-family"
                               <> help "Set the font family"
                               <> value (cfg ^. lensFontConfig . lensFontFamily)
                               <> showDefault )

-- | FontSize: ([--font-size-points INT] | [--font-size-units DOUBLE])
parseFontSize :: ConfigOptions -> Parser FontSize
parseFontSize cfg = optionPoints <|> optionUnits
  where optionPoints = option (FontSizePoints <$> auto) ( long "font-size-points"
                                                       <> help "Set the font size (points)"
                                                       <> value (cfg ^. lensFontConfig . lensFontSize)
                                                       <> showDefault )
        optionUnits = option (FontSizeUnits <$> auto) ( long "font-size-units"
                                                     <> help "Set the font size (units)"
                                                     <> value (cfg ^. lensFontConfig . lensFontSize)
                                                     <> showDefault )
-- | ShowScrollbar: [--scrollbar (never | always | if-needed)]
parseShowScrollbar :: ConfigOptions -> Parser ShowScrollbar
parseShowScrollbar cfg = option readScrollbar ( long "scrollbar"
                                             <> help "Scrollbar display mode"
                                             <> value (cfg ^. lensShowScrollbar)
                                             <> showDefault )
  where readScrollbar = eitherReader $ \case
          "never" -> Right ShowScrollbarNever
          "always" -> Right ShowScrollbarAlways
          "if-needed" -> Right ShowScrollbarIfNeeded
          _ -> Left ""

-- | ScrollbackLen: [--scrollback INT]
parseScrollbackLen :: ConfigOptions -> Parser Integer
parseScrollbackLen cfg = option auto ( long "scrollback"
                                    <> help "Set the scrollback length"
                                    <> value (cfg ^. lensScrollbackLen)
                                    <> showDefault )

-- | ConfirmExit: [--confirm-exit (off | on)]
parseConfirmExit :: ConfigOptions -> Parser Bool
parseConfirmExit cfg = option readOnOff ( long "confirm-exit"
                                       <> help "Ask for confirmation when closing Termonad."
                                       <> value (cfg ^. lensConfirmExit)
                                       <> showDefault )

-- | WordCharExceptions: [--word-char-exceptions STRING]
parseWordCharExceptions :: ConfigOptions -> Parser Text
parseWordCharExceptions cfg = strOption ( long "word-char-exceptions"
                                      <> help "Ask for confirmation when closing Termonad."
                                      <> value (cfg ^. lensWordCharExceptions)
                                      <> showDefault )
-- | ShowMenu: [--menu (off | on)]
parseShowMenu :: ConfigOptions -> Parser Bool
parseShowMenu cfg = option readOnOff ( long "menu"
                                    <> help "Menu display mode"
                                    <> value (cfg ^. lensShowMenu)
                                    <> showDefault )

-- | ShowTabBar: [--tabbar (never | always | if-needed)]
parseShowTabBar :: ConfigOptions -> Parser ShowTabBar
parseShowTabBar cfg = option readTabBar ( long "tabbar"
                                       <> help "Tab bar display mode"
                                       <> value (cfg ^. lensShowTabBar)
                                       <> showDefault )
  where readTabBar = eitherReader $ \case
          "never" -> Right ShowTabBarNever
          "always" -> Right ShowTabBarAlways
          "if-needed" -> Right ShowTabBarIfNeeded
          _ -> Left ""

-- | CursorBlinkMode: [--cursor-blink (off | on)]
parseCursorBlinkMode :: ConfigOptions -> Parser CursorBlinkMode
parseCursorBlinkMode cfg = option readCursorBlinkMode ( long "cursor-blink"
                                                        <> help "Cursor blink mode"
                                                        <> value (cfg ^. lensCursorBlinkMode)
                                                        <> showDefault )

  where readCursorBlinkMode = eitherReader $ \case
          "on" -> Right CursorBlinkModeOn
          "off" -> Right CursorBlinkModeOff
          _ -> Left ""

-- | TODO: Don't like applying each parser to cfg to extract the default values.
parseModifyConfigOptions :: ConfigOptions -> Parser ModifyConfigOptions
parseModifyConfigOptions cfg = do
    fontFamily <- set (lensFontConfig . lensFontFamily) <$> parseFontFamily cfg
    fontSize <- set (lensFontConfig . lensFontSize) <$> parseFontSize cfg
    showScrollbar <- set lensShowScrollbar <$> parseShowScrollbar cfg
    scrollbackLen <- set lensScrollbackLen <$> parseScrollbackLen cfg
    confirmExit <- set lensConfirmExit <$> parseConfirmExit cfg
    wordCharExceptions <- set lensWordCharExceptions <$> parseWordCharExceptions cfg
    showMenu <- set lensShowMenu <$> parseShowMenu cfg
    showTabBar <- set lensShowTabBar <$> parseShowTabBar cfg
    cursorBlinkMode <- set lensCursorBlinkMode <$> parseCursorBlinkMode cfg
    pure $ fontFamily
         . fontSize
         . showScrollbar
         . scrollbackLen
         . confirmExit
         . wordCharExceptions
         . showMenu
         . showTabBar
         . cursorBlinkMode

parseConfigDir :: Parser (Maybe FilePath)
parseConfigDir = optional $ strOption ( long "config-dir"
                                     <> help "Look here for termonad.hs" )

parseOverrides :: ConfigOptions -> ParserInfo TermonadOptions
parseOverrides cfg =
  let overrides = TermonadOptions <$> parseModifyConfigOptions cfg <*> parseConfigDir
   in info (overrides <**> helper)
        ( fullDesc
       <> progDesc "Termonad"
       <> header "Termonad - a terminal emulator configurable in Haskell" )

termonadOptions :: ConfigOptions -> IO TermonadOptions
termonadOptions = execParser . parseOverrides


