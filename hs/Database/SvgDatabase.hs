module Database.SvgDatabase where

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist.Sqlite
import Control.Monad.Trans.Reader
import Database.Tables
import SvgParsing.Types
import SvgParsing.ParserUtil

-- | Inserts an ellipse entry into the rects table.
insertEllipse :: MonadIO m0 => Double -> Double -> Double -> Double -> String -> ReaderT SqlBackend m0 ()
insertEllipse xPos yPos rx ry stroke =
        insert_ $ Ellipses xPos
                           yPos
                           rx
                           ry
                           stroke

-- | Inserts a rect entry into the rects table.
insertRect :: MonadIO m0 => String -> Double -> Double -> Double -> Double -> Style -> ReaderT SqlBackend m0 ()
insertRect id_ width height xPos yPos style =
        insert_ $ Rects 1
                        id_
                        width
                        height
                        xPos
                        yPos
                        (fill style)
                        (stroke style)
                        (fill style == "#a14c3a")

-- | Inserts a text entry into the texts table.
insertText :: MonadIO m0 => String -> Double -> Double -> String -> Style -> ReaderT SqlBackend m0 ()
insertText id_ xPos yPos text style =
        insert_ $ Text 1
                       id_
                       xPos
                       yPos
                       text

-- | Inserts a tex entry into the texts table.
insertPath :: MonadIO m0 => [Point] -> Style -> Bool -> ReaderT SqlBackend m0 ()
insertPath d style isRegion =
        insert_ $ Paths d
                        (fill style)
                        (stroke style)
                        isRegion