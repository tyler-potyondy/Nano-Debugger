{-# LANGUAGE CPP #-}
module Language.Nano.UI where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.State (modify)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import Text.Wrap (defaultWrapSettings, preserveIndentation)

import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  , strWrap
  )
import Brick.Util (fg, on)

-- ghci -package microlens -package microlens-mtl -package mtl -package vty -package vector programs/Debugger.hs

-- some sort of drop down

{--
env0 :: Nano.Env
env0 =  [ ("z1", (Nano.EIf (Nano.EBin Nano.Eq (Nano.EVar "z1") (Nano.EVar "x")) (Nano.EBin Nano.Le (Nano.EVar "y") (Nano.EVar "z")) (Nano.EBin Nano.Le (Nano.EVar "z") (Nano.EVar "y")))),
          ("z0", (Nano.VInt 0))
        ]
env0 =  [ ("z1", Nano.VInt 0)
        , ("x" , Nano.VInt 1)
        , ("y" , Nano.VInt 2)
        , ("z" , Nano.VInt 3)
        , ("z1", Nano.VInt 4)
        ]
env0' =
  [ ("c0", Nano.VInt 0)
  , ("c1", Nano.VInt 1)
  , ("c2", Nano.VInt 2)
  , ("c3", Nano.VInt 3)
  , ("c0", Nano.VInt 4)
  , ("c1", Nano.VInt 5)
  ]

env0Map = map (\(x,y) -> x ++  " " ++ show y) env0
env0'Map = map (\(x,y) -> x ++  " " ++ show y) env0'
--env1 = [env0Map,env0'Map]
--env1 = map (\(x,y) -> x ++  " " ++ show y) env0
env1 = env0
--}

finalEnv =   do
            inputEnv <- (Nano.execFileBrick "tests/input/t10.hs")
            let dispEnv = get2nd(snd (inputEnv))
            return dispEnv


drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total -- executing inside a closure?
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press enter to step through the environment."
                              , C.hCenter $ str "Press Esc to exit."
                              ]
get2nd (_,a,_) = a
pop :: [a] -> [a]
pop [] = []
pop xs = init xs

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () (Nano.Id, Nano.Value)) () -- suspend and return
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            els <- use L.listElementsL
            inputEnv <- liftIO finalEnv            
            let pos = Vec.length els
            if (pos < (length inputEnv)) 
              then modify $ L.listInsert pos ((reverse (inputEnv)) !! pos)
            else return ()
        V.EvKey V.KEsc [] -> M.halt

        ev -> L.handleListEvent ev
    
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (strWrap s)
                   else str s 
    in C.hCenter $ (selStr $ show a)

initialState :: L.List () (Nano.Id, Nano.Value)
initialState = L.list () (Vec.fromList []) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () (Nano.Id, Nano.Value)) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }



main :: IO ()
main = void $ M.defaultMain theApp initialState