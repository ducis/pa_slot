{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
import Syntax.Slot
a = [s| ı : ı : _ı : ı : _ı : _ı : _0 : [] |] 'a' 'b' 'c'
main = print a

