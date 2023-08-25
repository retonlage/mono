{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Qhello where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.IO
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Data.Fin.Base

import qualified Data.Text as T
-- hello.putStrLn
d_putStrLn_2 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.IO.T_IO_8
    () MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_putStrLn_2 = putStrLn . T.unpack
-- hello.Vec
d_Vec_6 a0 a1 = ()
data T_Vec_6 = C_'91''93'_10 | C__'8759'__18 AgdaAny T_Vec_6
-- hello.lookup
d_lookup_24 ::
  () ->
  Integer -> T_Vec_6 -> MAlonzo.Code.Data.Fin.Base.T_Fin_6 -> AgdaAny
d_lookup_24 ~v0 ~v1 v2 v3 = du_lookup_24 v2 v3
du_lookup_24 ::
  T_Vec_6 -> MAlonzo.Code.Data.Fin.Base.T_Fin_6 -> AgdaAny
du_lookup_24 v0 v1
  = case coe v0 of
      C__'8759'__18 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_10 -> coe v3
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v6
               -> coe du_lookup_24 (coe v4) (coe v6)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
