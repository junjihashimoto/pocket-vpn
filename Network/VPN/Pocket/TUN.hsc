{-# LANGUAGE ForeignFunctionInterface #-}

module Network.VPN.Pocket.TUN (
  DeviceName
, TError(..)
, toTError
, tunOpen
, tapOpen
, ifUp
, ifSetIp
, ifSetMask
) where

#include "vpn_tun.h"

import Foreign.C
import Foreign.Safe
import System.Posix.IO
import System.Posix.Types
import System.IO

type DeviceName = String

data TError =
    TOk
  | TOpenDevError
  | TSetTapError
  | TGetIfError
  | TSetIfError
  | TSetMaskError
  | TOtherError
  deriving (Eq, Show)

toTError :: CInt -> TError
toTError (#const T_OK)             = TOk
toTError (#const T_OPEN_DEV_ERROR) = TOpenDevError
toTError (#const T_SET_TAP_ERROR)  = TSetTapError
toTError (#const T_GET_IF_ERROR)   = TGetIfError
toTError (#const T_SET_IF_ERROR)   = TSetIfError
toTError (#const T_SET_MASK_ERROR) = TSetMaskError
toTError _                         = TOtherError

tunOpen :: DeviceName -> IO (Either TError Handle)
tunOpen name = withCString name $ \name' -> do
  res <- c_tunOpen name'
  if res >= 0
    then (fdToHandle $ Fd res) >>= return.Right
    else return $ Left $ toTError res

tapOpen :: DeviceName -> IO (Either TError Handle)
tapOpen name = withCString name $ \name' -> do
  res <- c_tapOpen name'
  if res >= 0
    then (fdToHandle $ Fd res) >>= return.Right
    else return $ Left $ toTError res

ifUp :: DeviceName -> IO (Either TError ())
ifUp name = withCString name $ \name' -> do
  res <- c_ifUp name'
  if res >= 0
    then return $ Right ()
    else return $ Left $ toTError res

ifSetIp :: DeviceName -> Word32 -> IO (Either TError ())
ifSetIp name ip = withCString name $ \name' -> do
  res <- c_ifSetIp name' $ CUInt ip
  if res >= 0
    then return $ Right ()
    else return $ Left $ toTError res

ifSetMask :: DeviceName -> Word32 -> IO (Either TError ())
ifSetMask name mask = withCString name $ \name' -> do
  res <- c_ifSetMask name' $ CUInt mask
  if res >= 0
    then return $ Right ()
    else return $ Left $ toTError res


foreign import ccall safe "vpn_tun.h tun_open"
  c_tunOpen :: CString -> IO CInt

foreign import ccall safe "vpn_tun.h tap_open"
  c_tapOpen :: CString -> IO CInt

foreign import ccall safe "vpn_tun.h if_up"
  c_ifUp :: CString -> IO CInt

foreign import ccall safe "vpn_tun.h if_set_ip"
  c_ifSetIp :: CString -> CUInt -> IO CInt

foreign import ccall safe "vpn_tun.h if_set_mask"
  c_ifSetMask :: CString -> CUInt -> IO CInt
