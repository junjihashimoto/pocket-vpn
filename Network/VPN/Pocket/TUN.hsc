{-# LANGUAGE ForeignFunctionInterface #-}

module Network.VPN.Pocket.TUN (
  DeviceName
, TError(..)
, toTError
, tunOpen
, tunUp
, tunSetIp
, tunSetMask
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

tunUp :: DeviceName -> IO (Either TError ())
tunUp name = withCString name $ \name' -> do
  res <- c_tunUp name'
  if res >= 0
    then return $ Right ()
    else return $ Left $ toTError res

tunSetIp :: DeviceName -> Word32 -> IO (Either TError ())
tunSetIp name ip = withCString name $ \name' -> do
  res <- c_tunSetIp name' $ CUInt ip
  if res >= 0
    then return $ Right ()
    else return $ Left $ toTError res

tunSetMask :: DeviceName -> Word32 -> IO (Either TError ())
tunSetMask name mask = withCString name $ \name' -> do
  res <- c_tunSetMask name' $ CUInt mask
  if res >= 0
    then return $ Right ()
    else return $ Left $ toTError res


foreign import ccall safe "vpn_tun.h tun_open"
  c_tunOpen :: CString -> IO CInt

foreign import ccall safe "vpn_tun.h tun_up"
  c_tunUp :: CString -> IO CInt

foreign import ccall safe "vpn_tun.h tun_set_ip"
  c_tunSetIp :: CString -> CUInt -> IO CInt

foreign import ccall safe "vpn_tun.h tun_set_mask"
  c_tunSetMask :: CString -> CUInt -> IO CInt
