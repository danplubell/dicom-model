module Data.DICOM.Model.Parser where

import           Data.DICOM.Model.RegisteredUID

data EndianType = BigEndian | LittleEndian deriving (Show, Eq, Ord)
data VREncoding = Explicit | Implicit deriving (Show, Eq, Ord)
data TransferSyntax = TransferSyntax { tsEndianType :: EndianType
                                     , tsVREncoding :: VREncoding
                                     , tsDicomUID   :: DicomUID
                                     } deriving (Show,Eq,Ord)
