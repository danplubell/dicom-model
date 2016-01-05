{-# LANGUAGE OverloadedStrings #-}
module Data.DICOM.Model.DataElement where

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import           Data.Word
import           Prelude         hiding (LT)

type GroupNbr = Word16
type ElementNbr = Word16
data DataElement = Element { deTag      :: (GroupNbr, ElementNbr)
                           , deVR       :: VR
                           , deVL       :: Word32
                           , deRawValue :: BS.ByteString
                           }
                 | Item    { diTag      :: (GroupNbr, ElementNbr)
                           , diVL       :: Word32
                           , diRawValue :: BS.ByteString
                           } deriving (Show, Eq, Ord)
data FileMetaInformation = FileMetaInformation
  { metaLength            ::DataElement
  , metaInfoVersion       ::DataElement
  , mediaSOP              ::DataElement
  , mediaSOPInstance      ::DataElement
  , transferSyntaxUID     ::DataElement
  , implClassUID          ::DataElement
  , implVersion           ::DataElement
  , sourceAETitle         ::DataElement
  , sendAETitle           ::DataElement
  , receiveAETitle        ::DataElement
  , privateInfoCreatorUID ::DataElement
  , privateInformation    ::DataElement
  } deriving (Show, Eq, Ord)


data DicomValue a =
      ULVal Word32
    | UIVal T.Text
    | OBVal BS.ByteString
    | RawVal BS.ByteString
    deriving (Show, Ord, Eq)

class VRType a where
  fromVR  ::  a -> BS.ByteString
  toVR  :: BS.ByteString -> a

data VR =
    AE -- | Application Entity
  | AS -- | Age String
  | AT -- | Attribute Tag
  | CS -- | Code String
  | DA -- | Date
  | DS -- | Decimal String
  | DT -- | DateTime
  | FL -- | Floating Point Single
  | FD -- | Floating Point Double
  | IS -- | Integer String
  | LO -- | Long String
  | LT -- | Long Text
  | OB -- | Other Byte String
  | OD -- | Other Double String
  | OF -- | Other Float String
  | OW -- | Other Word String
  | PN -- | Person Name
  | SH -- | Short String
  | SL -- | Signed Long
  | SQ -- | Sequence of Items
  | SS -- | Signed Short
  | ST -- | Short Text
  | TM -- | Time
  | UC -- | Unlimited Characters
  | UI -- | Unique Identifier (UID)
  | UL -- | Unsigned Long
  | UN -- | Unknown
  | UR -- | Universal Resource Identifier or Universal Resource Location
  | US -- | Unsigned binary integer
  | UT -- | Unlimited Text
  deriving (Show,Eq,Ord)

instance VRType VR where
  fromVR AE = "AE"
  fromVR AS = "AS"
  fromVR AT = "AT"
  fromVR CS = "CS"
  fromVR DA = "DA"
  fromVR DS = "DS"
  fromVR DT = "DT"
  fromVR FL = "FL"
  fromVR FD = "FD"
  fromVR IS = "IS"
  fromVR LO = "LO"
  fromVR LT = "LT"
  fromVR OB = "OB"
  fromVR OD = "OD"
  fromVR OF = "OF"
  fromVR OW = "OW"
  fromVR PN = "PN"
  fromVR SH = "SH"
  fromVR SL = "SL"
  fromVR SQ = "SQ"
  fromVR SS = "SS"
  fromVR ST = "ST"
  fromVR TM = "TM"
  fromVR UC = "UC"
  fromVR UI = "UI"
  fromVR UL = "UL"
  fromVR UN = "UN"
  fromVR UR = "UR"
  fromVR US = "US"
  fromVR UT = "UT"
  toVR "AE" = AE
  toVR "AS" = AS
  toVR "AT" = AT
  toVR "CS" = CS
  toVR "DA" = DA
  toVR "DS" = DS
  toVR "DT" = DT
  toVR "FL" = FL
  toVR "FD" = FD
  toVR "IS" = IS
  toVR "LO" = LO
  toVR "LT" = LT
  toVR "OB" = OB
  toVR "OD" = OD
  toVR "OF" = OF
  toVR "OW" = OW
  toVR "PN" = PN
  toVR "SH" = SH
  toVR "SL" = SL
  toVR "SQ" = SQ
  toVR "SS" = SS
  toVR "ST" = ST
  toVR "TM" = TM
  toVR "UC" = UC
  toVR "UI" = UI
  toVR "UL" = UL
  toVR "UN" = UN
  toVR "UR" = UR
  toVR "US" = US
  toVR "UT" = UT
  toVR _    = UN

