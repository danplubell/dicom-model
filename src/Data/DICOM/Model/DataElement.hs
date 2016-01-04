
module Data.DICOM.Model.DataElement where

import qualified Data.ByteString as BS
import           Data.Word

type GroupNbr = Word16
type ElementNbr = Word16
data DataElement = Element { deTag      :: (GroupNbr, ElementNbr)
                           , deVR       :: BS.ByteString
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
