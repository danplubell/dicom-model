module Data.DICOM.Model.DataElement where

import Data.Word 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

type GroupNbr = Word16
type ElementNbr = Word16
data DataElement = DataElement { tag      :: (GroupNbr, ElementNbr) 
                               , vr       :: VR
                               , vl       :: Word32
                               , rawValue :: BS.ByteString
                               } deriving (Show) 
data VR = AE -- | Application Entity
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
        deriving (Show)
