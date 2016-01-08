module Data.DICOM.Model.Dictionary where
import Data.Word 
import qualified Data.ByteString as BS
import qualified Data.Map as DM

type DicomTag = (Word16, Word16)

type DicomDictionary = DM.Map DicomTag DictElement

data DictElement = DictElement { groupNbr :: Word16
                               , elementNbr:: Word16
                               , vr:: BS.ByteString
                               , name::BS.ByteString
                               , vm::BS.ByteString
                               , version::BS.ByteString
                               }
                               | Comment
                               | Skip deriving (Show, Eq)
