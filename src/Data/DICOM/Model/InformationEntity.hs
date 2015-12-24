module Data.DICOM.Model.InformationEntity where

data InformationEntity = PatientIE
                       | StudyIE
                       | SeriesIE
                       | EquipmentIE
                       | FrameOfReferenceIE
                       | ImageIE
                       | OverlayIE
                       | CurveIE
                       | ModalityLutIE
                       | VoiLutIE
                       | PresentationStateIE
                       | WaveFormIE
                       | SRDocumentIE
                       | MRSpectroscopyIE
                       | RawDataIE
                       | EncapsulatedDocumentIE
                       | RealWorldValueMappingIE
                       | SurfaceIE
                       | MeasurementsIE


