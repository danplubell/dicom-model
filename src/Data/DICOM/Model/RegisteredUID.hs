{-# LANGUAGE OverloadedStrings #-}
module Data.DICOM.Model.RegisteredUID
       (UIDDictionary
       , mkUIDDictionary
       , lookupUIDType
       , lookupUID
       , DicomUID (..)
       )
       where

import qualified Data.Bimap      as DB
import qualified Data.ByteString as DBS

type UIDDictionary = DB.Bimap DBS.ByteString DicomUID


mkUIDDictionary::UIDDictionary
mkUIDDictionary = DB.fromList dicomUIDTable

lookupUIDType::UIDDictionary -> DBS.ByteString -> DicomUID
lookupUIDType  = (DB.!)

lookupUID::UIDDictionary -> DicomUID -> DBS.ByteString
lookupUID = (DB.!>)


data DicomUID =  VerificationSOPClass
  | ImplicitVRLittleEndian
  | ExplicitVRLittleEndian
  | DeflatedExplicitVRLittleEndian
  | ExplicitVRBigEndian
  | JPEGBaselineP1
  | JPEGExtendedProcess24
  | JPEGExtendedProcess35Retired
  | JPEGSpectralSelectionNonHierarchicalProcess68Retired
  | JPEGSpectralSelectionNonHierarchicalProcess79Retired
  | JPEGFullProgressionNonHierarchicalProcess1012Retired
  | JPEGFullProgressionNonHierarchicalProcess1113Retired
  | JPEGLosslessNonHierarchicalProcess14
  | JPEGLosslessNonHierarchicalProcess15Retired
  | JPEGExtendedHierarchicalProcess1618Retired
  | JPEGExtendedHierarchicalProcess1719Retired
  | JPEGSpectralSelectionHierarchicalProcess2022Retired
  | JPEGSpectralSelectionHierarchicalProcess2123Retired
  | JPEGFullProgressionHierarchicalProcess2426Retired
  | JPEGFullProgressionHierarchicalProcess2527Retired
  | JPEGLosslessHierarchicalProcess28Retired
  | JPEGLosslessHierarchicalProcess29Retired
  | JPEGLosslessNonHierarchicalFirstOrderPredictionProcess14
  | JPEGLSLosslessImageCompression
  | JPEGLSLossyNearLosslessImageCompression
  | JPEG2000ImageCompressionLosslessOnly
  | JPEG2000ImageCompression
  | JPEG2000Part2MulticomponentImageCompressionLosslessOnly
  | JPEG2000Part2MulticomponentImageCompression
  | JPIPReferenced
  | JPIPReferencedDeflate
  | MPEG2MainProfileMainLevel
  | MPEG2MainProfileHighLevel
  | MPEG4AVCH264HighProfileLevel41
  | MPEG4AVCH264BDcompatibleHighProfileLevel41
  | RLELossless
  | RFC2557MIMEencapsulation
  | XMLEncoding
  | MediaStorageDirectoryStorage
  | TalairachBrainAtlasFrameofReference
  | SPM2T1FrameofReference
  | SPM2T2FrameofReference
  | SPM2PDFrameofReference
  | SPM2EPIFrameofReference
  | SPM2FILT1FrameofReference
  | SPM2PETFrameofReference
  | SPM2TRANSMFrameofReference
  | SPM2SPECTFrameofReference
  | SPM2GRAYFrameofReference
  | SPM2WHITEFrameofReference
  | SPM2CSFFrameofReference
  | SPM2BRAINMASKFrameofReference
  | SPM2AVG305T1FrameofReference
  | SPM2AVG152T1FrameofReference
  | SPM2AVG152T2FrameofReference
  | SPM2AVG152PDFrameofReference
  | SPM2SINGLESUBJT1FrameofReference
  | ICBM452T1FrameofReference
  | ICBMSingleSubjectMRIFrameofReference
  | HotIronColorPaletteSOPInstance
  | PETColorPaletteSOPInstance
  | HotMetalBlueColorPaletteSOPInstance
  | PET20StepColorPaletteSOPInstance
  | BasicStudyContentNotificationSOPClassRetired
  | StorageCommitmentPushModelSOPClass
  | StorageCommitmentPushModelSOPInstance
  | StorageCommitmentPullModelSOPClassRetired
  | StorageCommitmentPullModelSOPInstanceRetired
  | ProceduralEventLoggingSOPClass
  | ProceduralEventLoggingSOPInstance
  | SubstanceAdministrationLoggingSOPClass
  | SubstanceAdministrationLoggingSOPInstance
  | DICOMUIDRegistry
  | DICOMControlledTerminology
  | DICOMApplicationContextName
  | DetachedPatientManagementSOPClassRetired
  | DetachedPatientManagementMetaSOPClassRetired
  | DetachedVisitManagementSOPClassRetired
  | DetachedStudyManagementSOPClassRetired
  | StudyComponentManagementSOPClassRetired
  | ModalityPerformedProcedureStepSOPClass
  | ModalityPerformedProcedureStepRetrieveSOPClass
  | ModalityPerformedProcedureStepNotificationSOPClass
  | DetachedResultsManagementSOPClassRetired
  | DetachedResultsManagementMetaSOPClassRetired
  | DetachedStudyManagementMetaSOPClassRetired
  | DetachedInterpretationManagementSOPClassRetired
  | StorageServiceClass
  | BasicFilmSessionSOPClass
  | BasicFilmBoxSOPClass
  | BasicGrayscaleImageBoxSOPClass
  | BasicColorImageBoxSOPClass
  | ReferencedImageBoxSOPClassRetired
  | BasicGrayscalePrintManagementMetaSOPClass
  | ReferencedGrayscalePrintManagementMetaSOPClassRetired
  | PrintJobSOPClass
  | BasicAnnotationBoxSOPClass
  | PrinterSOPClass
  | PrinterConfigurationRetrievalSOPClass
  | PrinterSOPInstance
  | PrinterConfigurationRetrievalSOPInstance
  | BasicColorPrintManagementMetaSOPClass
  | ReferencedColorPrintManagementMetaSOPClassRetired
  | VOILUTBoxSOPClass
  | PresentationLUTSOPClass
  | ImageOverlayBoxSOPClassRetired
  | BasicPrintImageOverlayBoxSOPClassRetired
  | PrintQueueSOPInstanceRetired
  | PrintQueueManagementSOPClassRetired
  | StoredPrintStorageSOPClassRetired
  | HardcopyGrayscaleImageStorageSOPClassRetired
  | HardcopyColorImageStorageSOPClassRetired
  | PullPrintRequestSOPClassRetired
  | PullStoredPrintManagementMetaSOPClassRetired
  | MediaCreationManagementSOPClassUID
  | ComputedRadiographyImageStorage
  | DigitalXRayImageStorageForPresentation
  | DigitalXRayImageStorageForProcessing
  | DigitalMammographyXRayImageStorageForPresentation
  | DigitalMammographyXRayImageStorageForProcessing
  | DigitalIntraOralXRayImageStorageForPresentation
  | DigitalIntraOralXRayImageStorageForProcessing
  | CTImageStorage
  | EnhancedCTImageStorage
  | LegacyConvertedEnhancedCTImageStorage
  | UltrasoundMultiframeImageStorageRetired
  | UltrasoundMultiframeImageStorage
  | MRImageStorage
  | EnhancedMRImageStorage
  | MRSpectroscopyStorage
  | EnhancedMRColorImageStorage
  | LegacyConvertedEnhancedMRImageStorage
  | NuclearMedicineImageStorageRetired
  | UltrasoundImageStorageRetired
  | UltrasoundImageStorage
  | EnhancedUSVolumeStorage
  | SecondaryCaptureImageStorage
  | MultiframeSingleBitSecondaryCaptureImageStorage
  | MultiframeGrayscaleByteSecondaryCaptureImageStorage
  | MultiframeGrayscaleWordSecondaryCaptureImageStorage
  | MultiframeTrueColorSecondaryCaptureImageStorage
  | StandaloneOverlayStorageRetired
  | StandaloneCurveStorageRetired
  | WaveformStorageTrialRetired
  | ECG12leadECGWaveformStorage
  | GeneralECGWaveformStorage
  | AmbulatoryECGWaveformStorage
  | HemodynamicWaveformStorage
  | CardiacElectrophysiologyWaveformStorage
  | BasicVoiceAudioWaveformStorage
  | GeneralAudioWaveformStorage
  | ArterialPulseWaveformStorage
  | RespiratoryWaveformStorage
  | StandaloneModalityLUTStorageRetired
  | StandaloneVOILUTStorageRetired
  | GrayscaleSoftcopyPresentationStateStorageSOPClass
  | ColorSoftcopyPresentationStateStorageSOPClass
  | PseudoColorSoftcopyPresentationStateStorageSOPClass
  | BlendingSoftcopyPresentationStateStorageSOPClass
  | XAXRFGrayscaleSoftcopyPresentationStateStorage
  | XRayAngiographicImageStorage
  | EnhancedXAImageStorage
  | XRayRadiofluoroscopicImageStorage
  | EnhancedXRFImageStorage
  | XRayAngiographicBiPlaneImageStorageRetired
  | XRay3DAngiographicImageStorage
  | XRay3DCraniofacialImageStorage
  | BreastTomosynthesisImageStorage
  | IntravascularOpticalCoherenceTomographyImageStorageForPresentation
  | IntravascularOpticalCoherenceTomographyImageStorageForProcessing
  | NuclearMedicineImageStorage
  | RawDataStorage
  | SpatialRegistrationStorage
  | SpatialFiducialsStorage
  | DeformableSpatialRegistrationStorage
  | SegmentationStorage
  | SurfaceSegmentationStorage
  | RealWorldValueMappingStorage
  | SurfaceScanMeshStorage
  | SurfaceScanPointCloudStorage
  | VLImageStorageTrialRetired
  | VLMultiframeImageStorageTrialRetired
  | VLEndoscopicImageStorage
  | VideoEndoscopicImageStorage
  | VLMicroscopicImageStorage
  | VideoMicroscopicImageStorage
  | VLSlideCoordinatesMicroscopicImageStorage
  | VLPhotographicImageStorage
  | VideoPhotographicImageStorage
  | OphthalmicPhotography8BitImageStorage
  | OphthalmicPhotography16BitImageStorage
  | StereometricRelationshipStorage
  | OphthalmicTomographyImageStorage
  | VLWholeSlideMicroscopyImageStorage
  | LensometryMeasurementsStorage
  | AutorefractionMeasurementsStorage
  | KeratometryMeasurementsStorage
  | SubjectiveRefractionMeasurementsStorage
  | VisualAcuityMeasurementsStorage
  | SpectaclePrescriptionReportStorage
  | OphthalmicAxialMeasurementsStorage
  | IntraocularLensCalculationsStorage
  | MacularGridThicknessandVolumeReportStorage
  | OphthalmicVisualFieldStaticPerimetryMeasurementsStorage
  | OphthalmicThicknessMapStorage
  | CornealTopographyMapStorage
  | TextSRStorageTrialRetired
  | AudioSRStorageTrialRetired
  | DetailSRStorageTrialRetired
  | ComprehensiveSRStorageTrialRetired
  | BasicTextSRStorage
  | EnhancedSRStorage
  | ComprehensiveSRStorage
  | Comprehensive3DSRStorage
  | ProcedureLogStorage
  | MammographyCADSRStorage
  | KeyObjectSelectionDocumentStorage
  | ChestCADSRStorage
  | XRayRadiationDoseSRStorage
  | ColonCADSRStorage
  | ImplantationPlanSRStorage
  | EncapsulatedPDFStorage
  | EncapsulatedCDAStorage
  | PositronEmissionTomographyImageStorage
  | LegacyConvertedEnhancedPETImageStorage
  | StandalonePETCurveStorageRetired
  | EnhancedPETImageStorage
  | BasicStructuredDisplayStorage
  | RTImageStorage
  | RTDoseStorage
  | RTStructureSetStorage
  | RTBeamsTreatmentRecordStorage
  | RTPlanStorage
  | RTBrachyTreatmentRecordStorage
  | RTTreatmentSummaryRecordStorage
  | RTIonPlanStorage
  | RTIonBeamsTreatmentRecordStorage
  | DICOSCTImageStorage
  | DICOSDigitalXRayImageStorageForPresentation
  | DICOSDigitalXRayImageStorageForProcessing
  | DICOSThreatDetectionReportStorage
  | DICOS2DAITStorage
  | DICOS3DAITStorage
  | DICOSQuadrupoleResonanceQRStorage
  | EddyCurrentImageStorage
  | EddyCurrentMultiframeImageStorage
  | PatientRootQueryRetrieveInformationModelFIND
  | PatientRootQueryRetrieveInformationModelMOVE
  | PatientRootQueryRetrieveInformationModelGET
  | StudyRootQueryRetrieveInformationModelFIND
  | StudyRootQueryRetrieveInformationModelMOVE
  | StudyRootQueryRetrieveInformationModelGET
  | PatientStudyOnlyQueryRetrieveInformationModelFINDRetired
  | PatientStudyOnlyQueryRetrieveInformationModelMOVERetired
  | PatientStudyOnlyQueryRetrieveInformationModelGETRetired
  | CompositeInstanceRootRetrieveMOVE
  | CompositeInstanceRootRetrieveGET
  | CompositeInstanceRetrieveWithoutBulkDataGET
  | ModalityWorklistInformationModelFIND
  | GeneralPurposeWorklistInformationModelFINDRetired
  | GeneralPurposeScheduledProcedureStepSOPClassRetired
  | GeneralPurposePerformedProcedureStepSOPClassRetired
  | GeneralPurposeWorklistManagementMetaSOPClassRetired
  | InstanceAvailabilityNotificationSOPClass
  | RTBeamsDeliveryInstructionStorageTrialRetired
  | RTConventionalMachineVerificationTrialRetired
  | RTIonMachineVerificationTrialRetired
  | UnifiedWorklistandProcedureStepServiceClassTrialRetired
  | UnifiedProcedureStepPushSOPClassTrialRetired
  | UnifiedProcedureStepWatchSOPClassTrialRetired
  | UnifiedProcedureStepPullSOPClassTrialRetired
  | UnifiedProcedureStepEventSOPClassTrialRetired
  | UnifiedWorklistandProcedureStepSOPInstance
  | UnifiedWorklistandProcedureStepServiceClass
  | UnifiedProcedureStepPushSOPClass
  | UnifiedProcedureStepWatchSOPClass
  | UnifiedProcedureStepPullSOPClass
  | UnifiedProcedureStepEventSOPClass
  | RTBeamsDeliveryInstructionStorage
  | RTConventionalMachineVerification
  | RTIonMachineVerification
  | GeneralRelevantPatientInformationQuery
  | BreastImagingRelevantPatientInformationQuery
  | CardiacRelevantPatientInformationQuery
  | HangingProtocolStorage
  | HangingProtocolInformationModelFIND
  | HangingProtocolInformationModelMOVE
  | HangingProtocolInformationModelGET
  | ColorPaletteStorage
  | ColorPaletteInformationModelFIND
  | ColorPaletteInformationModelMOVE
  | ColorPaletteInformationModelGET
  | ProductCharacteristicsQuerySOPClass
  | SubstanceApprovalQuerySOPClass
  | GenericImplantTemplateStorage
  | GenericImplantTemplateInformationModelFIND
  | GenericImplantTemplateInformationModelMOVE
  | GenericImplantTemplateInformationModelGET
  | ImplantAssemblyTemplateStorage
  | ImplantAssemblyTemplateInformationModelFIND
  | ImplantAssemblyTemplateInformationModelMOVE
  | ImplantAssemblyTemplateInformationModelGET
  | ImplantTemplateGroupStorage
  | ImplantTemplateGroupInformationModelFIND
  | ImplantTemplateGroupInformationModelMOVE
  | ImplantTemplateGroupInformationModelGET
  | NativeDICOMModel
  | AbstractMultiDimensionalImageModel
  | DicomDeviceName
  | DicomDescription
  | DicomManufacturer
  | DicomManufacturerModelName
  | DicomSoftwareVersion
  | DicomVendorData
  | DicomAETitle
  | DicomNetworkConnectionReference
  | DicomApplicationCluster
  | DicomAssociationInitiator
  | DicomAssociationAcceptor
  | DicomHostname
  | DicomPort
  | DicomSOPClass
  | DicomTransferRole
  | DicomTransferSyntax
  | DicomPrimaryDeviceType
  | DicomRelatedDeviceReference
  | DicomPreferredCalledAETitle
  | DicomTLSCyphersuite
  | DicomAuthorizedNodeCertificateReference
  | DicomThisNodeCertificateReference
  | DicomInstalled
  | DicomStationName
  | DicomDeviceSerialNumber
  | DicomInstitutionName
  | DicomInstitutionAddress
  | DicomInstitutionDepartmentName
  | DicomIssuerOfPatientID
  | DicomPreferredCallingAETitle
  | DicomSupportedCharacterSet
  | DicomConfigurationRoot
  | DicomDevicesRoot
  | DicomUniqueAETitlesRegistryRoot
  | DicomDevice
  | DicomNetworkAE
  | DicomNetworkConnection
  | DicomUniqueAETitle
  | DicomTransferCapability
  | UniversalCoordinatedTime
  deriving (Show,Eq,Ord)

dicomUIDTable:: [(DBS.ByteString,DicomUID)]
dicomUIDTable =[("1.2.840.10008.1.1", VerificationSOPClass )
                ,("1.2.840.10008.1.2", ImplicitVRLittleEndian )
                ,("1.2.840.10008.1.2.1", ExplicitVRLittleEndian )
                ,("1.2.840.10008.1.2.1.99", DeflatedExplicitVRLittleEndian )
                ,("1.2.840.10008.1.2.2", ExplicitVRBigEndian )
                ,("1.2.840.10008.1.2.4.50", JPEGBaselineP1 )
                ,("1.2.840.10008.1.2.4.51", JPEGExtendedProcess24 )
                ,("1.2.840.10008.1.2.4.52", JPEGExtendedProcess35Retired )
                ,("1.2.840.10008.1.2.4.53", JPEGSpectralSelectionNonHierarchicalProcess68Retired )
                ,("1.2.840.10008.1.2.4.54", JPEGSpectralSelectionNonHierarchicalProcess79Retired )
                ,("1.2.840.10008.1.2.4.55", JPEGFullProgressionNonHierarchicalProcess1012Retired )
                ,("1.2.840.10008.1.2.4.56", JPEGFullProgressionNonHierarchicalProcess1113Retired )
                ,("1.2.840.10008.1.2.4.57", JPEGLosslessNonHierarchicalProcess14 )
                ,("1.2.840.10008.1.2.4.58", JPEGLosslessNonHierarchicalProcess15Retired )
                ,("1.2.840.10008.1.2.4.59", JPEGExtendedHierarchicalProcess1618Retired )
                ,("1.2.840.10008.1.2.4.60", JPEGExtendedHierarchicalProcess1719Retired )
                ,("1.2.840.10008.1.2.4.61", JPEGSpectralSelectionHierarchicalProcess2022Retired )
                ,("1.2.840.10008.1.2.4.62", JPEGSpectralSelectionHierarchicalProcess2123Retired )
                ,("1.2.840.10008.1.2.4.63", JPEGFullProgressionHierarchicalProcess2426Retired )
                ,("1.2.840.10008.1.2.4.64", JPEGFullProgressionHierarchicalProcess2527Retired )
                ,("1.2.840.10008.1.2.4.65", JPEGLosslessHierarchicalProcess28Retired )
                ,("1.2.840.10008.1.2.4.66", JPEGLosslessHierarchicalProcess29Retired )
                ,("1.2.840.10008.1.2.4.70", JPEGLosslessNonHierarchicalFirstOrderPredictionProcess14)
                ,("1.2.840.10008.1.2.4.80", JPEGLSLosslessImageCompression )
                ,("1.2.840.10008.1.2.4.81", JPEGLSLossyNearLosslessImageCompression )
                ,("1.2.840.10008.1.2.4.90", JPEG2000ImageCompressionLosslessOnly )
                ,("1.2.840.10008.1.2.4.91", JPEG2000ImageCompression )
                ,("1.2.840.10008.1.2.4.92", JPEG2000Part2MulticomponentImageCompressionLosslessOnly )
                ,("1.2.840.10008.1.2.4.93", JPEG2000Part2MulticomponentImageCompression )
                ,("1.2.840.10008.1.2.4.94", JPIPReferenced )
                ,("1.2.840.10008.1.2.4.95", JPIPReferencedDeflate )
                ,("1.2.840.10008.1.2.4.100", MPEG2MainProfileMainLevel )
                ,("1.2.840.10008.1.2.4.101", MPEG2MainProfileHighLevel )
                ,("1.2.840.10008.1.2.4.102", MPEG4AVCH264HighProfileLevel41 )
                ,("1.2.840.10008.1.2.4.103", MPEG4AVCH264BDcompatibleHighProfileLevel41 )
                ,("1.2.840.10008.1.2.5", RLELossless )
                ,("1.2.840.10008.1.2.6.1", RFC2557MIMEencapsulation )
                ,("1.2.840.10008.1.2.6.2", XMLEncoding )
                ,("1.2.840.10008.1.3.10", MediaStorageDirectoryStorage )
                ,("1.2.840.10008.1.4.1.1", TalairachBrainAtlasFrameofReference )
                ,("1.2.840.10008.1.4.1.2", SPM2T1FrameofReference )
                ,("1.2.840.10008.1.4.1.3", SPM2T2FrameofReference )
                ,("1.2.840.10008.1.4.1.4", SPM2PDFrameofReference )
                ,("1.2.840.10008.1.4.1.5", SPM2EPIFrameofReference )
                ,("1.2.840.10008.1.4.1.6", SPM2FILT1FrameofReference )
                ,("1.2.840.10008.1.4.1.7", SPM2PETFrameofReference )
                ,("1.2.840.10008.1.4.1.8", SPM2TRANSMFrameofReference )
                ,("1.2.840.10008.1.4.1.9", SPM2SPECTFrameofReference )
                ,("1.2.840.10008.1.4.1.10", SPM2GRAYFrameofReference )
                ,("1.2.840.10008.1.4.1.11", SPM2WHITEFrameofReference )
                ,("1.2.840.10008.1.4.1.12", SPM2CSFFrameofReference )
                ,("1.2.840.10008.1.4.1.13", SPM2BRAINMASKFrameofReference )
                ,("1.2.840.10008.1.4.1.14", SPM2AVG305T1FrameofReference )
                ,("1.2.840.10008.1.4.1.15", SPM2AVG152T1FrameofReference )
                ,("1.2.840.10008.1.4.1.16", SPM2AVG152T2FrameofReference )
                ,("1.2.840.10008.1.4.1.17", SPM2AVG152PDFrameofReference )
                ,("1.2.840.10008.1.4.1.18", SPM2SINGLESUBJT1FrameofReference )
                ,("1.2.840.10008.1.4.2.1", ICBM452T1FrameofReference )
                ,("1.2.840.10008.1.4.2.2", ICBMSingleSubjectMRIFrameofReference )
                ,("1.2.840.10008.1.5.1", HotIronColorPaletteSOPInstance )
                ,("1.2.840.10008.1.5.2", PETColorPaletteSOPInstance )
                ,("1.2.840.10008.1.5.3", HotMetalBlueColorPaletteSOPInstance )
                ,("1.2.840.10008.1.5.4", PET20StepColorPaletteSOPInstance )
                ,("1.2.840.10008.1.9", BasicStudyContentNotificationSOPClassRetired )
                ,("1.2.840.10008.1.20.1", StorageCommitmentPushModelSOPClass )
                ,("1.2.840.10008.1.20.1.1", StorageCommitmentPushModelSOPInstance )
                ,("1.2.840.10008.1.20.2", StorageCommitmentPullModelSOPClassRetired )
                ,("1.2.840.10008.1.20.2.1", StorageCommitmentPullModelSOPInstanceRetired )
                ,("1.2.840.10008.1.40", ProceduralEventLoggingSOPClass )
                ,("1.2.840.10008.1.40.1", ProceduralEventLoggingSOPInstance )
                ,("1.2.840.10008.1.42", SubstanceAdministrationLoggingSOPClass )
                ,("1.2.840.10008.1.42.1", SubstanceAdministrationLoggingSOPInstance )
                ,("1.2.840.10008.2.6.1", DICOMUIDRegistry )
                ,("1.2.840.10008.2.16.4", DICOMControlledTerminology )
                ,("1.2.840.10008.3.1.1.1", DICOMApplicationContextName )
                ,("1.2.840.10008.3.1.2.1.1", DetachedPatientManagementSOPClassRetired )
                ,("1.2.840.10008.3.1.2.1.4", DetachedPatientManagementMetaSOPClassRetired )
                ,("1.2.840.10008.3.1.2.2.1", DetachedVisitManagementSOPClassRetired )
                ,("1.2.840.10008.3.1.2.3.1", DetachedStudyManagementSOPClassRetired )
                ,("1.2.840.10008.3.1.2.3.2", StudyComponentManagementSOPClassRetired )
                ,("1.2.840.10008.3.1.2.3.3", ModalityPerformedProcedureStepSOPClass )
                ,("1.2.840.10008.3.1.2.3.4", ModalityPerformedProcedureStepRetrieveSOPClass )
                ,("1.2.840.10008.3.1.2.3.5", ModalityPerformedProcedureStepNotificationSOPClass )
                ,("1.2.840.10008.3.1.2.5.1", DetachedResultsManagementSOPClassRetired )
                ,("1.2.840.10008.3.1.2.5.4", DetachedResultsManagementMetaSOPClassRetired )
                ,("1.2.840.10008.3.1.2.5.5", DetachedStudyManagementMetaSOPClassRetired )
                ,("1.2.840.10008.3.1.2.6.1", DetachedInterpretationManagementSOPClassRetired )
                ,("1.2.840.10008.4.2", StorageServiceClass )
                ,("1.2.840.10008.5.1.1.1", BasicFilmSessionSOPClass )
                ,("1.2.840.10008.5.1.1.2", BasicFilmBoxSOPClass )
                ,("1.2.840.10008.5.1.1.4", BasicGrayscaleImageBoxSOPClass )
                ,("1.2.840.10008.5.1.1.4.1", BasicColorImageBoxSOPClass )
                ,("1.2.840.10008.5.1.1.4.2", ReferencedImageBoxSOPClassRetired )
                ,("1.2.840.10008.5.1.1.9", BasicGrayscalePrintManagementMetaSOPClass )
                ,("1.2.840.10008.5.1.1.9.1", ReferencedGrayscalePrintManagementMetaSOPClassRetired )
                ,("1.2.840.10008.5.1.1.14", PrintJobSOPClass )
                ,("1.2.840.10008.5.1.1.15", BasicAnnotationBoxSOPClass )
                ,("1.2.840.10008.5.1.1.16", PrinterSOPClass )
                ,("1.2.840.10008.5.1.1.16.376", PrinterConfigurationRetrievalSOPClass )
                ,("1.2.840.10008.5.1.1.17", PrinterSOPInstance )
                ,("1.2.840.10008.5.1.1.17.376", PrinterConfigurationRetrievalSOPInstance )
                ,("1.2.840.10008.5.1.1.18", BasicColorPrintManagementMetaSOPClass )
                ,("1.2.840.10008.5.1.1.18.1", ReferencedColorPrintManagementMetaSOPClassRetired )
                ,("1.2.840.10008.5.1.1.22", VOILUTBoxSOPClass )
                ,("1.2.840.10008.5.1.1.23", PresentationLUTSOPClass )
                ,("1.2.840.10008.5.1.1.24", ImageOverlayBoxSOPClassRetired )
                ,("1.2.840.10008.5.1.1.24.1", BasicPrintImageOverlayBoxSOPClassRetired )
                ,("1.2.840.10008.5.1.1.25", PrintQueueSOPInstanceRetired )
                ,("1.2.840.10008.5.1.1.26", PrintQueueManagementSOPClassRetired )
                ,("1.2.840.10008.5.1.1.27", StoredPrintStorageSOPClassRetired )
                ,("1.2.840.10008.5.1.1.29", HardcopyGrayscaleImageStorageSOPClassRetired )
                ,("1.2.840.10008.5.1.1.30", HardcopyColorImageStorageSOPClassRetired )
                ,("1.2.840.10008.5.1.1.31", PullPrintRequestSOPClassRetired )
                ,("1.2.840.10008.5.1.1.32", PullStoredPrintManagementMetaSOPClassRetired )
                ,("1.2.840.10008.5.1.1.33", MediaCreationManagementSOPClassUID )
                ,("1.2.840.10008.5.1.4.1.1.1", ComputedRadiographyImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.1.1", DigitalXRayImageStorageForPresentation )
                ,("1.2.840.10008.5.1.4.1.1.1.1.1", DigitalXRayImageStorageForProcessing )
                ,("1.2.840.10008.5.1.4.1.1.1.2", DigitalMammographyXRayImageStorageForPresentation )
                ,("1.2.840.10008.5.1.4.1.1.1.2.1", DigitalMammographyXRayImageStorageForProcessing )
                ,("1.2.840.10008.5.1.4.1.1.1.3", DigitalIntraOralXRayImageStorageForPresentation )
                ,("1.2.840.10008.5.1.4.1.1.1.3.1", DigitalIntraOralXRayImageStorageForProcessing )
                ,("1.2.840.10008.5.1.4.1.1.2", CTImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.2.1", EnhancedCTImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.2.2", LegacyConvertedEnhancedCTImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.3", UltrasoundMultiframeImageStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.3.1", UltrasoundMultiframeImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.4", MRImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.4.1", EnhancedMRImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.4.2", MRSpectroscopyStorage )
                ,("1.2.840.10008.5.1.4.1.1.4.3", EnhancedMRColorImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.4.4", LegacyConvertedEnhancedMRImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.5", NuclearMedicineImageStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.6", UltrasoundImageStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.6.1", UltrasoundImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.6.2", EnhancedUSVolumeStorage )
                ,("1.2.840.10008.5.1.4.1.1.7", SecondaryCaptureImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.7.1", MultiframeSingleBitSecondaryCaptureImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.7.2", MultiframeGrayscaleByteSecondaryCaptureImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.7.3", MultiframeGrayscaleWordSecondaryCaptureImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.7.4", MultiframeTrueColorSecondaryCaptureImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.8", StandaloneOverlayStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.9", StandaloneCurveStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.9.1", WaveformStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.9.1.1", ECG12leadECGWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.1.2", GeneralECGWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.1.3", AmbulatoryECGWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.2.1", HemodynamicWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.3.1", CardiacElectrophysiologyWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.4.1", BasicVoiceAudioWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.4.2", GeneralAudioWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.5.1", ArterialPulseWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.9.6.1", RespiratoryWaveformStorage )
                ,("1.2.840.10008.5.1.4.1.1.10", StandaloneModalityLUTStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.11", StandaloneVOILUTStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.11.1", GrayscaleSoftcopyPresentationStateStorageSOPClass )
                ,("1.2.840.10008.5.1.4.1.1.11.2", ColorSoftcopyPresentationStateStorageSOPClass )
                ,("1.2.840.10008.5.1.4.1.1.11.3", PseudoColorSoftcopyPresentationStateStorageSOPClass )
                ,("1.2.840.10008.5.1.4.1.1.11.4", BlendingSoftcopyPresentationStateStorageSOPClass )
                ,("1.2.840.10008.5.1.4.1.1.11.5", XAXRFGrayscaleSoftcopyPresentationStateStorage )
                ,("1.2.840.10008.5.1.4.1.1.12.1", XRayAngiographicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.12.1.1", EnhancedXAImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.12.2", XRayRadiofluoroscopicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.12.2.1", EnhancedXRFImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.12.3", XRayAngiographicBiPlaneImageStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.13.1.1", XRay3DAngiographicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.13.1.2", XRay3DCraniofacialImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.13.1.3", BreastTomosynthesisImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.14.1", IntravascularOpticalCoherenceTomographyImageStorageForPresentation )
                ,("1.2.840.10008.5.1.4.1.1.14.2", IntravascularOpticalCoherenceTomographyImageStorageForProcessing )
                ,("1.2.840.10008.5.1.4.1.1.20", NuclearMedicineImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.66", RawDataStorage )
                ,("1.2.840.10008.5.1.4.1.1.66.1", SpatialRegistrationStorage )
                ,("1.2.840.10008.5.1.4.1.1.66.2", SpatialFiducialsStorage )
                ,("1.2.840.10008.5.1.4.1.1.66.3", DeformableSpatialRegistrationStorage )
                ,("1.2.840.10008.5.1.4.1.1.66.4", SegmentationStorage )
                ,("1.2.840.10008.5.1.4.1.1.66.5", SurfaceSegmentationStorage )
                ,("1.2.840.10008.5.1.4.1.1.67", RealWorldValueMappingStorage )
                ,("1.2.840.10008.5.1.4.1.1.68.1", SurfaceScanMeshStorage )
                ,("1.2.840.10008.5.1.4.1.1.68.2", SurfaceScanPointCloudStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1", VLImageStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.77.2", VLMultiframeImageStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.77.1.1", VLEndoscopicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.1.1", VideoEndoscopicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.2", VLMicroscopicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.2.1", VideoMicroscopicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.3", VLSlideCoordinatesMicroscopicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.4", VLPhotographicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.4.1", VideoPhotographicImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.5.1", OphthalmicPhotography8BitImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.5.2", OphthalmicPhotography16BitImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.5.3", StereometricRelationshipStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.5.4", OphthalmicTomographyImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.77.1.6", VLWholeSlideMicroscopyImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.1", LensometryMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.2", AutorefractionMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.3", KeratometryMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.4", SubjectiveRefractionMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.5", VisualAcuityMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.6", SpectaclePrescriptionReportStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.7", OphthalmicAxialMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.78.8", IntraocularLensCalculationsStorage )
                ,("1.2.840.10008.5.1.4.1.1.79.1", MacularGridThicknessandVolumeReportStorage )
                ,("1.2.840.10008.5.1.4.1.1.80.1", OphthalmicVisualFieldStaticPerimetryMeasurementsStorage )
                ,("1.2.840.10008.5.1.4.1.1.81.1", OphthalmicThicknessMapStorage )
                ,("11.2.840.10008.5.1.4.1.1.82.1", CornealTopographyMapStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.1", TextSRStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.88.2", AudioSRStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.88.3", DetailSRStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.88.4", ComprehensiveSRStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.1.1.88.11", BasicTextSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.22", EnhancedSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.33", ComprehensiveSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.34", Comprehensive3DSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.40", ProcedureLogStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.50", MammographyCADSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.59", KeyObjectSelectionDocumentStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.65", ChestCADSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.67", XRayRadiationDoseSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.69", ColonCADSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.88.70", ImplantationPlanSRStorage )
                ,("1.2.840.10008.5.1.4.1.1.104.1", EncapsulatedPDFStorage )
                ,("1.2.840.10008.5.1.4.1.1.104.2", EncapsulatedCDAStorage )
                ,("1.2.840.10008.5.1.4.1.1.128", PositronEmissionTomographyImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.128.1", LegacyConvertedEnhancedPETImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.129", StandalonePETCurveStorageRetired )
                ,("1.2.840.10008.5.1.4.1.1.130", EnhancedPETImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.131", BasicStructuredDisplayStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.1", RTImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.2", RTDoseStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.3", RTStructureSetStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.4", RTBeamsTreatmentRecordStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.5", RTPlanStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.6", RTBrachyTreatmentRecordStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.7", RTTreatmentSummaryRecordStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.8", RTIonPlanStorage )
                ,("1.2.840.10008.5.1.4.1.1.481.9", RTIonBeamsTreatmentRecordStorage )
                ,("1.2.840.10008.5.1.4.1.1.501.1", DICOSCTImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.501.2.1", DICOSDigitalXRayImageStorageForPresentation )
                ,("1.2.840.10008.5.1.4.1.1.501.2.2", DICOSDigitalXRayImageStorageForProcessing )
                ,("1.2.840.10008.5.1.4.1.1.501.3", DICOSThreatDetectionReportStorage )
                ,("1.2.840.10008.5.1.4.1.1.501.4", DICOS2DAITStorage )
                ,("1.2.840.10008.5.1.4.1.1.501.5", DICOS3DAITStorage )
                ,("1.2.840.10008.5.1.4.1.1.501.6", DICOSQuadrupoleResonanceQRStorage )
                ,("1.2.840.10008.5.1.4.1.1.601.1", EddyCurrentImageStorage )
                ,("1.2.840.10008.5.1.4.1.1.601.2", EddyCurrentMultiframeImageStorage )
                ,("1.2.840.10008.5.1.4.1.2.1.1", PatientRootQueryRetrieveInformationModelFIND )
                ,("1.2.840.10008.5.1.4.1.2.1.2", PatientRootQueryRetrieveInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.1.2.1.3", PatientRootQueryRetrieveInformationModelGET )
                ,("1.2.840.10008.5.1.4.1.2.2.1", StudyRootQueryRetrieveInformationModelFIND )
                ,("1.2.840.10008.5.1.4.1.2.2.2", StudyRootQueryRetrieveInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.1.2.2.3", StudyRootQueryRetrieveInformationModelGET )
                ,("1.2.840.10008.5.1.4.1.2.3.1", PatientStudyOnlyQueryRetrieveInformationModelFINDRetired)
                ,("1.2.840.10008.5.1.4.1.2.3.2",PatientStudyOnlyQueryRetrieveInformationModelMOVERetired)
                ,("1.2.840.10008.5.1.4.1.2.3.3", PatientStudyOnlyQueryRetrieveInformationModelGETRetired )
                ,("1.2.840.10008.5.1.4.1.2.4.2", CompositeInstanceRootRetrieveMOVE )
                ,("1.2.840.10008.5.1.4.1.2.4.3", CompositeInstanceRootRetrieveGET )
                ,("1.2.840.10008.5.1.4.1.2.5.3", CompositeInstanceRetrieveWithoutBulkDataGET )
                ,("1.2.840.10008.5.1.4.31", ModalityWorklistInformationModelFIND )
                ,("1.2.840.10008.5.1.4.32.1", GeneralPurposeWorklistInformationModelFINDRetired )
                ,("1.2.840.10008.5.1.4.32.2", GeneralPurposeScheduledProcedureStepSOPClassRetired )
                ,("1.2.840.10008.5.1.4.32.3", GeneralPurposePerformedProcedureStepSOPClassRetired )
                ,("1.2.840.10008.5.1.4.32", GeneralPurposeWorklistManagementMetaSOPClassRetired )
                ,("1.2.840.10008.5.1.4.33", InstanceAvailabilityNotificationSOPClass )
                ,("1.2.840.10008.5.1.4.34.1", RTBeamsDeliveryInstructionStorageTrialRetired )
                ,("1.2.840.10008.5.1.4.34.2", RTConventionalMachineVerificationTrialRetired )
                ,("1.2.840.10008.5.1.4.34.3", RTIonMachineVerificationTrialRetired )
                ,("1.2.840.10008.5.1.4.34.4", UnifiedWorklistandProcedureStepServiceClassTrialRetired )
                ,("1.2.840.10008.5.1.4.34.4.1", UnifiedProcedureStepPushSOPClassTrialRetired )
                ,("1.2.840.10008.5.1.4.34.4.2", UnifiedProcedureStepWatchSOPClassTrialRetired )
                ,("1.2.840.10008.5.1.4.34.4.3", UnifiedProcedureStepPullSOPClassTrialRetired )
                ,("1.2.840.10008.5.1.4.34.4.4", UnifiedProcedureStepEventSOPClassTrialRetired )
                ,("1.2.840.10008.5.1.4.34.5", UnifiedWorklistandProcedureStepSOPInstance )
                ,("1.2.840.10008.5.1.4.34.6", UnifiedWorklistandProcedureStepServiceClass )
                ,("1.2.840.10008.5.1.4.34.6.1", UnifiedProcedureStepPushSOPClass )
                ,("1.2.840.10008.5.1.4.34.6.2", UnifiedProcedureStepWatchSOPClass )
                ,("1.2.840.10008.5.1.4.34.6.3", UnifiedProcedureStepPullSOPClass )
                ,("1.2.840.10008.5.1.4.34.6.4", UnifiedProcedureStepEventSOPClass )
                ,("1.2.840.10008.5.1.4.34.7", RTBeamsDeliveryInstructionStorage )
                ,("1.2.840.10008.5.1.4.34.8", RTConventionalMachineVerification )
                ,("1.2.840.10008.5.1.4.34.9", RTIonMachineVerification )
                ,("1.2.840.10008.5.1.4.37.1", GeneralRelevantPatientInformationQuery )
                ,("1.2.840.10008.5.1.4.37.2", BreastImagingRelevantPatientInformationQuery )
                ,("1.2.840.10008.5.1.4.37.3", CardiacRelevantPatientInformationQuery )
                ,("1.2.840.10008.5.1.4.38.1", HangingProtocolStorage )
                ,("1.2.840.10008.5.1.4.38.2", HangingProtocolInformationModelFIND )
                ,("1.2.840.10008.5.1.4.38.3", HangingProtocolInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.38.4", HangingProtocolInformationModelGET )
                ,("1.2.840.10008.5.1.4.39.1", ColorPaletteStorage )
                ,("1.2.840.10008.5.1.4.39.2", ColorPaletteInformationModelFIND )
                ,("1.2.840.10008.5.1.4.39.3", ColorPaletteInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.39.4", ColorPaletteInformationModelGET )
                ,("1.2.840.10008.5.1.4.41", ProductCharacteristicsQuerySOPClass )
                ,("1.2.840.10008.5.1.4.42", SubstanceApprovalQuerySOPClass )
                ,("1.2.840.10008.5.1.4.43.1", GenericImplantTemplateStorage )
                ,("1.2.840.10008.5.1.4.43.2", GenericImplantTemplateInformationModelFIND )
                ,("1.2.840.10008.5.1.4.43.3", GenericImplantTemplateInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.43.4", GenericImplantTemplateInformationModelGET )
                ,("1.2.840.10008.5.1.4.44.1", ImplantAssemblyTemplateStorage )
                ,("1.2.840.10008.5.1.4.44.2", ImplantAssemblyTemplateInformationModelFIND )
                ,("1.2.840.10008.5.1.4.44.3", ImplantAssemblyTemplateInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.44.4", ImplantAssemblyTemplateInformationModelGET )
                ,("1.2.840.10008.5.1.4.45.1", ImplantTemplateGroupStorage )
                ,("1.2.840.10008.5.1.4.45.2", ImplantTemplateGroupInformationModelFIND )
                ,("1.2.840.10008.5.1.4.45.3", ImplantTemplateGroupInformationModelMOVE )
                ,("1.2.840.10008.5.1.4.45.4", ImplantTemplateGroupInformationModelGET )
                ,("1.2.840.10008.7.1.1", NativeDICOMModel )
                ,("1.2.840.10008.7.1.2", AbstractMultiDimensionalImageModel )
                ,("1.2.840.10008.15.0.3.1", DicomDeviceName )
                ,("1.2.840.10008.15.0.3.2", DicomDescription )
                ,("1.2.840.10008.15.0.3.3", DicomManufacturer )
                ,("1.2.840.10008.15.0.3.4", DicomManufacturerModelName )
                ,("1.2.840.10008.15.0.3.5", DicomSoftwareVersion )
                ,("1.2.840.10008.15.0.3.6", DicomVendorData )
                ,("1.2.840.10008.15.0.3.7", DicomAETitle )
                ,("1.2.840.10008.15.0.3.8", DicomNetworkConnectionReference )
                ,("1.2.840.10008.15.0.3.9", DicomApplicationCluster )
                ,("1.2.840.10008.15.0.3.10", DicomAssociationInitiator )
                ,("1.2.840.10008.15.0.3.11", DicomAssociationAcceptor )
                ,("1.2.840.10008.15.0.3.12", DicomHostname )
                ,("1.2.840.10008.15.0.3.13", DicomPort )
                ,("1.2.840.10008.15.0.3.14", DicomSOPClass )
                ,("1.2.840.10008.15.0.3.15", DicomTransferRole )
                ,("1.2.840.10008.15.0.3.16", DicomTransferSyntax )
                ,("1.2.840.10008.15.0.3.17", DicomPrimaryDeviceType )
                ,("1.2.840.10008.15.0.3.18", DicomRelatedDeviceReference )
                ,("1.2.840.10008.15.0.3.19", DicomPreferredCalledAETitle )
                ,("1.2.840.10008.15.0.3.20", DicomTLSCyphersuite )
                ,("1.2.840.10008.15.0.3.21", DicomAuthorizedNodeCertificateReference )
                ,("1.2.840.10008.15.0.3.22", DicomThisNodeCertificateReference )
                ,("1.2.840.10008.15.0.3.23", DicomInstalled )
                ,("1.2.840.10008.15.0.3.24", DicomStationName )
                ,("1.2.840.10008.15.0.3.25", DicomDeviceSerialNumber )
                ,("1.2.840.10008.15.0.3.26", DicomInstitutionName )
                ,("1.2.840.10008.15.0.3.27", DicomInstitutionAddress )
                ,("1.2.840.10008.15.0.3.28", DicomInstitutionDepartmentName )
                ,("1.2.840.10008.15.0.3.29", DicomIssuerOfPatientID )
                ,("1.2.840.10008.15.0.3.30", DicomPreferredCallingAETitle )
                ,("1.2.840.10008.15.0.3.31", DicomSupportedCharacterSet )
                ,("1.2.840.10008.15.0.4.1", DicomConfigurationRoot )
                ,("1.2.840.10008.15.0.4.2", DicomDevicesRoot )
                ,("1.2.840.10008.15.0.4.3", DicomUniqueAETitlesRegistryRoot )
                ,("1.2.840.10008.15.0.4.4", DicomDevice )
                ,("1.2.840.10008.15.0.4.5", DicomNetworkAE )
                ,("1.2.840.10008.15.0.4.6", DicomNetworkConnection )
                ,("1.2.840.10008.15.0.4.7", DicomUniqueAETitle )
                ,("1.2.840.10008.15.0.4.8", DicomTransferCapability )
                ,("1.2.840.10008.15.1.1", UniversalCoordinatedTime )
                ]

