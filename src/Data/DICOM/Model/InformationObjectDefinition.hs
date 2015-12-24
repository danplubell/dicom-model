module Data.DICOM.Model.InformationObjectDefinition where

data InformationObjectDefinition = 
       CRImageIOD                 -- | Computed Radiography Image IOD
     | CTImageIOD                 -- | Computed Tomography Image IOD
     | MRImageIOD                 -- | Magnetic Resonance Image IOD
     | NMImageIOD                 -- | Nuclear Medicine Image IOD
     | USImageIOD                 -- | Ultrasound Image IOD
     | USMultiFrameImageIOD       -- | Ultrasound Multi-frame Image IOD 
     | SCImageIOD                 -- | Secondary Capture Image IOD
     | MFrameSbitSCImageIOD       -- | Multi-frame Single Bit Secondary Capture Image IOD
     | MFrameGbyteSCImageIOD      -- | Multi-frame Grayscale Byte Secondary Capture Image IOD
     | MFrameGWordSCImageIOD      -- | Multi-frame Grayscale Word Secondary Capture Image IOD
     | MFrameTrueColorSCImageIOD  -- | Multi-frame True Color Secondry Capture Image IOD
     | StandaloneOverlayIOD       -- | Standalone Overlay IOD
     | StandaloneCurveIOD         -- | Standalone Curve IOD
     | BasicStudyDescriptorIOD    -- | Basic Study Descriptor IOD
     | StandaloneModalityLutIOD   -- | Standalone Modality LUT IOD
     | StandaloneVoiLutIOD        -- | Standalone VOI LUT IOD
     | XRayAngioImageIOD          -- | XRay Angiographic Image IOD
     | XRayRFImageIOD             -- | XRay RF Image IOD
     | RTImageIOD                 -- | RT ImageIOD
     | RTDoseIOD                  -- | RT Dose IOD
     | RTStructureSetIOD          -- | RT Structure Set IOD
     | RTPlanIOD                  -- | RT Plan IOD
     | PETImageIOD                -- | Positron Emission Tomography Image IOD
     | StandalonePETCurveIOD      -- | Standalone PET Curve IOD
     | StoredPrintIOD             -- | Stored Print IOD
     | HCGrayscaleImageIOD        -- | Hardcopy Grayscale Image IOD
     | HCColorImageIOD            -- | Hardcopy Color Image IOD
     | DigXRayImageIOD            -- | Digital XRay Image IOD
     | DigMammoXRayImageIOD       -- | Digital Mammography XRay Image IOD
     | DigIntraOralXRayImageIOD   -- | Digital Intral-Oral XRay Image IOD
     | RTBeamsTreatmentRcdIOD     -- | RT Beams Treatment Record IOD
     | RTBrachyTreatmentRcdIOD    -- | RT Brachy Treatment Record IOD
     | RTTreatmentSummaryRcdIOD   -- | RT Treatment Summary Record IOD
     | VLEndoscopicImageIOD       -- | Visible Light Endoscopic Image IOD
     | VLMicroscopicImageIOD      -- | Visible Light Microscopic Image IOD
     | VLSlideCoordsMicroImageIOD -- | Visible Light Slide-coordinates Microscopic Image IOD
     | VLPhotoImageIOD            -- | Visible Light Photographic Image IOD
     | VideoEndoImageIOD          -- | Video Endoscopic Image IOD
     | VideoMicroImageIOD         -- | Video Microscopic Image IOD
     | VideoPhotoImageIOD         -- | Video Photographic Image IOD
     | VLWholeSlideMicroImageIOD  -- | Visible Light While Slide Microscopy Image IOD
     | GScaleSoftCpyPresStateIOD  -- | Gray Scale Softcopy Presenttion State IOD
     | ColorSoftCpyPresStateIOD   -- | Color Softcopy Presentation State IOD
     | PseudoClrSofCpyPreStateIOD -- | Pseudo-color Softcopy Presentation State IOD
     | BlendingSofCpyPreStateIOD  -- | Blending Softcopy Presentation State IOD
     | BasicStructuredDisplayIOD  -- | Basic Structured Display IOD
     | XARFGScaleSofCpyPreStIOD   -- | XA/RF Grayscale Softcopy Presentation State IOD
     | BasicVoiceAudioIOD         -- | Basic Voice Audio IOD
     | TwelveLeadEcgIOD           -- | 12 lead Electrocardiogram IOD
     | GeneralEcgIOD              -- | General Electrocardiogram IOD
     | AmbulatoryEcgIOD           -- | Ambulatory Electroardiogram IOD
     | HemodynamicIOD             -- | Hemodynamic IOD
     | BasicCardEpIOD             -- | Basic Cardiac Electrophysiology IOD
     | ArterialPulseWaveformIOD   -- | Arterial Pulse Waveform IOD
     | RespiratoryWaveformIOD     -- | Respiratory Waveform IOD
     | GeneralAudioWaveformIOD    -- | General Audio Waveform IOD
     | BasicTextSrIOD             -- | Basic Text SR IOD
     | EnhancedSrIOD              -- | Enhanced SR IOD
     | ComprehensiveSrIOD         -- | Comprehensive SR IOD
     | KeyObjectSelectionIOD      -- | Key Object Selection IOD
     | MammoCADSrIOD              -- | Mammography CAD SR IOD
     | ChestCADSrIOD              -- | Chest CAD SR IOD
     | ProcedureLogIOD            -- | Procedure Log IOD
     | XRayRadiationDoseSrIOD     -- | XRay Radiation Dose SR IOD
     | SpectaclePrescrRptIOD      -- | Spectacle Prescription Report IOD
     | ColonCadSrIOD              -- | Coloc CAD SR IOD
     | MacularGridThickVolRptIOD  -- | Macular Grid Thickness and Volume Report IOD
     | ImplantationPlanSrDocIOD   -- | Implantation Plan SR Document IOD
     | Comprehensive3dSrIOD       -- | Comprehensive 3D SR IOD
     | RadioPharmaRadDoseSrIOD    -- | Radiopharmaceutical Radiation Dose SR IOD
     | ExtensibleSrIOD            -- | Extensible SR IOD
     | MRSpectroscopyIOD          -- | MR Spectroscopy IOD
     | EnhancedMRImageIOD         -- | Enhanced MR Image IOD
     | EnhancedMRColorImageIOD    -- | Enhanced MR Color Image IOD
     | RawDataIOD                 -- | Raw Data IOD
     | EnhancedCTImageIOD         -- | Enhanced CT Image IOD
     | SpatialRegistrationIOD     -- | Spatial Registration IOD
     | DeformableSpatialRegIOD    -- | Deformable Spatial Registration IOD
     | SpatialFiducialsIOD        -- | Spatial Fiducials IOD
     | OphthalmicPhoto8BitImgIOD  -- | Ophthalmic Photography 8 Bit Image IOD
     | OphthalmicPhoto16BitImgIOD -- | Ophthalmic Photography 16 bit Image IOD
     | StereometricReltnIOD       -- | Sterometric Relationship IOD
     | HangingProtocolIOD         -- | Hanging Protocol IOD
     | EncapsulatedPdfIOD         -- | Encapsulated PDF IOD
     | EncapsulatedCdaIOD         -- | Encapsulated CDA IOD
     | RealWorldMappingIOD        -- | Real World Mapping IOD
     | EnhancedXRayAngioImageIOD  -- | Enhanced XRay Angiographic Image IOD
     | EnhancedXRayRFImageIOD     -- | Enhanced XRay Image IOD
     | RTIonPlanIOD               -- | RT Ion Plan IOD
     | RTIonBeamsTreatmentRcdIOD  -- | RT Ion Beams Treatment Record IOD
     | SegmentationIOD            -- | Segmentation IOD
     | OphthalmicTomoImgIOD       -- | Ophthalmic Tomography Image IOD
     | XRay3DAngioImgIOD          -- | Ophthalmic 3d Angiographic Image IOD
     | XRay3DCraniofacialImgIOD   -- | XRay 3D Craniofacial Image IOD
     | BreastTomoImageIOD         -- | Breast Tomosyntheses Image IOD
     | EnhancedPETImageIOD        -- | Enhanced PET Image IOD
     | SurfaceSegmentationIOD     -- | Surface Segmentation IOD
     | ColorPaletteIOD            -- | Color Palette IOD
     | EnhancedUSVolumeIOD        -- | Enhanced US Volume IOD
     | LensometryMeasurementsIOD  -- | Lensometry Measurements IOD
     | AutorefractionMsmtIOD      -- | Autorefraction Measurements IOD
     | KeratometryMsmtIOD         -- | Keratometry Measurements IOD
     | SubjectiveRefractMsmtIOD   -- | Subjective Refraction Measurements IOD
     | VisualAcuityMsmgIOD        -- | Visual Acuity Measurements IOD
     | OphthalmicAxialMsmtIOD     -- | Ophthalmic Axial Measurements IOD
     | IntraocularLensCalcIOD     -- | Intraocular Lens Calculations IOD
     | GenericImplantTempIOD      -- | Generic Implant Template IOD
     | ImplantAssemblyTempIOD     -- | Implant Assembly Template IOD
     | ImplanteTemplateGroupIOD   -- | Implant Template Group IOD
     | RTBeamsDeliveryInstructIOD -- | RT Beams Delivery Instructions
     | OphthaVisFieldStatPeriIOD  -- | Ophthalmic Visual Field Static Perimetry Measurements IOD
     | IntravascularOctIOD        -- | Intravascular OCT IOD
     | OphhalmicThicknessMapIOD   -- | Ophthalmic Thickness Map IOD
     | SurfaceScanMeshIOD         -- | Surface Scan Mesh IOD
     | SurfaceScanPointCloudIOD   -- | Surface Scan Point Cloud IOD
     | LegacyConvertEnhCtImageIOD -- | Legacy Converted Enhanced CT Image IOD
     | LegacyConvertEnhMrImageIOD -- | Legacy Converted Enhanced MR Image IOD
     | LegacyConvrtEnhPetImageIOD -- | Legacy Converted Enhanced PET Image IOD
     | CornealTopoMapIOD          -- | Corneal Topography Map IOD
     | BreastProjXRayImageIOD     -- | Breast Projection XRay Image IOD
     | ParametricMapIOD           -- | Parametric Map IOD
     | WdeFldOphPhotoStProjImgIOD -- | Wide Field Ophthalmic Photography Stereographic Projection Image IOD
     | WdeFldOphPhoto3dCordImgIOD -- | Wide Field Ophthalmic Photography 3D Coordinates Image IOD
     

