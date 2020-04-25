$benchnum  = '252';
$benchname = 'eon';
$exename   = 'eon';
$benchlang = 'CXX';
@base_exe  = ($exename);
$abstol   = 0.005;

@sources=qw(ggCoverageSolidTexture.cc ggPathDielectricMaterial.cc ggBox2.cc
	    ggBox3.cc ggRasterSurfaceTexture.cc ggJitterSample1.cc
	    ggNRooksSample2.cc ggJitterSample2.cc ggRGBFPixel.cc
	    ggRotatingPinholeCamera.cc ggRandomSample2.cc ggRandomSample3.cc
	    ggSolidNoise2.cc ggSolidNoise3.cc ggSolidTexture.cc
	    ggOakSolidTexture.cc ggSpecularMaterial.cc ggNA.cc ggMaterial.cc
	    ggDiffuseMaterial.cc ggDielectricMaterial.cc ggConductorMaterial.cc
	    ggEdgeDiscrepancy.cc ggPolishedMaterial.cc ggFrameInterpolation.cc
	    ggQuaternion.cc ggSpline.cc ggONB2.cc ggONB3.cc ggFrame2.cc
	    ggFrame3.cc ggHMatrix3.cc ggSphere.cc ggRGBE.cc ggGamma.cc
	    ggBRDF.cc ggDiffuseBRDF.cc ggPhongBRDF.cc ggPolishedBRDF.cc
	    ggCamera.cc ggPinholeCamera.cc ggThinLensCamera.cc ggErr.cc
	    ggPixelFilter.cc ggTrianglePixelFilter.cc ggBoxPixelFilter.cc
	    ggFormat.cc ggGeometry.cc ggHAffineMatrix3.cc ggHShearMatrix3.cc
	    ggHBoxMatrix3.cc ggHPerspectiveMatrix3.cc ggHTranslationMatrix3.cc
	    ggHRigidBodyMatrix3.cc ggHRotationMatrix3.cc ggHScaleMatrix3.cc
	    ggHPoint3.cc ggPoint2.cc ggPoint3.cc ggOptics.cc ggPlane.cc
	    ggPolygon.cc ggGrayPixel_x.cc ggRGBPixel_x.cc
	    ggRay2.cc ggRay3.cc ggBoardFloorSolidTexture.cc ggVector2.cc
	    ggVector3.cc ggString.cc ggFineSpectrum.cc ggSpectrum.cc eon.cc
	    eonImageCalculator.cc mrObjectRecord.cc mrCoarsePixelRenderer.cc
	    mrIndirectPixelRenderer.cc mrRushmeierPixelRenderer.cc
	    mrBruteForcePixelRenderer.cc mrKajiyaPixelRenderer.cc
	    mrCookPixelRenderer.cc mrGrid.cc mrScene.cc mrCamera.cc
	    mrTriangle.cc mrFastTriangle.cc mrPolygon.cc mrXYDisk.cc
	    mrXZDisk.cc mrYZDisk.cc mrPhongAreaXYRectangleLuminaire.cc
	    mrPhongAreaXZRectangleLuminaire.cc
	    mrPhongAreaYZRectangleLuminaire.cc mrEmitter.cc mrImposter.cc
	    mrLinkedObjects.cc mrBox.cc mrXCylinder.cc mrYCylinder.cc
	    mrZCylinder.cc mrDiffuseAreaZCylinderLuminaire.cc
	    mrDiffuseCosineZCylinderLuminaire.cc
	    mrDiffuseVisibleAreaZCylinderLuminaire.cc mrXEllipticalCylinder.cc
	    mrYEllipticalCylinder.cc mrZEllipticalCylinder.cc
	    mrSpotAreaXYDiskLuminaire.cc mrDiffuseAreaXYRectangleLuminaire.cc
	    mrDiffuseAreaXZRectangleLuminaire.cc
	    mrDiffuseAreaYZRectangleLuminaire.cc mrSurfaceList.cc
	    mrDiffuseAreaTriangleLuminaire.cc mrDiffuseAreaSphereLuminaire.cc
	    mrDiffuseCosineSphereLuminaire.cc
	    mrDiffuseSolidAngleSphereLuminaire.cc mrShellLuminaire.cc
	    mrInstance.cc mrMaterial.cc mrPhongAreaTriangleLuminaire.cc
	    mrSolidTexture.cc mrSphere.cc mrSurface.cc mrSurfaceTexture.cc
	    mrXYRectangle.cc mrXZRectangle.cc mrYZRectangle.cc myrand.cc);
$bench_flags='-I. -DNDEBUG';

sub invoke {
    my ($me) = @_;
    my @rc;

    my @kajiya_flags=('chair.control.kajiya', 'chair.camera', 'chair.surfaces', 'chair.kajiya.ppm', 'ppm', 'pixels_out.kajiya' );
    my @cook_flags=('chair.control.cook', 'chair.camera', 'chair.surfaces', 'chair.cook.ppm', 'ppm', 'pixels_out.cook' );
    my @rushmeier_flags=('chair.control.rushmeier', 'chair.camera', 'chair.surfaces', 'chair.rushmeier.ppm', 'ppm', 'pixels_out.rushmeier' );

    push (@rc, { 'command' => $me->exe_file,
                    'args'    => [ @cook_flags ],
                    'output'  => 'cook_log.out',
                    'error'   => 'cook_log.err',
                });
    push (@rc, { 'command' => $me->exe_file,
                    'args'    => [ @rushmeier_flags ],
                    'output'  => 'rushmeier_log.out',
                    'error'   => 'rushmeier_log.err',
                });
    push (@rc, { 'command' => $me->exe_file,
                    'args'    => [ @kajiya_flags ],
                    'output'  => 'kajiya_log.out',
                    'error'   => 'kajiya_log.err',
                });


    return @rc;
}

