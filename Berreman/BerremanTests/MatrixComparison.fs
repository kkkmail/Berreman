﻿namespace BerremanTests

module MatrixComparison =
    open System.Numerics
    open Berreman.MathNetNumericsMath
    open Berreman.Geometry
    open Berreman.MaterialProperties
    open Berreman.Fields
    open Xunit
    open Xunit.Abstractions


    let allowedDiff = 1.0e-05


    let outputData (output : ITestOutputHelper) msg result expected = 
        output.WriteLine ("{0}", msg.ToString())
        output.WriteLine ("result = {0}", result.ToString())
        output.WriteLine ("expected = {0}", expected.ToString())


    let verifyMatrixEquality (output : ITestOutputHelper) (ComplexMatrix result) (ComplexMatrix expected) =
        outputData output "" result expected

        let diff = result - expected
        let norm = expected.L2Norm ()
        let diffNorm = diff.L2Norm ()

        output.WriteLine ("diff = {0}", diff.ToString())

        output.WriteLine ("norm = {0}", norm)
        output.WriteLine ("diffValue = {0}", diffNorm)
        Assert.True(diffNorm / norm < allowedDiff)

    let verifyMatrixEqualityEps o (Eps (ComplexMatrix3x3 r)) (Eps (ComplexMatrix3x3 e)) = verifyMatrixEquality o r e


    let verifyComplexVectorEquality (output : ITestOutputHelper) (msg : string) (ComplexVector3 (ComplexVector result)) (ComplexVector3 (ComplexVector expected)) =
        outputData output msg result expected

        let diff = result - expected
        let norm = expected.L2Norm ()
        let diffNorm = diff.L2Norm ()

        output.WriteLine ("diff = {0}", diff.ToString())

        output.WriteLine ("norm = {0}", norm)
        output.WriteLine ("diffValue = {0}", diffNorm)
        Assert.True(diffNorm / norm < allowedDiff)


    let verifyRealVectorEquality (output : ITestOutputHelper) (msg : string) (RealVector result) (RealVector expected) =
        outputData output msg result expected

        let diff = result - expected
        let norm = expected.L2Norm ()
        let diffNorm = diff.L2Norm ()

        output.WriteLine ("diff = {0}", diff.ToString())

        output.WriteLine ("norm = {0}", norm)
        output.WriteLine ("diffValue = {0}", diffNorm)
        Assert.True(diffNorm / norm < allowedDiff)


    let verifyVectorEqualityE o m (E r) (E e) = verifyComplexVectorEquality o m r e
    let verifyVectorEqualityH o m (H r) (H e) = verifyComplexVectorEquality o m r e
    let verifyVectorEqualityStokes o m (StokesVector (RealVector4 r)) (StokesVector (RealVector4 e)) = verifyRealVectorEquality o m r e


    // Compares one pair of complex basis (value + vector) for equality.
    let evdDiff (output : ITestOutputHelper) (v0 : Complex) (v1 : Complex) (ComplexVector4 (ComplexVector e0)) (ComplexVector4 (ComplexVector e1)) =
        output.WriteLine ("v0 = {0}", v0)
        output.WriteLine ("v1 = {0}", v1)

        output.WriteLine ("e0 = {0}", e0)
        output.WriteLine ("e1 = {0}", e1)

        let vDiff = (v0 - v1).abs
        let eDiff = (e0 - e1).L2Norm ()
        vDiff + eDiff


    // Compares two pairs of complex basis (value + vector) for equality.
    let eigenBasisEquality (output : ITestOutputHelper) (msg : string) (result : EigenBasis) (expected : EigenBasis) = 
        outputData output msg result expected
        
        let diff00 = evdDiff output result.v0 expected.v0 result.e0 expected.e0
        let diff11 = evdDiff output result.v1 expected.v1 result.e1 expected.e1
        let diff = diff00 + diff11

        let diff01 = evdDiff output result.v0 expected.v1 result.e0 expected.e1
        let diff10 = evdDiff output result.v1 expected.v0 result.e1 expected.e0
        let diffAlt = diff01 + diff10

        let diffMin = min diff diffAlt
        Assert.True(diffMin < allowedDiff)

    let verifyPolarizationEquality (output : ITestOutputHelper) (msg : string) (Polarization (Angle result)) (Polarization (Angle expected)) = 
        outputData output msg result expected
        let diff = abs (result - expected)
        Assert.True(diff < allowedDiff)

    let verifyEllipticityEquality (output : ITestOutputHelper) (msg : string) (Ellipticity result) (Ellipticity expected) = 
        outputData output msg result expected
        let diff = abs (result - expected)
        Assert.True(diff < allowedDiff)
