#!MC 1410
$!OpenLayout  "./solutions.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!VARSET |PRIOR| = 0


#$!FrameControl ActivateByNumber
#  Frame = 1
$!LineMap [1-1701]  Lines{LineThickness = 0.4}
$!LineMap [100,200,300,400,500,600,700,800,900,1000]  Assign{YAxisVar = 2}
$!LineMap [1100,1200,1300,1400,1500,1600,1700,1701]  Assign{YAxisVar = 2}


# ('Quebec convolution routine for dead variable')
#Q$!LOOP 103
#Q$!IF |LOOP| > 1
#Q$!AlterData[14]
#Q  Equation = 'V|LOOP| = 0.37*V|LOOP|(i-8) + 0.32*V|LOOP|(i-4) + 0.31*V|LOOP|(i)'
#Q$!ENDIF
#Q$!ENDLOOP

$!AlterData [1-21]
  Equation = 'V1=V1+43880'

$!ActiveLineMaps -= [1-1701]

# Active cases
$!ActiveLineMaps += [1-100]
$!LineMap [100]   Name = 'Active cases'
$!LineMap [100]  Lines{LineThickness = 0.80}

# Total cases
$!IF |PRIOR| == 1
   $!ActiveLineMaps += [201-300]
   $!LineMap [201-300]  Lines{LineThickness = 0.10}
$!ENDIF
$!ActiveLineMaps += [901-1000]
$!LineMap [999]   Name = 'Total cases'
$!LineMap [999]  Lines{LineThickness = 0.80}
$!LineMap [1000]  Symbols{Size = 0.7 }

# ICU patients
$!IF |PRIOR| == 1
   $!ActiveLineMaps += [601-700]
   $!LineMap [601-700]  Lines{LineThickness = 0.10}
$!ENDIF
$!ActiveLineMaps += [1301-1400]
$!LineMap [1399]  Name = 'ICU patients'
$!LineMap [1399]  Lines{LineThickness = 0.80}
$!LineMap [1400]  Symbols{Size = 0.7 }

# Total dead
$!IF |PRIOR| == 1
   $!ActiveLineMaps += [701-800]
   $!LineMap [701-800]  Lines{LineThickness = 0.10}
$!ENDIF
$!LineMap [1499]  Name = 'Total dead'
$!ActiveLineMaps += [1401-1500]
$!LineMap [1499]  Lines{LineThickness = 0.80}
$!LineMap [1500]  Symbols{Size = 0.7 }


# LEGEND
$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 9.19724}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}

# AXIS
$!XYLineAxis YDetail 1 {CoordScale = Log}
$!XYLineAxis YDetail 1 {RangeMin = 10}
$!XYLineAxis YDetail 1 {RangeMax = 100000000}
$!XYLineAxis XDetail 1 {RangeMin = 43880}
$!XYLineAxis XDetail 1 {RangeMax = 44015}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}


$!RedrawAll 
$!ExportSetup ExportFName = './HDlog.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "HDlog"




###############################
# ICU patients
$!ActiveLineMaps -= [1-1701]

$!IF |PRIOR| == 1
   $!ActiveLineMaps += [601-700]
   $!LineMap [601-700]  Lines{LineThickness = 0.10}
$!ENDIF
$!ActiveLineMaps += [1301-1400]
$!LineMap [1399]  Name = 'ICU patients'

# Total dead
$!IF |PRIOR| == 1
   $!ActiveLineMaps += [701-800]
   $!LineMap [701-800]  Lines{LineThickness = 0.10}
$!ENDIF
$!LineMap [1499]  Name = 'Total dead'
$!ActiveLineMaps += [1401-1500]

$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 9.19724}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}

$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 80000}
$!XYLineAxis XDetail 1 {RangeMin = 43880}
$!XYLineAxis XDetail 1 {RangeMax = 44015}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}
 
$!RedrawAll 
$!ExportSetup ExportFName = './HD.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "HD"


###############################
# R E N S
$!FrameControl ActivateByNumber
  Frame = 1
$!XYLineAxis YDetail 1 {Title{Text = 'Effective R(t)'}}
$!ActiveLineMaps -= [1-1701]

$!LineMap [1600]  Name = 'Prior R(t)'
$!LineMap [1600]  Lines{LineThickness = 0.80}
$!ActiveLineMaps += [1501-1600]

$!LineMap [1701]  Name = 'Posterior R(t)'
$!LineMap [1701]  Lines{LineThickness = 0.80}
$!ActiveLineMaps += [1601-1701]

$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!View Fit
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43880}
$!XYLineAxis XDetail 1 {RangeMax = 44015}
$!XYLineAxis YDetail 1 {AutoGrid = No}
$!XYLineAxis YDetail 1 {GRSpacing = 1}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}


$!GlobalLinePlot Legend{AnchorAlignment = TopRight}
$!GlobalLinePlot Legend{XYPos{X = 98.1606}}
$!GlobalLinePlot Legend{XYPos{Y = 96.5265}}

$!AttachGeom
  AnchorPos
    {
    X = 43800.0
    Y = 1.0
    }
  Color = Red
  LineThickness = 0.2
  RawData
1
2
0 0
480.000 0.0

$!RedrawAll 
$!ExportSetup ExportFName = './RENS.eps'
$!Export 
  ExportRegion = AllFrames
#$!PAUSE "RENS"
