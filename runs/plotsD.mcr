#!MC 1410
$!OpenLayout  "./solutions.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!VARSET |PRIOR| = XXPRIXX


#$!FrameControl ActivateByNumber
#  Frame = 1
$!LineMap [1-1701]  Lines{LineThickness = 0.4}
$!LineMap [100,200,300,400,500,600,700,800,900,1000]  Assign{YAxisVar = 2}
$!LineMap [1100,1200,1300,1400,1500,1600,1700,1701]  Assign{YAxisVar = 2}



$!AlterData [1-21]
  Equation = 'V1=V1+XXXXA'

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

# Hospitalized
$!IF |PRIOR| == 1
   $!ActiveLineMaps += [601-700]
   $!LineMap [601-700]  Lines{LineThickness = 0.10}
$!ENDIF
$!ActiveLineMaps += [1301-1400]
$!LineMap [1399]  Name = 'Hospitalized'
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
$!GlobalLinePlot Legend{XYPos{X = LEGXX}}
$!GlobalLinePlot Legend{XYPos{Y = LEGYY}}

# AXIS
$!XYLineAxis YDetail 1 {CoordScale = Log}
$!XYLineAxis YDetail 1 {RangeMin = 10}
$!XYLineAxis YDetail 1 {RangeMax = YYYYC}
$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
$!XYLineAxis XDetail 1 {RangeMax = XXXXB}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}


$!RedrawAll 
$!ExportSetup ExportFName = './HDlog.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "HDlog"




###############################
# Hospitalized
$!ActiveLineMaps -= [1-1701]

$!IF |PRIOR| == 1
   $!ActiveLineMaps += [601-700]
   $!LineMap [601-700]  Lines{LineThickness = 0.10}
$!ENDIF
$!ActiveLineMaps += [1301-1400]
$!LineMap [1399]  Name = 'Hospitalized'

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
$!XYLineAxis YDetail 1 {RangeMax = YYYYD}
$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
$!XYLineAxis XDetail 1 {RangeMax = XXXXB}
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
$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
$!XYLineAxis XDetail 1 {RangeMax = XXXXB}
$!XYLineAxis YDetail 1 {AutoGrid = No}
$!XYLineAxis YDetail 1 {GRSpacing = 1}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}


$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 70.0}}
$!GlobalLinePlot Legend{XYPos{Y = 93.0}}

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
